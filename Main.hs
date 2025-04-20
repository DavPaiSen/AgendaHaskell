module Main (main) where
import Tipos
import Funcoes
import Persistencia
import System.IO (hFlush, stdout)
import Data.Time (getCurrentTime, utctDay)
import System.IO.Error (tryIOError)

main :: IO ()
main = do
    -- Tenta carregar tarefas do arquivo com tratamento de erro
    tentativa <- tryIOError (carregarDeArquivo "tarefas.txt")
    tarefasIniciais <- case tentativa of
        Left _ -> do
            putStrLn "Arquivo 'tarefas.txt' não encontrado. Iniciando com lista vazia."
            return []
        Right t -> return t
    
    putStrLn "Bem-vindo ao Gerenciador de Tarefas!"
    if null tarefasIniciais
        then putStrLn "Nenhuma tarefa encontrada."
        else putStrLn $ "Tarefas carregadas: " ++ show (length tarefasIniciais) ++ " encontradas."

    -- Inicia o loop principal
    mainLoop tarefasIniciais

-- Exibe o menu de opções
menu :: IO ()
menu = do
    putStrLn "\nMenu de Tarefas"
    putStrLn "1. Carregar arquivo"
    putStrLn "2. Salvar tarefas"
    putStrLn "3. Adicionar tarefa"
    putStrLn "4. Remover tarefa"
    putStrLn "5. Marcar tarefa como concluída"
    putStrLn "6. Listar todas as tarefas"
    putStrLn "7. Listar por categoria"
    putStrLn "8. Listar por prioridade"
    putStrLn "9. Ordenar por prioridade"
    putStrLn "10. Filtrar por status"
    putStrLn "11. Buscar por palavra-chave"
    putStrLn "12. Verificar tarefas atrasadas"
    putStrLn "13. Calcular dias restantes"
    putStrLn "14. Filtrar por tag"
    putStrLn "15. Gerar nuvem de tags"
    putStrLn "16. Criar relatório"
    putStrLn "17. Sair"
    putStr "Escolha uma opção: "
    hFlush stdout -- força a exibição imediata, evitando o buffer

-- Loop principal incluindo todas as funcionalidades
mainLoop :: [Tarefa] -> IO ()
mainLoop tarefas = do
    menu
    opcao <- getLine
    case opcao of
        "1" -> do
            putStr "Digite o nome do arquivo para carregar: "
            hFlush stdout
            arquivo <- getLine
            novasTarefas <- carregarDeArquivo arquivo
            putStrLn $ "Tarefas carregadas de " ++ arquivo
            mainLoop novasTarefas
            
        "2" -> do
            putStr "Digite o nome do arquivo para salvar: "
            hFlush stdout
            arquivo <- getLine
            salvarEmArquivo arquivo tarefas
            putStrLn $ "Tarefas salvas em " ++ arquivo
            mainLoop tarefas
            
        "3" -> do
            let novaTarefa = criarTarefa (proximoId tarefas)
            case adicionarTarefa novaTarefa tarefas of
                Left msg -> do
                    putStrLn msg
                    mainLoop tarefas
                Right novas -> do
                    putStrLn "Tarefa adicionada com sucesso!"
                    mainLoop novas
            
        "4" -> do
            putStr "Digite o ID da tarefa a ser removida: "
            hFlush stdout
            idStr <- getLine
            let id = read idStr :: Int
            case removerTarefa id tarefas of
                Left False -> do
                    putStrLn "Tarefa não encontrada!"
                    mainLoop tarefas
                Right novasTarefas -> do
                    putStrLn "Tarefa removida com sucesso!"
                    mainLoop novasTarefas
            
        "5" -> do
            putStr "Digite o ID da tarefa a marcar como concluída: "
            hFlush stdout
            idStr <- getLine
            let id = read idStr :: Int
            case marcarConcluida id tarefas of
                Left False -> do
                    putStrLn "Tarefa não encontrada!"
                    mainLoop tarefas
                Right novasTarefas -> do
                    putStrLn "Tarefa marcada como concluída!"
                    mainLoop novasTarefas
            
        "6" -> do
            listarTarefas tarefas
            mainLoop tarefas
            
        "7" -> do
            putStr "Digite a categoria (Trabalho/Estudos/Pessoal/Outro): "
            hFlush stdout
            categoriaStr <- getLine
            let categoria = lerCategoria categoriaStr
            let tarefasFiltradas = listarPorCategoria categoria tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas
            
        "8" -> do
            putStr "Digite a prioridade (Alta/Media/Baixa): "
            hFlush stdout
            prioridadeStr <- getLine
            let prioridade = lerPrioridade prioridadeStr
            let tarefasFiltradas = listarPorPrioridade prioridade tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas
            
        "9" -> do
            let tarefasOrdenadas = ordenarPorPrioridade tarefas
            putStrLn "Tarefas ordenadas por prioridade (Alta -> Baixa):"
            listarTarefas tarefasOrdenadas
            mainLoop tarefas
            
        "10" -> do
            putStr "Digite o status (Pendente/Concluida): "
            hFlush stdout
            statusStr <- getLine
            let status = lerStatus statusStr
            let tarefasFiltradas = filtrarPorStatus status tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas
            
        "11" -> do
            putStr "Digite a palavra-chave para buscar: "
            hFlush stdout
            keyword <- getLine
            let tarefasEncontradas = buscarPorPalavraChave keyword tarefas
            listarTarefas tarefasEncontradas
            mainLoop tarefas
            
        "12" -> do
            dataAtual <- getCurrentTime
            let hoje = utctDay dataAtual
            let tarefasAtrasadas = verificarAtrasos tarefas hoje
            putStrLn "Tarefas atrasadas:"
            listarTarefas tarefasAtrasadas
            mainLoop tarefas
            
        "13" -> do
            putStr "Digite o ID da tarefa para calcular dias restantes: "
            hFlush stdout
            idStr <- getLine
            let id = read idStr :: Int
            dataAtual <- utctDay <$> getCurrentTime
            case encontrarTarefa id tarefas of
                Nothing -> putStrLn "Tarefa não encontrada!"
                Just t -> case calcularDiasRestantes t dataAtual of
                    Nothing -> putStrLn "Tarefa sem prazo definido"
                    Just dias ->
                        putStrLn $
                            if dias < 0
                                then "Atrasada em " ++ show (abs dias) ++ " dias"
                                else "Faltam " ++ show dias ++ " dias"
            mainLoop tarefas
            
        "14" -> do
            putStr "Digite a tag para filtrar: "
            hFlush stdout
            tag <- getLine
            let tarefasFiltradas = filtrarPorTag tag tarefas
            listarTarefas tarefasFiltradas
            mainLoop tarefas
            
        "15" -> do
            let nuvem = nuvemDeTags tarefas
            putStrLn "Nuvem de Tags (tag: ocorrências):"
            mapM_ (\(tag, count) -> putStrLn $ "  " ++ tag ++ ": " ++ show count) nuvem
            mainLoop tarefas
            
        "16" -> do
            criarRelatorio tarefas
            mainLoop tarefas
            
        "17" -> putStrLn "Até logo! :)"
        
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            mainLoop tarefas
