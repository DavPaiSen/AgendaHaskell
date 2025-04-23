module Testes where

import Tipos
import Funcoes
import Data.Time.Calendar (Day, fromGregorian) -- Especifica um prazo (prazo :: Maybe Day) de forma clara e tipada.

-- Exemplos de tarefas
tarefas :: [Tarefa]
tarefas =
  [ Tarefa 1 "Estudar Haskell" Pendente Alta Estudos
      (Just (fromGregorian 2025 4 11)) ["ufu", "haskell"]
  , Tarefa 2 "Fazer compras" Concluida Media Pessoal
      (Just (fromGregorian 2025 4 11)) ["casa"]
  , Tarefa 3 "Finalizar projeto" Pendente Alta Trabalho
      (Just (fromGregorian 2025 4 11)) ["dev", "haskell"]
  , Tarefa 4 "Ler artigo" Pendente Media Estudos
      (Just (fromGregorian 2025 4 15)) ["leitura"]
  , Tarefa 5 "Consulta médica" Concluida Baixa Pessoal
      (Just (fromGregorian 2025 4 10)) ["saude"]
  ]
  
executarTestes :: IO ()
executarTestes = do
  putStrLn "\nIniciando testes...\n"

  putStrLn "Teste 1: adicionarTarefa"
  let nova = Tarefa 6 "Nova tarefa" Pendente Baixa Outro Nothing []
  let listaNova = adicionarTarefa nova tarefas
  putStrLn $ "Esperado: 6 tarefas | Obtido: " ++ show (length listaNova)

  putStrLn "\nTeste 2: removerTarefa (ID 2)"
  case removerTarefa 2 tarefas of
    Right lista -> putStrLn $ "Esperado: 4 tarefas | Obtido: " ++ show (length lista)
    Left _      -> putStrLn "Erro: tarefa não encontrada"

  putStrLn "\nTeste 3: marcarConcluida (ID 1)"
  case marcarConcluida 1 tarefas of
    Right lista ->
      let status1 = status (head lista)
      in putStrLn $ "Esperado: Concluida | Obtido: " ++ show status1
    Left _ -> putStrLn "Erro: tarefa não encontrada"

  putStrLn "\nTeste 4: listarPorCategoria (Estudos)"
  let estudando = listarPorCategoria Estudos tarefas
  putStrLn $ "Esperado: 2 tarefas | Obtido: " ++ show (length estudando)

  putStrLn "\nTeste 5: listarPorPrioridade (Alta)"
  let altas = listarPorPrioridade Alta tarefas
  putStrLn $ "Esperado: 2 tarefas | Obtido: " ++ show (length altas)

  putStrLn "\nTeste 6: buscarPorPalavraChave \"haskell\""
  let haskellTarefas = buscarPorPalavraChave "haskell" tarefas
  putStrLn $ "Esperado: 2 tarefas | Obtido: " ++ show (length haskellTarefas)

  putStrLn "\nTeste 7: nuvemDeTags"
  let nuvem = nuvemDeTags tarefas
  putStrLn $ "Esperado conter (\"haskell\",2) | Obtido: " ++ show nuvem

  putStrLn "\nTeste 8: calcularDiasRestantes"
  let hoje = fromGregorian 2025 4 10
  let dias = map (\t -> (idTarefa t, calcularDiasRestantes t hoje)) tarefas
  mapM_ (\(idT, resultado) ->
           putStrLn $ "Tarefa " ++ show idT ++ ": " ++ maybe "Sem prazo" show resultado ++ " dias restantes")
        dias

  putStrLn "\nTeste 9: criarRelatorio"
  putStrLn "Executando relatório:"
  criarRelatorio tarefas

  putStrLn "\nFim dos testes.\n"
