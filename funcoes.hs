--como no pdf diz que o identificador e unico, nao vou me preocupar em ter
--dois IDs iguais
module Funcoes (
        adicionarTarefa,
        removerTarefa,
        marcarConcluida,
        listarPorCategoria,
        listarPorPrioridade,
        ordenarPorPrioridade,
        filtrarPorStatus,
        buscarPorPalavraChave
)where
import Tipos

--funcoes basicas
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa tarefa listaTarefas = tarefa : listaTarefas

removerTarefa :: Int -> [Tarefa] -> Either Bool [Tarefa]
removerTarefa _ [] = Left False
removerTarefa n (x:xs)
    |idTarefa x == n = Right xs
    |otherwise =
        case removerTarefa n xs of
            Left False -> Left False
            Right r -> Right (x : r)

marcarConcluida :: Int -> [Tarefa] -> Either Bool [Tarefa]
marcarConcluida _ [] = Left False
marcarConcluida n (x:xs)
    |idTarefa x == n = Right (x {status = Concluida} : xs)
    |otherwise =
        case marcarConcluida n xs of
            Left False -> Left False
            Right l -> Right (x : l)

--funcoes avancadas
listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria _ [] = []
listarPorCategoria c (x:xs)
    |categoria x == c = x : listarPorCategoria c xs
    |otherwise = listarPorCategoria c xs

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade _ [] = []
listarPorPrioridade p (x:xs)
    |prioridade x == p = x : listarPorPrioridade p xs
    |otherwise = listarPorPrioridade p xs

ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade l = concat [listarPorPrioridade Alta l, listarPorPrioridade Media l, listarPorPrioridade Baixa l]

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus _ [] = []
filtrarPorStatus s (x:xs)
    |status x == s = x : filtrarPorStatus s xs
    |otherwise = filtrarPorStatus s xs

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave _ [] = []
buscarPorPalavraChave p (x:xs)
    |elem p (tags x) = x : buscarPorPalavraChave p xs
    |otherwise = buscarPorPalavraChave p xs

