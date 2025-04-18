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
import Exemplos
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

recursaoGenerica :: (Tarefa -> Bool) -> [Tarefa] -> [Tarefa]
recursaoGenerica _ [] = []
recursaoGenerica condicao (x:xs)
    |condicao x = x : recursaoGenerica condicao xs
    |otherwise = recursaoGenerica condicao xs

listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria c l = recursaoGenerica (\t -> categoria t == c) l

listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade p l = recursaoGenerica (\t -> prioridade t == p) l

ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade l = concat [listarPorPrioridade Alta l, listarPorPrioridade Media l, listarPorPrioridade Baixa l]

filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus s l = recursaoGenerica (\t -> status t == s) l

buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave p l = recursaoGenerica (\t -> elem p (tags t)) l
