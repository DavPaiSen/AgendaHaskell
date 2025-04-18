module FuncoesAvancadas (
        listarPorCategoria,
        listarPorPrioridade,
        ordenarPorPrioridade,
        filtrarPorStatus,
        buscarPorPalavraChave
)where
import Tipos

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
ordenarPorPrioridade l = (listarPorPrioridade Alta l) ++ (listarPorPrioridade Media l) ++ (listarPorPrioridade Baixa l)

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
