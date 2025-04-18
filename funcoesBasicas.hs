import Tipos
--como no pdf diz que o identificador e unico, nao vou me preocupar em ter
--dois IDs iguais

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

