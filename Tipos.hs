module Tipos (
        Tarefa(..),
        Status(..),
        Prioridade(..),
        Categoria(..),
        listaTarefas,
        Day
)where

import Data.Time.Calendar (Day, fromGregorian)
data Status = Pendente | Concluida deriving (Show, Eq)
data Prioridade = Baixa | Media | Alta deriving (Show, Eq, Ord)
data Categoria = Trabalho | Estudos | Pessoal | Outro deriving (Show, Eq)
data Tarefa = Tarefa
     { idTarefa :: Int
     , descricao :: String
     , status :: Status
     , prioridade :: Prioridade
     , categoria :: Categoria
     , prazo :: Maybe Day -- Usando Data.Time.Calendar
     , tags :: [String]
     } deriving (Show, Eq)

listaTarefas :: [Tarefa]
listaTarefas =
    [ Tarefa
        { idTarefa = 1
        , descricao = "Estudar Haskell"
        , status = Pendente
        , prioridade = Alta
        , categoria = Estudos
        , prazo = Just (fromGregorian 2023 12 15)
        , tags = ["programação", "importante"]
        }
    , Tarefa
        { idTarefa = 2
        , descricao = "Fazer relatório mensal"
        , status = Concluida
        , prioridade = Media
        , categoria = Trabalho
        , prazo = Just (fromGregorian 2023 11 30)
        , tags = ["relatório", "chefe"]
        }
    , Tarefa
        { idTarefa = 3
        , descricao = "Comprar presentes de Natal"
        , status = Pendente
        , prioridade = Baixa
        , categoria = Pessoal
        , prazo = Just (fromGregorian 2023 12 24)
        , tags = ["compras", "familia"]
        }
    , Tarefa
        { idTarefa = 4
        , descricao = "Reunião com equipe"
        , status = Pendente
        , prioridade = Alta
        , categoria = Trabalho
        , prazo = Nothing  -- Sem prazo definido
        , tags = ["reunião"]
        }
    , Tarefa
        { idTarefa = 5
        , descricao = "Ler livro sobre algoritmos"
        , status = Pendente
        , prioridade = Media
        , categoria = Estudos
        , prazo = Just (fromGregorian 2024 01 31)
        , tags = ["leitura", "aprendizado"]
        }
    ]
