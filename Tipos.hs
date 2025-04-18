module Tipos (
        Tarefa(..),
        Status(..),
        Prioridade(..),
        Categoria(..),
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
     , prazo :: Maybe Day 
     , tags :: [String]
     } deriving (Show, Eq)
