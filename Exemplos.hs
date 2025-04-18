--nao enviar pro trabalho final, isso é só pra teste
module Exemplos (
        listaTarefas
)where
import Tipos
import Data.Time.Calendar (fromGregorian)
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
