-- | Este módulo define os tipos de dados comuns a todos os alunos, tal como descrito e utilizado no enunciado do trabalho prático. 
module LI11920 where

-- * Tipos de dados auxiliares.

-- | Mapa é uma lista de 'Pista' .

type Mapa = [Pista]

-- | Pista é uma lista de 'Peca' .

type Pista = [Peca]

-- | Uma 'Peca' pode ser uma Recta (com uma altura em Int) ou uma Rampa (com uma altura inicial e final em Int), que tem um determinado Piso.

data Peca
    = Recta Piso Int
    | Rampa Piso Int Int
  deriving (Read,Show,Eq)

-- | Um 'Piso' pode ser Terra, Relva, Lama, Boost ou Cola.

data Piso = Terra | Relva | Lama | Boost | Cola
  deriving (Read,Show,Eq)

-- | Um 'Jogador' é definido pela pista onde se encontra, pela distância que possui, velocidade que possui, quantas colas de munição possui e o seu estado.

data Jogador
    = Jogador { pistaJogador :: Int, distanciaJogador :: Double, velocidadeJogador :: Double, colaJogador :: Int, estadoJogador :: EstadoJogador }
  deriving (Read,Show,Eq)

-- | Um estado do jogador pode ser 'Chao' (onde pode estar a acelerar ou não), 'Morto' (com um timeout), ou no 'Ar' (onde tem uma altura, uma inclinação e uma gravidade)

data EstadoJogador
    = Chao { aceleraJogador :: Bool }
    | Morto { timeoutJogador :: Double }
    | Ar { alturaJogador :: Double, inclinacaoJogador :: Double, gravidadeJogador :: Double }
  deriving (Read,Show,Eq)

-- | Estado do jogo que é constituído por um mapa e uma lista de jogadores

data Estado = Estado
    { mapaEstado      :: Mapa
    , jogadoresEstado :: [Jogador] -- ^ lista de jogadores com identificador igual ao índice na lista
    }
  deriving (Read,Show,Eq)

-- | Uma direção que pode ser Cima, Direita, Baixo e Esquerda
data Direcao
    = C -- ^ Cima
    | D -- ^ Direita
    | B -- ^ Baixo
    | E -- ^ Esquerda
  deriving (Read,Show,Eq,Enum,Bounded)

-- | Uma Jogada de um Jogador pode ser movimenta (cima, baixo, direita, esquerda), acelera, desacelera ou dispara.

data Jogada
    = Movimenta Direcao
    | Acelera
    | Desacelera
    | Dispara -- ^ cola   
  deriving (Read,Show,Eq)

-- | Intrucoes é uma lista de Intrucao
    
type Instrucoes = [Instrucao]

-- | Tipos de intrução que um bulldozer recebe para criar um mapa.

data Instrucao
    = Anda [Int] Piso
    | Sobe [Int] Piso Int
    | Desce [Int] Piso Int
    | Teleporta [Int] Int
    | Repete Int Instrucoes
  deriving (Read,Show,Eq)

-- | Recebe Instrucoes e calcula o seu tamanho.

tamanhoInstrucoes :: Instrucoes -> Int
tamanhoInstrucoes is = sum (map tamanhoInstrucao is)

-- | Recebe uma Intrucao e calcula o seu tamanho.

tamanhoInstrucao :: Instrucao -> Int
tamanhoInstrucao (Repete _ is) = succ $ tamanhoInstrucoes is
tamanhoInstrucao _ = 1
