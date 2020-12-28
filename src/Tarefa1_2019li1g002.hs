-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g002 where

import LI11920
import System.Random
import LITestes_002

-- * Testes.

-- | Testes da Tarefa 1 (Cada teste apresenta 3 inteiros que correspondem, respetivamente, ao número de pistas, ao comprimento das pistas e à semente)

testesT1 :: [(Int,Int,Int)]
testesT1 = testes1

-- * Função pré-definida da Tarefa 1.
-- | Gera uma lista de número semi-aleatória.

geraAleatorios :: Int -- ^ Comprimento da lista.
               -> Int -- ^ Semente.
               -> [Int] -- ^ Lista de números semi-aleatórios.
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções auxiliares da Tarefa 1.

-- | Descodifica o gama do piso.

descGama :: Int -- ^ Gama do piso.
         -> Int -- ^ Gama equivalente ao piso anterior.
         -> Int -- ^ Gama do piso descodificado.
descGama g1 pa | g1 <= 5 = g1
               | g1 >= 6 = pa

-- | Gera o piso para as peças.

geraPiso :: Int -- ^ Gama do piso descodificado.
         -> Piso -- ^ 'Piso' gerado.
geraPiso p | p==0 || p==1 = Terra
           | p==2 || p==3 = Relva
           | p==4         = Lama
           | otherwise    = Boost

-- | Gera a altura para as peças.

geraAlt :: Int -- ^ Gama do tipo.
        -> Int -- ^ Altura da peça anterior.
        -> Int -- ^ Altura gerada.
geraAlt g2 ya | g2de0a1 && ynS > 3 = ya
              | g2de0a1            = ynS
              | g2de2a5 && ynD < 0 = 0
              | g2de2a5            = ynD
              | otherwise          = ya
              where g2de2a5        = g2 <= 5 && g2 >= 2
                    g2de0a1        = g2 == 0 || g2 == 1
                    ynS            = ya + 1
                    ynD            = ya - 1
-- | Gera a Peça.

geraPeca :: Int -- ^ Gama do piso.
         -> Int -- ^ Gama do tipo.
         -> (Int,Int) -- ^ (Gama equivalente ao piso anterior, Altura da peça anterior).
         -> Peca -- ^ A 'Peca' gerada.
geraPeca g1 g2 (pa,ya) | g2==0 || g2==1 = Rampa p ya a
                       | g2>=6 || a==ya = Recta p a
                       | g2<=5          = Rampa p ya a
                       where p          = geraPiso gamaDoPiso
                             a          = geraAlt g2 ya
                             gamaDoPiso = descGama g1 pa
-- | Gera uma Pista.

geraPista :: [Int] -- ^ Semente aleatorio com os gamas.
          -> (Int,Int) -- ^ (gama equivalente ao piso anterior, altura da peça anterior).
          -> Pista -- ^ A 'Pista' gerada.
geraPista [] _              = []
geraPista (g1:g2:t) (pa,ya) = let peca   = geraPeca g1 g2 (pa,ya)
                                  outras = geraPista t (p,a)
                                  p      = descGama g1 pa
                                  a      = geraAlt g2 ya
                              in peca:outras

addRectaTerra :: Pista -> Pista
addRectaTerra [] = [Recta Terra 0]
addRectaTerra pista = (Recta Terra 0):pista


-- * Função principal da Tarefa 1.
-- | Gera um Mapa.

gera :: Int -- ^ Número de pistas.
     -> Int -- ^ Comprimento das pistas.
     -> Int -- ^ Semente.
     -> Mapa -- ^ 'Mapa' gerado.
gera npi c s | npi <= 0 = []
gera npi c s | npi > 0  = let x    = [(Recta Terra 0)]++(geraPista seed (0,0))
                              y    = gera (npi-1) c s
                              seed    = geraAleatorios n s
                              n    = 2*(c-1)
                           in y++[x]
                            