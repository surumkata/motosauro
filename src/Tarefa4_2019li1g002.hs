-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g002 where

import Tarefa2_2019li1g002
import Tarefa0_2019li1g002
import LI11920
import LITestes_002

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).

testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = testes4

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.

passo :: Double -- ^ O tempo decorrido.
      -> Mapa    -- ^ O mapa utilizado.
      -> Jogador -- ^ O estado anterior do 'Jogador'.
      -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.

acelera :: Double -- ^ O tempo decorrido.
        -> Mapa    -- ^ O mapa utilizado.
        -> Jogador -- ^ O estado anterior do 'Jogador'.
        -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j = case testaChao j of -- Testa se está no Chão.
                True -> j {velocidadeJogador = vChao}
                False -> case testaAr j of -- Testa se está no Ar.
                         True  -> j {velocidadeJogador = vAr ,estadoJogador = ej {gravidadeJogador = (gravidadeJogador ej) + t}}
                         False -> j -- Quando 'EstadoJogador' = Morto.

              where v         = velocidadeJogador j
                    ej        = estadoJogador j
                    at        = atrito(pisoDaPeca (pecaJog (pistaJogador j) (distanciaJogador j) m)) 
                    vAr       = notNegative ((velocidadeJogador j) - (0.125 * (velocidadeJogador j) * t))
                    vChao     = notNegative(v + (accelMota - at * v) * t)
                    accelMota = if ej == Chao True && v<2 then 1 else 0

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.

move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m j = case testaChao j of
             True  -> moveJogadorChao t m j
             False -> case testaAr j of
                      True  -> moveJogadorAr t m j
                      False -> j {estadoJogador = (alteraTimeout (estadoJogador j) t)}

-- * Funções auxiliares da função 'acelera'.

-- | Diz o 'Piso' de uma 'Peca'.

pisoDaPeca :: Peca -- ^ A 'Peca'.
           -> Piso -- ^ O 'Piso' da 'Peca'.
pisoDaPeca (Recta p _)   = p
pisoDaPeca (Rampa p _ _) = p

-- | Diz o atrito de um 'Piso'.

atrito :: Piso -- ^ O 'Piso'.
       -> Double -- ^ O valor do atrito do 'Piso'.
atrito Terra = 0.25
atrito Relva = 0.75
atrito Lama  = 1.50
atrito Boost = (-0.50)
atrito Cola  = 3.00

-- * Funções auxiliares da função 'move'.

-- | Altera o timeout do 'Jogador' em função do tempo.

alteraTimeout :: EstadoJogador -- ^ O 'EstadoJogador' no estado Morto
              -> Double -- ^ O tempo a descontar.
              -> EstadoJogador -- ^ O 'EstadoJogador' resultante de descontar o tempo ao timeout.
alteraTimeout ej t = case descTimeout > 0 of    
                     True  -> ej {timeoutJogador = descTimeout}
                     False -> Chao False

                     where descTimeout = timeoutJogador ej - t

-- | Altera a posição de 'Jogador' quando ele está no chão, durante um determinado período de tempo.

moveJogadorChao :: Double -- ^ O tempo decorrido.
                -> Mapa -- ^ O mapa utilizado. 
                -> Jogador -- ^ O estado anterior do 'Jogador'.
                -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
moveJogadorChao t m j = case distPerc >= xp of
                        True  -> case inclpp >= inclp of
                                 True  -> j {distanciaJogador = xp}
                                 False -> (mudaParaAr yp (toGraus inclp) j) {distanciaJogador = xp}  
                        False -> j {distanciaJogador = distPerc} 

                      where x        = distanciaJogador j 
                            v        = velocidadeJogador j 
                            xp       = next x 
                            yp       = calcAlt xp (pecaJog npi (x+1) m)
                            npi      = pistaJogador j 
                            inclp    = calcIncl (pecaJog npi x m) 
                            inclpp   = calcIncl (pecaJog npi (x+1) m) 
                            distPerc = x + cos(inclp) * v * t

-- | Altera a altura de 'Jogador' quando ele está no ar, durante um determinado período de tempo.

moveJogadorAr :: Double -- ^ O tempo decorrido.
              -> Mapa -- ^ O mapa utilizado. 
              -> Jogador -- ^ O estado anterior do 'Jogador'.
              -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
moveJogadorAr t m j = case ((intersetam retavt retapeca) && (y /= altpeca)) of 
                      False -> case distPerc >= next x of
                               True  -> j {distanciaJogador = next x, estadoJogador = ej {alturaJogador = yPercNext}}
                               False -> j {distanciaJogador = distPerc, estadoJogador = ej {alturaJogador = yPerc}}
                      True  -> case testaIncl (pecaJog npi x m) (toGraus incl) of
                               True  -> (morreJog j) {distanciaJogador = xn}
                               False -> j {distanciaJogador = xn, velocidadeJogador = v * cos(abs(abs(incl)-abs(inclpeca))), estadoJogador = Chao False}
                    
                    where x         = distanciaJogador j
                          v         = velocidadeJogador j
                          y         = alturaJogador ej
                          g         = gravidadeJogador ej
                          ej        = estadoJogador j
                          tn        = (next x - x)/((cos incl)*v)
                          xn        = converteX(intersecao retavt retapeca)
                          npi       = pistaJogador j
                          incl      = toRadianos (inclinacaoJogador ej)
                          peca      = pecaJog npi x m
                          yPerc     = y + sin(incl)*v*t - g*t
                          retavt    = ((Cartesiano x y),(Cartesiano distPerc yPerc))
                          retapeca  = retaPeca npi x m
                          inclpeca  = calcIncl peca
                          distPerc  = x + cos(incl) * v * t
                          yPercNext = y + sin(incl)*v*tn - g*tn
                          altpeca   = calcAlt x (pecaJog npi x m)

-- | Calcula a reta da 'Peca' em que o 'Jogador' está.

retaPeca :: Int -- ^ A 'Pista' do 'Jogador'.
         -> Double -- ^ A posiçao do 'Jogador'.
         -> Mapa -- ^ O 'Mapa'.
         -> Reta -- ^ A Reta calculada.
retaPeca npi x m = ((Cartesiano x y),(Cartesiano nx ny))
             
           where y  = calcAlt x (pecaJog npi x m)
                 ny = calcAlt nx (pecaJog npi x m)
                 nx = ((next x)-0.01)

-- | Testa se difrença entre a inclinação do 'Jogador' e da 'Peca' é maior ou igual a 45º.

testaIncl :: Peca -- ^ A 'Peca'
          -> Double -- ^ A inclinação do 'Jogador'
          -> Bool -- ^ True se a difrença entre a inclinação do 'Jogador' e da 'Peca' foi maior ou igual a 45 | False se não.
testaIncl peca incl = abs(incl-(calcIncl peca)) >= 45


-- | Arredonda um número para cima ou se ele for inteiro acrescenta um.

next :: Double -- ^ Número.
     -> Double -- ^ Número arredondado.
next x = if x == xp then x+1 else xp
         
         where xp = fromIntegral(ceiling x)

-- | Recebe uma velocidade e se esta for negativa dá zero, se não retorna a mesma velocidade.

notNegative :: Double -- ^ A velocidade.
            -> Double -- ^ A velocidade retornada.
notNegative v = if v<0 then 0 else v

-- | Converte um ponto (Cartesiano x y) em x

converteX :: Ponto -- ^ O 'Ponto' em (Cartesiano x y)
          -> Double -- ^ O x.
converteX (Cartesiano x _) = x