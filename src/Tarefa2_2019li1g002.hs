-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g002 where

import LI11920
import LITestes_002
import Tarefa0_2019li1g002
-- * Testes.

-- | Testes da Tarefa 2 (Cada teste apresenta um inteiro que identifica o 'Jogador', uma 'Jogada' para se efetuar ao 'Jogador' e um 'Estado' onde se encontra o 'Mapa' e a lista de 'Jogador')

testesT2 :: [(Int,Jogada,Estado)]
testesT2 = testes2++testes2'

-- * Função principal da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada id (Movimenta dir) e | dir == B || dir == C = Estado m jsnBC
                            | dir == E || dir == D = Estado m jsnED
                            where jsnBC = movJogChao id js dir m
                           		  jsnED = movJogAr id js dir
                                  e     = (Estado m js)

jogada id Acelera (Estado m js)                                     = Estado m (acelera' id js)
jogada id Desacelera (Estado m js)                                  = Estado m (desacelera id js)
jogada id Dispara (Estado m js) | verMunicoes && verChao && verPeca = Estado mn jsn
                                | otherwise                         = Estado m js
                                where j                             = js!!id
                                      verMunicoes                   = testaMunicoes j
                                      verChao                       = testaChao j
                                      verPeca                       = testaPeca j
                                      mn                            = disparaNoMapa npi x m
                                      jsn                           = alteraMunicoes id js
                                      npi                           = pistaJogador j
                                      x                             = distanciaJogador j

-- * Funções auxiliares da 'Jogada' Movimenta (B ou C).

-- | Aplica a 'Jogada' Movimenta (B ou C) ao 'Jogador' identificado (id).

movJogChao :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
           -> [Jogador] -- ^ A lista 'Jogador' anterior.
           -> Direcao -- ^ A 'Direcao' (B ou C) a efetuar.
           -> Mapa -- ^ O 'Mapa' onde o Jogador está.
           -> [Jogador] -- ^ A lista 'Jogador' resultante após o jogador efetuar o Movimento 'Direcao'.
movJogChao id js dir m | ver && verDifAlt    = alteraJogPista id js dir
                       | ver && (dif < 0.2)  = alteraMorte id js
                       | ver && (dif > -0.2) = alteraCai id a incl (alteraJogPista id js dir)
                       | otherwise                           = js
                       where j                               = js!!id 
                             x                               = distanciaJogador j
                             npi                             = pistaJogador j
                             verChao                         = testaChao j
                             verPista                        = testaPista j dir (length m)
                             ver                             = verChao && verPista
                             verDifAlt                       = abs(dif) <= 0.2
                             dif                             = calcDif x pecas
                             pecas                           = escolhePecas npi x m dir
                             peca                            = pecaJog npi x m 
                             incl                            = toGraus(calcIncl peca)
                             a                               = calcAlt x peca

-- | Muda a 'Pista' do 'Jogador' Identificado (id) consoante a 'Direcao' (Cima (-1), Baixo (+1)).

alteraJogPista :: Int -- ^ O identificador do 'Jogador'.
               -> [Jogador] -- ^ A lista 'Jogador' anterior.
               -> Direcao -- ^ A 'Direcao' (B ou C) a aplicar.
               -> [Jogador] -- ^ A lista 'Jogador' resultante após aumentar/diminuir a 'Pista' do 'Jogador'.
alteraJogPista 0 (j:t) B = j {pistaJogador = (pistaJogador j)+1} : t  
alteraJogPista 0 (j:t) C = j {pistaJogador = (pistaJogador j)-1} : t
alteraJogPista id (h:t) dir                = h : alteraJogPista (id-1) t dir  

-- | Altera o estado do 'Jogador' Identificado (id) para Morto com timeout de 1.0.

alteraMorte :: Int -- ^ O identificador do 'Jogador'.
            -> [Jogador] -- ^ A lista 'Jogador' anterior.
            -> [Jogador] -- ^ A lista 'Jogador' resultante após alterar o estado do 'Jogador' para Morto.
alteraMorte 0 (h:t)  = (morreJog h) : t
alteraMorte id (h:t) = h : alteraMorte (id-1) t

-- | Altera o estado do 'Jogador' para Morto com timeout de 1.0.

morreJog :: Jogador -> Jogador
morreJog j = j {velocidadeJogador = 0, estadoJogador = Morto 1.0}

-- | Altera o estado do 'Jogador' Identificado (id) para Ar com a altura anterior a que estava e a inclinação da 'Peca' em que estava.

alteraCai :: Int -- ^ O identificador do 'Jogador'.
          -> Double -- ^ Altura da 'Peca' em que o 'Jogador' se encontrava.
          -> Double -- ^ Inclinação da 'Peca' em que o 'Jogador' se encontrava.
          -> [Jogador] -- ^ A lista 'Jogador' anterior.
          -> [Jogador] -- ^ A lista 'Jogador' resultante após deixar o 'Jogador' a cair.
alteraCai 0 y incl (h:t)  = (mudaParaAr y incl h) : t
alteraCai id y incl (h:t) = h : alteraCai (id-1) y incl t

-- | Altera o estado do 'Jogador' para Ar com a altura anterior a que estava e a inclinação da 'Peca' em que estava.

mudaParaAr :: Double -> Double -> Jogador -> Jogador
mudaParaAr y incl j = j {estadoJogador = Ar y incl 0}

-- * Funções auxiliares da 'Jogada' Movimenta (D ou E).

-- | Aplica a 'Jogada' Movimenta (D ou E) ao 'Jogador' identificado (id).

movJogAr :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
         -> [Jogador] -- ^ A lista 'Jogador' anterior.
         -> Direcao -- ^ A 'Direcao' (D ou E) a efetuar.
         -> [Jogador] -- ^ A lista 'Jogador' resultante após o jogador efetuar o Movimento 'Direcao'.
movJogAr id js dir | verAr            = jsn
                   | otherwise        = js
                   where j            = js!!id
                         verAr        = testaAr j
                         jsn          = alteraJogIncl id js dir

-- | Muda a Inclinação do 'Jogador' Identificado (id) consoante a 'Direcao' (Esquerda -> +15 graus; Direita -> -15 graus).

alteraJogIncl :: Int -- ^ O identificador do 'Jogador'.
              -> [Jogador] -- ^ A lista 'Jogador' anterior.
              -> Direcao -- ^ A 'Direcao' (D ou E) a aplicar.
              -> [Jogador] -- ^ A lista 'Jogador' resultante após aumentar/diminuir a inclinação do 'Jogador'.
alteraJogIncl 0 (j:t) dir  = (j{estadoJogador = mudaIncl (estadoJogador j) dir}):t
alteraJogIncl id (h:t) dir = h:alteraJogIncl (id-1) t dir  


-- | Muda a Inclinação do 'Jogador'.

mudaIncl :: EstadoJogador -- ^ O 'EstadoJogador' do 'Jogador'.
         -> Direcao -- ^ A 'Direcao' (E ou D) a ser executada.
         -> EstadoJogador -- ^ O 'EstadoJogador' resultante após a 'Direcao' ser executada.
mudaIncl ej dir = case dir of
                  E -> case verifinclE of
                       True -> ej {inclinacaoJogador = incl+15}
                       False -> ej {inclinacaoJogador = 90}
                  D -> case verifinclD of
                       True -> ej {inclinacaoJogador = incl-15}
                       False -> ej {inclinacaoJogador = (-90)}
              
              where incl = toGraus(atan(tan(toRadianos(inclinacaoJogador ej))))
                    verifinclE = incl <= 75 && incl >= (-90)
                    verifinclD = incl <= 90 && incl >= (-75)


-- * Funções auxiliares da 'Jogada' Acelera.

-- | Aplica a 'Jogada' Acelera ao 'Jogador' identificado (id)

acelera' :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
         -> [Jogador] -- ^ A lista 'Jogador' anterior.
         -> [Jogador] -- ^ A lista 'Jogador' resultante após aplicar a 'Jogada' Acelera.
acelera' id js | verChao     = jsn
               | otherwise   = js
               where j       = js!!id
                     verChao = testaChao j
                     jsn     = alteraAcelera id js

-- | Muda o estado do 'Jogador' Identificado (id) para True (Acelera).

alteraAcelera :: Int -- ^ O identificador do 'Jogador'.
              -> [Jogador] -- ^ A lista 'Jogador' anterior.
              -> [Jogador] -- ^ A lista 'Jogador' resultante após colocar a aceleração do 'Jogador' em True.
alteraAcelera 0 (h:t)  = (mudaAcelera h) : t
alteraAcelera id (h:t) = h:(alteraAcelera (id-1) t)

-- | Muda o estado de aceleração do 'Jogador' para True (Acelera).

mudaAcelera :: Jogador -> Jogador
mudaAcelera j = j {estadoJogador = Chao True}

-- * Funções auxiliares da 'Jogada' Desacelera.

-- | Aplica a 'Jogada' Desacelera ao 'Jogador' identificado (id).

desacelera :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
           -> [Jogador] -- ^ A lista 'Jogador' anterior.
           -> [Jogador] -- ^ A lista 'Jogador' resultante após aplicar a 'Jogada' Desacelera.
desacelera id js | verChao     = jsn
                 | otherwise   = js
                 where j       = js!!id
                       verChao = testaChao j
                       jsn     = alteraDesacelera id js

-- | Muda o estado do 'Jogador' Identificado (id) para False (Desacelera).

alteraDesacelera :: Int -- ^ O identificador do 'Jogador'.
                 -> [Jogador] -- ^ A lista 'Jogador' anterior.
                 -> [Jogador] -- ^ A lista 'Jogador' resultante após colocar a aceleração do 'Jogador' em False.
alteraDesacelera 0 (h:t)  = (mudaDesacelera h):t
alteraDesacelera id (h:t) = h:(alteraDesacelera (id-1) t)

-- | Muda o estado de aceleração do 'Jogador' para False (Acelera).

mudaDesacelera :: Jogador -> Jogador
mudaDesacelera j = j {estadoJogador = Chao False}


-- * Funções auxiliares da 'Jogada' Dispara.

-- | Aplica Cola ao 'Mapa', numa determinada posição de uma 'Pista'.

disparaNoMapa :: Int -- ^ O número da 'Pista' onde se pertende disparar.
              -> Double -- ^ A distância da origem à 'Peca', na 'Pista', de onde se dispara.
              -> Mapa -- ^ O 'Mapa' anterior.
              -> Mapa -- ^ O 'Mapa' resultante após aplicar a Cola.
disparaNoMapa 0 x (h:t)   = (disparaNaPista ((floor x)-1) h):t
disparaNoMapa npi x (h:t) = h : disparaNoMapa (npi-1) x t

-- | Aplica Cola numa determinada posição de uma 'Pista'

disparaNaPista :: Int -- ^ A posição na 'Pista' que recebe Cola. 
               -> Pista -- ^ A 'Pista' anterior.
               -> Pista -- ^ A 'Pista' resultante após aplicar a Cola.
disparaNaPista 0 (h:t) = (disparaNaPeca h) : t
disparaNaPista x (h:t) = h : (disparaNaPista (x-1) t)

-- | Altera o tipo de piso de uma 'Peca' para Cola.

disparaNaPeca :: Peca -- ^ A 'Peca' anterior.
              -> Peca -- ^ A 'Peca' resultante após aplicar a Cola.
disparaNaPeca (Recta p z1)    = (Recta Cola z1)
disparaNaPeca (Rampa p z1 z2) = (Rampa Cola z1 z2)  

-- | Retira uma munição ao 'Jogador' Identificado (id).

alteraMunicoes :: Int -- ^ O identificador do 'Jogador'.
               -> [Jogador] -- ^ A lista 'Jogador' anterior.
               -> [Jogador] -- ^ A lista 'Jogador' resultante após de diminuir em 1 as munições do 'Jogador'
alteraMunicoes 0 (j:t) = j {colaJogador = (colaJogador j)-1}:t
alteraMunicoes id (h:t)                 = h : alteraMunicoes (id-1) t

-- * Funções auxiliares à tarefa 2 de verificação.

-- | Testa se o Jogador está no chão.

testaChao :: Jogador -- ^ O 'Jogador' a verificar.
          -> Bool -- ^ True se o 'Jogador' estiver no Chao | False se o 'Jogador' não estiver.
testaChao (Jogador _ _ _ _ (Chao z1)) = True
testaChao (Jogador _ _ _ _ _)         = False

-- | Testa se o 'Jogador' está no ar.

testaAr :: Jogador -- ^ O 'Jogador' a verificar.
        -> Bool -- ^ True se o 'Jogador' estiver no Ar | False se o 'Jogador' não estiver.
testaAr (Jogador _ _ _ _ (Ar z1 z2 z3)) = True
testaAr (Jogador _ _ _ _ _)             = False

-- | Testa se o 'Jogador' já saiu da 'Peca' inicial.

testaPeca :: Jogador -- ^ O 'Jogador' a verificar.
          -> Bool -- ^ True se o 'Jogador' estiver depois da 1º 'Peca' | False se o 'Jogador' não estiver.
testaPeca (Jogador _ x _ _ _) = x >= 1

-- | Testa se o 'Jogador' tem muniçãoes para puder disparar.

testaMunicoes :: Jogador -- ^ O 'Jogador' a verificar.
              -> Bool -- ^ True se o Jogador tiver munições disponíveis| False se o Jogador não tiver.
testaMunicoes (Jogador _ _ _ mu _) = mu >= 1 && mu <= 5

-- | Testa se um 'Jogador' tem uma 'Pista' em cima dele para ele puder subir ou uma 'Pista' em baixo para puder descer.

testaPista :: Jogador -- ^ O 'Jogador' a verificar.
           -> Direcao -- ^ A 'Direcao' (B ou C).
           -> Int -- ^ O número de 'Pista'.
           -> Bool -- ^ True se o 'Jogador' puder aplicar o Movimento 'Direcao' | False se o Jogador não puder.
testaPista (Jogador pi _ _ _ _) dir npi = (dir == C && pi/=0) || (dir == B && pi/=(npi-1))

-- * Outras funções auxiliares à tarefa 2.

-- | Diz qual a 'Peca' em que o 'Jogador' se encontra e a 'Peca' acima(ou)abaixo (dependendo da 'Direcao' C(ou)B) dele.

escolhePecas :: Int -- ^ O número da 'Pista'.
             -> Double -- ^ A posiçao da 'Peca' na 'Pista'.
             -> Mapa -- ^ O 'Mapa'.
             -> Direcao -- ^ A 'Direcao'
             -> (Peca,Peca) -- ^ ('Peca' da posição e do número da 'Pista' dados, Se 'Direcao' (B) 'Peca' abaixo | Se 'Direcao' (C) 'Peca' acima)
escolhePecas npi x m dir | dir == C    = (peca,pecasup)
                         | dir == B    = (peca,pecainf)
                         where peca    = pecaJog npi x m
                               pecasup = pecaJog (npi-1) x m
                               pecainf = pecaJog (npi+1) x m

-- | Diz qual é a 'Peca' que se encontra numa determinada posição de uma 'Pista' de um 'Mapa'.

pecaJog :: Int -- ^ O número da 'Pista'.
        -> Double -- ^ A posiçao da 'Peca' na 'Pista'.
        -> Mapa -- ^ O 'Mapa'.
        -> Peca -- ^ 'Peca' da posição e do número da 'Pista' dados.
pecaJog npi x m = peca
                
                where peca  = pista!!(floor x)
                      pista = m!!npi

-- | Calcula a difrença entre duas alturas de duas 'Peca' numa determinada posição. 

calcDif :: Double -- ^ A distancia em relação à origem.
        -> (Peca,Peca) -- ^ As duas 'Peca'
        -> Double -- ^ Difrença de altura entre as duas 'Peca' numa determinada distância.
calcDif x (pe1,pe2) = (calcAlt x pe1) - (calcAlt x pe2)

-- | Calcula a inclinação de uma 'Peca'.

calcIncl :: Peca -- ^ A 'Peca'.
         -> Double -- ^ A inclinação da 'Peca'.
calcIncl (Recta p y)     = 0
calcIncl (Rampa p y1 y2) = atan(fromIntegral(y2-y1))

-- |  Calcula a Altura duma 'Peca' numa determinada Distância
--    (Se for Recta dá a altura da Recta | Se for Rampa calcula a altura através da semelhança de triângulos).

calcAlt :: Double -- ^ A distancia em relação à origem.
        -> Peca -- ^ A 'Peca'.
        -> Double -- ^ Altura da 'Peca' numa determinada distância.
calcAlt _ (Recta _ y)     = fromIntegral y
calcAlt x (Rampa _ yi yf) = fromIntegral(yi) + ((x-(fromIntegral(floor x)))*(fromIntegral(yf-yi)))

-- | Converte ângulos radianos em ângulos graus.

toGraus :: Double -- ^ Ângulo em radianos.
        -> Double -- ^ Ângulo em graus.
toGraus a = (a*180)/pi

