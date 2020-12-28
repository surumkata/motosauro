-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g002 where

import LI11920
import Tarefa0_2019li1g002
import Tarefa2_2019li1g002
import Tarefa4_2019li1g002
import LITestes_002

{-
= Introdução

  Neste módulo estão contidas as funções relativas ao bot.
  O objetivo é percorrer um dado mapa o mais rápido possível, tendo em conta que outros jogadores tentarão o mesmo.
  Este objetivo cumpre analisando o estado do jogo (mapa e posições dos outros jogadores).

= Objetivos
 
  *1 - Verificar se está no chao, no ar ou morto, e tomar decisões consoante isso.
  *2 - Verifica se é uma boa oportunidade para (Just Dispara) (caso esteja no Chao):
        -- se tiver munições.
        -- se tiver um jogador atrás (mesma pista, e distância-1) dele ou um boost.
        -- se a peça atrás dele ainda não é uma cola.
  *3 - Verifica se é melhor mudar de pista (caso esteja no Chao, depois de ter testado o Dispara):
        -- Verifica se pode movimentar para cima e para baixo.
            se puder -- calcula as peças a cima dele.
                     -- calcula as peças abaixo dele.
                     -- escolhe a direção que tem a peça com menor atrito e compara com a peça atual.
                        se o atrito da peça atual for menor -- Maybe Acelera.
                        se não -- Maybe (Movimenta direção).
            se não -- Verifica se pode movimentar só para cima.
                      se puder -- escolhe a melhor peça de cima e compara com a atual.
                            
                      se não -- Verifica se pode movimentar só para baixo.
                                se puder -- escolhe a melhor peça de baixo e compara com a atual.
                                se não -- Maybe Acelera.
  *4 - Verifica se deve movimentar-se para esquerda ou direita (caso esteja no Ar) atendendo que a tenta manter a inclinação ideal tal que:

                            (-30) >= (inclinação da peça por baixo do jogador - inclinação do jogador) > (-15)

  *5 - Nothing (caso esteja morto)

= Conclusão

  Ainda que limitado na profundidade a que examina a pista, o bot construído é relativamente eficaz.
    É rápido no ar, graças ao intervalo de ângulos específico que é utilizado.
    Perde por não simular várias jogadas à frente, quando está no chão, mas, por observação dos torneios do site, a falta dessa capacidade não parece afetar significativamente a prestação do bot.
    Para além disso, nas pistas usadas nos testes essa habilidade parece bastante situacional.
  Conclui-se assim que esta tarefa foi concluída satisfatóriamente, ainda que com noção de onde havia espaço para melhorar. 

-}

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.

bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot id e | testaChao j && testaDisparo j js m = Just Dispara
         | testaChao j                        = botChaoJoga j m
         | testaAr j                          = botArJoga j m
         | otherwise                          = Just Acelera

           where j  = js!!id
                 m  = mapaEstado e
                 js = jogadoresEstado e

-- * Funções para ver se aplica a (Just Dispara).

-- | Testa se o bot deve aplicar a jogada Dispara

testaDisparo :: Jogador -- ^ O bot.
             -> [Jogador] -- ^ Os 'Jogador' a correrem no 'Mapa'.
             -> Mapa -- ^ O 'Mapa'.
             -> Bool -- ^ True se deve | False se não.
testaDisparo j [] m = False
testaDisparo j1 (j2:js) m | temMunicoes && (not inicio) && mesmaPista && atrasJog && (not cola) = True
                          | temMunicoes && (not inicio) && boost                                = True
                          | otherwise                                                           = testaDisparo j1 js m

               where x1          = distanciaJogador j1
                     x2          = distanciaJogador j2
                     npi1        = pistaJogador j1
                     npi2        = pistaJogador j2
                     cola        = temCola pecaAtras
                     boost       = temBoost pecaAtras
                     inicio      = x1 <= 1.0
                     atrasJog    = x2 < fromIntegral(floor x1) && x2 > (fromIntegral(floor x1)-1)
                     pecaAtras   = pecaJog npi1 (x1-1) m
                     mesmaPista  = npi1 == npi2
                     temMunicoes = testaMunicoes j1

-- | Testa se uma 'Peca' tem como 'Piso' boost.

temBoost :: Peca -- ^ A 'Peca'.
         -> Bool -- ^ True se tiver | False se não.
temBoost (Recta Boost _)   = True
temBoost (Rampa Boost _ _) = True
temBoost _                 = False

-- | Testa se uma 'Peca' tem como 'Piso' cola.

temCola :: Peca -- ^ A 'Peca'.
        -> Bool -- ^ True se tiver | False se não.
temCola (Recta Cola _)   = True
temCola (Rampa Cola _ _) = True
temCola _                = False

-- * Funções para ver a melhor 'Jogada' no chao.

-- | Calcula a melhor 'Jogada' a ser efetuada pelo bot ,no Chao (à exceção da Dispara).

botChaoJoga :: Jogador -- ^ O bot.
            -> Mapa -- ^ O 'Mapa'.
            -> Maybe Jogada -- ^ A 'Jogada' calculada.
botChaoJoga j m | boolC && boolB = melhorJogada
                | boolC          = melhorJogadaC
                | boolB          = melhorJogadaB
                | otherwise      = Just Acelera
                where x             = distanciaJogador j
                      npi           = pistaJogador j
                      peca          = pecaJog npi x m
                      boolC         = podeMudar j m C
                      boolB         = podeMudar j m B
                      pecasC        = pecasCima npi x m
                      pecasB        = pecasBaixo npi x m
                      pecaMelhorC   = melhorPeca pecasC
                      pecaMelhorB   = melhorPeca pecasB
                      (dir,melhor)  = cimaOUbaixo pecasC pecasB
                      melhorJogada  = escolheLado (peca,melhor) dir
                      melhorJogadaC = escolheLado (peca,pecaMelhorC) C
                      melhorJogadaB = escolheLado (peca,pecaMelhorB) B

-- | Verifica se o bot pode movimentar-se para uma direção especifíca.

podeMudar :: Jogador -- ^ O bot.
          -> Mapa -- ^ O 'Mapa'.
          -> Direcao -- ^ A 'Direcao'.
          -> Bool -- ^ True se ele puder aplicar a 'Direcao' | False se não.
podeMudar j m dir = case dir of
                    C -> boolC
                    B -> boolB
                    
                    where x          = distanciaJogador j
                          npi        = pistaJogador j
                          peca       = pecaJog npi x m
                          difC       = calcDif x (peca,pecaAcima)
                          difB       = calcDif x (peca,pecaAbaixo)
                          boolC      = testaPista j C (length m) && abs(difC) <= 0.2
                          boolB      = testaPista j B (length m) && abs(difB) <= 0.2
                          pecaAcima  = pecaJog (npi-1) x m
                          pecaAbaixo = pecaJog (npi+1) x m
                      
-- | Recebe uma lista de 'Peca' e diz qual a com menos atrito.

melhorPeca :: [Peca] -- ^ A lista de 'Peca'.
           -> Peca -- ^ A 'Peca' com menor atrito.
melhorPeca [p1] = p1
melhorPeca (p1:p2:ps) | atrito piso1 < atrito piso2 = melhorPeca (p1:ps)
                      | otherwise                   = melhorPeca (p2:ps)
                      where piso1 = pisoDaPeca p1
                            piso2 = pisoDaPeca p2
-- | Recebe a lista de 'Peca' de cima, e a lista de 'Peca' de baixo e calcula a (melhorPeca) e diz se é de cima (C) ou de baixo (B).

cimaOUbaixo :: [Peca] -- ^ A lista de 'Peca' de cima.
            -> [Peca] -- ^ A lista de 'Peca' de baixo.
            -> (Direcao,Peca) -- ^ (A 'Direcao' da melhorPeca, a melhorPeca).
cimaOUbaixo p [] = (C,(melhorPeca p))
cimaOUbaixo [] p = (B,(melhorPeca p))
cimaOUbaixo (p1:ps1) (p2:ps2) | atrito (pisoDaPeca p1) <= atrito (pisoDaPeca p2) = cimaOUbaixo (p1:ps1) ps2
                              | otherwise = cimaOUbaixo ps1 (p2:ps2)

-- | Calcula a lista 'Peca' acima do bot.

pecasCima :: Int -- ^ Número da 'Pista'.
          -> Double -- ^ Distância do bot.
          -> Mapa -- ^ O 'Mapa'.
          -> [Peca] -- ^ A lista de 'Peca' de cima.
pecasCima 0 _ _   = []
pecasCima npi x m = (pecaJog (npi-1) x m):pecasCima (npi-1) x m

-- | Calcula a lista 'Peca' abaixo do bot.

pecasBaixo :: Int -- ^ Número da 'Pista'.
           -> Double -- ^ Distância do bot.
           -> Mapa -- ^ O 'Mapa'.
           -> [Peca] -- ^ A lista de 'Peca' de baixo.
pecasBaixo 3 _ _   = []
pecasBaixo npi x m = (pecaJog (npi+1) x m):pecasBaixo (npi+1) x m

-- | Recebe um par de 'Peca' ('Peca' atual,'Peca' ('Direcao')) e a 'Direcao' e calcula a melhor 'Jogada' a fazer.

escolheLado :: (Peca,Peca) -- ^ ('Peca' atual,'Peca' ('Direcao')).
            -> Direcao -- ^ A 'Direcao'.
            -> Maybe Jogada -- ^ (Just Acelera) se o atrito da 'Peca' atual for menor ou igual | (Just (Movimenta 'Direcao')) se não.
escolheLado (peca,pecaDir) dir | atritoM <= atritoDir = Just Acelera
                               | otherwise            = Just (Movimenta dir)
                                where atritoM   = atrito (pisoDaPeca peca)
                                      atritoDir = atrito (pisoDaPeca pecaDir)

-- * Funções para ver a melhor 'Jogada' no ar.

-- | Calcula a melhor 'Jogada' a ser efetuada pelo bot ,no Ar.

botArJoga :: Jogador -- ^ O bot.
          -> Mapa -- ^ 'O Mapa'.
          -> Maybe Jogada -- ^ A melhor 'Jogada'.
botArJoga j m | inclPeca - incl > (-15)  = Just (Movimenta E)
              | inclPeca - incl <= (-30) = Just (Movimenta D)
              | otherwise                = Just Acelera

              where x        = distanciaJogador j
                    npi      = pistaJogador j
                    incl     = inclinacaoJogador (estadoJogador j)
                    peca     = pecaJog npi x m
                    inclPeca = calcIncl peca