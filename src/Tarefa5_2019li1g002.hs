-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11920
import Tarefa2_2019li1g002
import Tarefa3_2019li1g002
import Tarefa4_2019li1g002
import Tarefa6_2019li1g002
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LITestes_002
import Control
import Numeric

{-
= Introdução
  
  Este ficheiro contém a implementação da Tarefa 5, ou seja do jogo em si, utilizando a biblioteca Gloss, e as restantes tarefas do projeto.
  O objetivo seria criar uma interface gráfica que permita jogar o jogo 

= Objetivos

  Alguns dos nossos objetivos nesta tarefa eram:
    Ter mapas pré-construídos, que fornecemos ao programa diretamente, bem como mapas gerados aleatoreamente, graças à Tarefa 1 e à função Gera
    Ter quatros jogadores jogáveis, o que se tornou difícil, devido ao espaço limitado num teclado.
    Implementar o nosso bot, e fazê-lo jogar por todos os jogadores não controlados.
    Implementar o jogo em si. Isto inclui:
      Importar imagens referentes a cada elemento do jogo(jogadores, peças, backgrounds, etc,..).
      Desenhar essas peças de acordo com o estado do jogo(conjunto de funções desenha).
      Fazer com que o jogo reaja aos imputs do jogador.
      Fazer com que as jogadas, quer do jogador, quer do bot, afetem o jogo de forma correta, usando a Tarefa 2.
      Fazer com que o jogo reaja corretamente à passagem do tempo, usando a Tarefa 4.
      Mostrar toda a informação relevante ao jogo no ecrã(munições, cronómetro, cores dos jogadores, etc,..).
    Implementar menus simples e intuítivos.
    Manter um design original e limpo em todo o jogo.

= Conclusão

  A implementação desta tarefa provou-se totalmente operacional, sendo que implementamos todas as funcionalidades que queriamos.
  No entanto lamentamos o design final que,  embora aceitável, e relativamente criativo, poderia estar mais polido.
  Utilizamos alguns artifícios no trabalho que facilitaram a realização, como a ferramenta mogrify que converte imagens em ficheiros .bmp, melhores no uso do gloss.   
  Concluimos assim que cumprimos a tarefa de forma pelo menos satisfatória, a todos os níveis. 
-}

-- | O 'IdJog' contem o 'Jogador', o índice, a inclinação da peça atual, altura da peça atual.

type IdJog = (Jogador,Int,Float,Float)

-- * Constantes.

-- | x Inicial.

xstart :: Float -- ^ x Inicial.
xstart = (-200)

-- | y Inicial.

ystart :: Float -- ^ y Inicial.
ystart = 150

-- | Tamanho dos Pixeis das 'Peca'.

l :: Float -- ^ Tamanho dos Pixeis * Scale
l = 100*sls 

-- | Framerates.

fr :: Int -- ^ Framerates
fr = 50

-- | Scale utilizado.

sls :: Float -- ^ Scale utilizado
sls = 0.6

-- | Cor de fundo.

background :: Color -- ^ Cor de fundo.
background = black

-- * Funções secúndarias da Tarefa 5.

-- | Desenha o Menu do Jogo.
desenhaMenu :: EstadoGloss -- ^ O 'EstadoGloss'.
            -> [Picture] -- ^ A lista de 'Picture'.
            -> Picture -- ^ Menu desenhado.
desenhaMenu (Menu Start)           pics = pics!!0
desenhaMenu (Menu ControlsB)       pics = pics!!1
desenhaMenu (Menu Exit)            pics = pics!!2
desenhaMenu (Menu Controls)        pics = pics!!3
desenhaMenu (Menu (Players n))     pics = pics!!(16-n)
desenhaMenu (Menu (DefaultMaps _)) pics = pics!!4
desenhaMenu (Menu (CreateMaps _))  pics = pics!!5
desenhaMenu (Menu (Maps n n2))     pics = Pictures ([pics!!(5+n2)])
desenhaMenu (Menu (Length n n2))   pics = Pictures ([pics!!10]++[Translate (-35) 10 (Scale 0.5 0.5 (Text (show n2)))])
desenhaMenu (Menu (Seed n n2 n3))  pics = Pictures ([pics!!11]++[Translate (-85) 10 (Scale 0.5 0.5 (Text (show n3)))])

-- | Pega na 'Pista' depois do fim e desenha-a.

desenhaFim :: Pista -- ^ a 'Pista' depois do fim.
           -> (Float,Float) -- ^ Coordenadas de referência. 
           -> [Picture] -- ^ Lista de 'Picture'.
           -> [Picture] -- ^ a 'Pista' depois do fim desenhada.
desenhaFim [] _ _           = []
desenhaFim (h:t) (x,y) pics = pecaDesenhada ++ pistaContinuacaoRecta

                            where dif                   = difrencaRampa h
                                  pecaDesenhada         = desenhaPeca h (x,y) pics
                                  pistaContinuacaoRecta = desenhaFim t (x+l,y) pics

-- | Desenha a 'Pista'.

desenhaPista :: Pista -- ^ A 'Pista' a ser desenhada.
             -> (Float,Float) -- ^ Coordenadas de referência.  
             -> [Picture] -- ^ Lista de 'Picture'.
             -> [Picture] -- ^ a 'Pista' desenhada.
desenhaPista [h] (x,y) pics = case testaRampa h of
                              True  -> pecaDesenhada ++ pistaContinuacaoRampa
                              False -> pecaDesenhada ++ pistaContinuacaoRecta

                              where yf                    = alturaFinal h
                                    dif                   = difrencaRampa h
                                    unders                = fazLista 5 10 pics
                                    finalRampa            = desenhaFim replicaFinal (x+2*l,y+l*dif) pics
                                    finalRecta            = desenhaFim replicaFinal (x+2*l,y) pics
                                    finishRampa           = Translate (x+l) (y+l*dif) (Scale sls sls (pics!!16))
                                    finishRecta           = Translate (x+l) y (Scale sls sls (pics!!16))
                                    pecaDesenhada         = desenhaPeca h (x,y) pics
                                    replicaFinal          = replicaFim h 15
                                    pistaContinuacaoRampa = (finishRampa:(desenhaUnder (yf-1) ((x+l),(y+l*dif)-l) Terra unders))++finalRampa
                                    pistaContinuacaoRecta = (finishRecta:(desenhaUnder (yf-1) ((x+l),y-l) Terra unders))++finalRecta

desenhaPista (h:t) (x,y) pics = case testaRampa h of
                              True  -> pecaDesenhada ++ pistaContinuacaoRampa
                              False -> pecaDesenhada ++ pistaContinuacaoRecta

                              where dif                   = difrencaRampa h
                                    pecaDesenhada         = desenhaPeca h (x,y) pics
                                    pistaContinuacaoRampa = desenhaPista t (x+l,y+l*dif) pics
                                    pistaContinuacaoRecta = desenhaPista t (x+l,y) pics

-- | Desenha a 'Peca'.

desenhaPeca :: Peca -- ^ A 'Peca'. 
            -> (Float,Float) -- ^ Coordenadas de referência. 
            -> [Picture] -- ^ Lista de 'Picture'.
            -> [Picture] -- ^ A 'Peca' desenhada.

desenhaPeca peca (x,y) pics = case testaRampa peca of
                              True -> case dif == 1 of
                                      True  -> (Translate x y rampaDe1) : unders1

                                      False -> case dif == (-1) of
                                            True  -> (Translate x (y-l) rampaDe1B) : unders1b

                                            False -> case dif == 2 of
                                                  True  -> (Translate x y rampaDe2): unders2

                                                  False -> (Translate x y rampaDeMaisB): undersMaisB

                              False -> (Translate x y rectaDes) : (desenhaUnder (yf-1) (x,y-l) piso unders)
                                          
                              where yi           = alturaInicial peca
                                    yf           = alturaFinal peca
                                    dif          = fromIntegral(yf-yi)
                                    piso         = pisoDaPeca peca
                                    coords       = (((abs dif)-1)*(-(l/2)),(dif*l))
                                    rectas       = take 5 pics
                                    unders       = fazLista 5 10 pics
                                    rampas       = fazLista 10 15 pics
                                    unders1      = desenhaUnder (yf-2) (x,y-l) piso unders
                                    unders2      = desenhaUnder (yi-1) (x,y-l) piso unders
                                    unders1b     = desenhaUnder (yf-1) (x,y-2*l) piso unders
                                    rectaDes     = desenhaPiso piso rectas
                                    rampaDe1     = desenhaPiso piso rampas
                                    rampaDe2     = Scale (1/dif) 1 (Pictures rampasReplic)
                                    rampaDe1B    = Scale (-1) 1 (desenhaPiso piso rampas)
                                    undersMaisB  = desenhaUnder (yf-1) (x,y-(1+(abs dif))*l) piso unders
                                    rampaDeMaisB = Scale ((-1)/(abs dif)) 1 (Pictures rampasRepliB)
                                    rampasReplic = desenhaRampas (replicaRampas (ceiling(dif)) peca) ((-(l/2)),0) pics
                                    rampasRepliB = desenhaRampas (replicaRampas (ceiling(abs dif)) peca) coords pics

-- | Desenha a parte de baixo da 'Peca'.

desenhaUnder :: Int -- ^ Número de unders necessários.
             -> (Float,Float) -- ^ Coordenadas de referência. 
             -> Piso -- ^ O 'Piso'.
             -> [Picture] -- ^ A lista de 'Picture'.
             -> [Picture] -- ^ A parte de baixo da 'Peca' desenhada.
desenhaUnder 0 (x,y) piso pics = [(Translate x y (desenhaPiso piso pics))]
desenhaUnder n (x,y) piso pics = case n>0 of
                                 True -> (Translate x y (desenhaPiso piso pics) ) : desenhaUnder (n-1) (x,y-l) piso pics
                                 False -> []

-- | Desenha o 'Piso' da 'Peca'.

desenhaPiso :: Piso -- ^ O 'Piso'.
            -> [Picture] -- ^ A lista de 'Picture'.
            -> Picture -- ^ 'Piso' da 'Peca' desenhado.
desenhaPiso Terra pics = Scale sls sls (pics!!0)
desenhaPiso Relva pics = Scale sls sls (pics!!1)
desenhaPiso Lama  pics = Scale sls sls (pics!!2)
desenhaPiso Cola  pics = Scale sls sls (pics!!3)
desenhaPiso Boost pics = Scale sls sls (pics!!4)

-- | Auxilia o desenho das rampas de +1 de difrença de altura.

desenhaRampas :: [Peca] -- ^ As rampas a serem desenhadas.
             -> (Float,Float) -- ^ Coordenadas de referência. 
             -> [Picture] -- ^ Lista de 'Picture'.
             -> [Picture] -- ^ As rampas desenhadas.
desenhaRampas [] _ _           = []
desenhaRampas (h:t) (x,y) pics = pecaDesenhada ++ pistaContinuacaoRampa

                               where dif                   = difrencaRampa h
                                     pecaDesenhada         = desenhaPeca h (x,y) pics
                                     pistaContinuacaoRampa = desenhaRampas t (x+l,y+l*dif) pics

-- | Cria a 'Pista' depois do fim da 'Pista' original.

replicaFim :: Peca -- ^ A 'Peca' final da 'Pista' original.
           -> Int -- ^ Número de vezes a ser repetida.
           -> Pista -- ^ a 'Pista' depois do fim.
replicaFim _ 0              = []
replicaFim (Recta _ y) n    = (Recta Terra y):(replicaFim (Recta Terra y) (n-1))
replicaFim (Rampa _ _ yf) n = (Recta Terra yf):(replicaFim (Recta Terra yf) (n-1))

-- | Testa se uma Peca é Rampa.

testaRampa :: Peca -- ^ A 'Peca'.
           -> Bool -- ^ True se for Rampa | False se não.
testaRampa (Rampa _ _ _) = True
testaRampa _             = False

-- | Descobre a difrença de alturas numa Rampa.

difrencaRampa :: Peca -- ^ A rampa.
              -> Float -- ^ Difrença de alturas da rampa.
difrencaRampa (Rampa _ yi yf) = fromIntegral(yf-yi)

-- | Descobre a altura inicial de uma 'Peca'.

alturaInicial :: Peca -- ^ A 'Peca'.
              -> Int -- ^ A altura inicial.
alturaInicial (Rampa _ yi _) = yi
alturaInicial (Recta _ y)    = y

-- | Descobre a altura final de uma 'Peca'.

alturaFinal :: Peca -- ^ A 'Peca'.
            -> Int -- ^ A altura final.
alturaFinal (Rampa _ _ yf) = yf
alturaFinal (Recta _ y)    = y

-- | Estado Inicial do Jogo.

estadoInicial :: EstadoGloss -- ^ Estado Inicial
estadoInicial = Menu Start

-- | Desenha o EstadoGloss.

desenhaEstado :: [Picture] -- ^ A lista de 'Picture'.
              -> EstadoGloss -- ^ O 'Estado Gloss'.
              -> Picture -- ^ O 'EstadoGloss' desenhado.

desenhaEstado pics eg = case verifMenu eg of
                        True -> desenhaMenu eg (fazLista 22 38 pics)

                        False -> case verifPlay ej of
                                 True -> Pictures (fundo++mapaejogadores++placar)
                                 False -> case verifStart ej of
                                          True -> Pictures (fundo++mapaejogadores++semaforo)

                                          False -> case verifPause ej of
                                                   True -> Pictures (fundo++mapaejogadores++pause)

                                                   False -> Pictures [menufinish,numeroJogFinish,timerpic,jogVencedor]
                        

                        where e               = estado ej
                              m               = mapaEstado e
                              ej              = estadoJogo eg
                              js              = jogadoresEstado e
                              pause           = [desenhaPause option (fazLista 38 42 pics)]
                              option          = bottom ej
                              placar          = desenhaPlacar (jogadoresEstado e) timeplay (last pics)
                              playerj         = player ej
                              timerpic        = (Translate (-182) (-152) (Scale 0.4 0.4 (Text ((showFFloat (Just 2) timerfinish "") ++ "seg."))))
                              timeplay        = timer ej
                              semaforo        = [Translate 0 0 (pics!!(46+(floor timerstart)))]
                              menufinish      = (Translate 0 0 (pics!!46))
                              timerstart      = timestart ej
                              timerfinish     = timefinish ej
                              jogVencedor     = (escolhePlayer (playerj-1) (fazLista 17 22 pics))
                              mapaejogadores  = desenhaTudo 0 m js (fazLista 0 22 pics)
                              numeroJogFinish = (Translate (-79) (-80) (Scale 0.6 0.6 (Text (show playerj))))
                              fundo           = [(pics!!42)]

-- | Desenha o 'Jogador' vencedor.

escolhePlayer :: Int -- ^ Índice do 'Jogador' vencedor.
              -> [Picture] -- ^ A lista de 'Picture'.
              -> Picture -- ^ 'Jogador' vencedor desenhado. 
escolhePlayer n pics = Translate (-240) 80 (Scale 2 2 (pics!!n))


-- | Desenha o Pause do Jogo.

desenhaPause :: Tipo -- ^ Tipo de Pause a ser desenhado. 
             -> [Picture] -- ^ A lista de 'Picture'.
             -> Picture -- ^ Tipo de Pause desenhado.
desenhaPause Resume pics         = pics!!0
desenhaPause ControlsBPause pics = pics!!1
desenhaPause MainMenu pics       = pics!!2
desenhaPause ControlsPause pics  = pics!!3



-- | Reage a um evento e devide-se em reageEventoMenu (se estiver no Menu) e reageEventoPlay (se estiver no Jogo)

reageEvento :: Event -- ^ O 'Event'.
            -> EstadoGloss -- ^ O 'EstadoGloss'
            -> EstadoGloss -- ^ 'EstadoGloss' resultante do 'Event'.
reageEvento e s = case verifMenu s of
                  True -> reageEventoMenu e s
                  False -> reageEventoPlay e s

-- | Aplica a passagem do tempo ao EstadoJogo

passoGloss :: Float -- ^ O Tempo.
           -> EstadoJogo -- ^ O 'EstadoJogo'
           -> EstadoJogo -- ^ O 'EstadoJogo' resultante da passagem do tempo.
passoGloss t (Play e nc time) = case verifFinal (length ((mapaEstado e)!!0)) (jogadoresEstado e) of
                                       False -> (Play e {mapaEstado = mapaN, jogadoresEstado = novalj} nc (time+t))
                                       True  -> (Finish player time)
                                       
                                       where bots       = jogaBot (nc+1) e
                                             novalj     = passoJogadores t (mapaEstado bots) (jogadoresEstado bots)
                                             mapaN      = mapaEstado bots
                                             (_,player) = maisLonge 1 (jogadoresEstado e)
passoGloss _ s = s

-- | Aplica as jogadas aos bots

jogaBot :: Int -- ^ Índice do bot.
        -> Estado -- ^ O 'Estado'
        -> Estado -- ^ O 'Estado' resultante da jogada efetuada do bot.
jogaBot 4 e = e
jogaBot n e = jogaBot (n+1) novoe
          
            where novoe              = jogada n jogadaDoBot e
                  (Just jogadaDoBot) = bot n e

-- | Aplica a função passo a todos os 'Jogador' es.

passoJogadores :: Float -- ^ O tempo.
               -> Mapa -- ^ O 'Mapa'.
               -> [Jogador] -- ^ A lista de 'Jogador' es.
               -> [Jogador] -- ^ A lista de 'Jogador' es resultante da passagem do tempo.
passoJogadores t m []     = []
passoJogadores t m (j:js) = (passo (realToFrac t) m j) : (passoJogadores t m js) 

-- | Verifica se algum 'Jogador' chegou ao final.

verifFinal :: Int -- ^ Comprimento do 'Mapa'.
           -> [Jogador] -- ^ A lista 'Jogador' es.
           -> Bool -- ^ True se chegou ao final | False se não.
verifFinal n []    = False
verifFinal n (h:t) | distanciaJogador h >= fromIntegral (n)-0.1 = True
                   | otherwise                                  = verifFinal n t


-- | Função que determinada o tempo decorrido.

reageTempoGloss :: Float 
                -> EstadoGloss 
                -> EstadoGloss
reageTempoGloss n eg = reageTempo 0.015 eg

-- | Altera o 'EstadoGloss' do jogo com um determinado tempo.

reageTempo :: Float -- ^
           -> EstadoGloss -- ^
           -> EstadoGloss -- ^
reageTempo n (Jogo (Finish player time) t) = (Jogo (Finish player time) t)
reageTempo n (Jogo (StartJ e nc time) t)   | n+time > 0 = (Jogo (Play e nc 0) t)
                                           | otherwise  = (Jogo (StartJ e nc (time+n)) t)
reageTempo n eg = case verifMenu eg of 
                  True  -> eg
                  False -> case finish of 
                           False -> novoeg {estadoJogo = passoGloss n (estadoJogo novoeg) , teclas = novasKeys}
                           True  -> eg {estadoJogo = finishGame (estadoJogo eg)}
                  
                  where e         = estado ej
                        ej        = estadoJogo eg
                        novoeg    = reageTempoPlay eg
                        finish    = verifFinal (length ((mapaEstado e)!!0)) (jogadoresEstado e)
                        novasKeys = resetKeyLista (teclas eg)

-- | Altera o 'EstadoJogo' para Finish.

finishGame :: EstadoJogo -- ^ O 'EstadoJogo'
           -> EstadoJogo -- ^ O 'EstadoJogo' Finish.
finishGame (Play e _ time) = (Finish player time)
                           
                           where (_,player) = maisLonge 1 (jogadoresEstado e)
finishGame s = s

-- | Pega numa lista e 2 índices e faz uma lista do primeiro índice ao segundo.

fazLista :: Int -- ^ 1º Índice.
         -> Int -- ^ 2º Índice.
         -> [a] -- ^ A lista.
         -> [a] -- ^ A lista resultante.
fazLista s f lista = drop s (take f lista)  

-- | Replica uma rampa n vezes.

replicaRampas :: Int -- ^ Número de vezes.
              -> Peca -- ^ A rampa.
              -> [Peca] -- ^ A rampa replicada n vezes.
replicaRampas 0 _ = []
replicaRampas n (Rampa piso yi yf) = replicaRampas (n-1) (Rampa piso yi yf) ++ [Rampa piso (n-1) n]

-- | Produz o 'IdJog' de um 'Jogador' de uma determinada pista.

jogNaPista :: Int -- ^ O índice do 'Jogador'
           -> Int -- ^ Número da pista.
           -> Pista -- ^ A 'Pista'.
           -> [Jogador] -- ^ A lista de 'Jogador' es.
           -> [IdJog] -- ^ O 'IdJog'.
jogNaPista _ _ _ [] = []
jogNaPista id n pis (j:js) = case n==(pistaJogador j) of
                             True  -> (j,id,incl,alt):jogNaPista (id+1) n pis js
                             False -> jogNaPista (id+1) n pis js
                         
                         where x    = distanciaJogador j
                               alt  = realToFrac(calcAlt x (pis!!(floor x)))
                               incl = realToFrac(-(toGraus (calcIncl (pis!!(floor x)))))

-- | Desenha o 'Mapa' e os 'Jogador' es.

desenhaTudo :: Int -- ^ Número da Pista atual.
            -> Mapa -- ^ O 'Mapa'.
            -> [Jogador] -- ^ A lista de 'Jogador'.
            -> [Picture] -- ^ A lista de 'Picture'.
            -> [Picture] -- ^ O 'Mapa' e os 'Jogador' es desenhados.
desenhaTudo _ [] _ _ = []
desenhaTudo n (h:t) js pics = inicio ++ pistaPic ++ jogadoresPic ++ desenhaTudo (n+1) t js pics
                                   
                                   where d                = realToFrac (distanciaJogador (jogMaisLonge))
                                         (x,y)            = (xstart-d*l,ystart-fromIntegral(n)*l)
                                         inicio           = desenhaStart 8 (x-8*l,y) picinic
                                         picinic          = [(pics!!0)]++[(pics!!15)]
                                         pistaPic         = desenhaPista (tail h) (x+l,y) (fazLista 0 17 pics)
                                         jogadoresPic     = desenhaJogs n (x,y) jsDaPista (fazLista 17 22 pics)
                                         jsDaPista        = jogNaPista 0 n h js
                                         (jogMaisLonge,_) = maisLonge 1 js

-- | Desenha as 'Peca' antes do começo da 'Pista'.

desenhaStart :: Int -- ^ Número de 'Peca' a desenhar antes do começo.
             -> (Float,Float) -- ^ Coordenadas de referência.
             -> [Picture] -- ^ A lista de 'Picture'.
             -> [Picture] -- ^ As 'Peca' antes do começo da 'Pista' desenhadas.
desenhaStart 0 (x,y) pics = [(Translate x y (Scale sls sls (pics!!1)))]
desenhaStart n (x,y) pics = (Translate x y (Scale sls sls (pics!!0))):desenhaStart (n-1) (x+l,y) pics

-- | Desenha os 'Jogador' es.

desenhaJogs :: Int -- ^ Número da 'Pista'.
            -> (Float,Float) -- ^ Coordenadas de referência. 
            -> [IdJog] -- ^ A lista de 'IdJog' do 'Jogador' es.
            -> [Picture] -- ^ A lista de 'Picture' de 'Jogador' es
            -> [Picture] -- ^ Os 'Jogador' es desenhados.
desenhaJogs _ _ [] _           = []
desenhaJogs n (x,y) (h:t) pics = desenhaJog n (x-(l/1.5),y-(l/2.5)) h pics : desenhaJogs n (x,y) t pics

-- | Desenha o 'Jogador'.

desenhaJog :: Int -- ^ Número da 'Pista'.
           -> (Float,Float) -- ^ Coordenadas de referência.
           -> IdJog -- ^ 'IdJog' do 'Jogador'.
           -> [Picture] -- ^ A lista de 'Picture' de 'Jogador' es.
           -> Picture -- ^ O 'Jogador' desenhado.
desenhaJog n (x,y) (j,id,incl,alt) pics | testaChao j = Translate (x+d*l) (y+alt*l) (Rotate i (Scale (sls/1.5) (sls/1.5) (pics!!id)))
                                        | testaAr j   = Translate (x+d*l) (y+yAr*l) (Rotate iAr (Scale (sls/1.5) (sls/1.5) (pics!!id)))
                                        | otherwise   = Translate (x+d*l) (y+alt*l) (Rotate i (Scale (sls/1.5) (sls/1.5) (pics!!4)))
                                        where d   = realToFrac (distanciaJogador j)
                                              i   = realToFrac (incl)
                                              ej  = estadoJogador j
                                              iAr = - (realToFrac (inclinacaoJogador ej))
                                              yAr = realToFrac (alturaJogador ej)


-- | Desenha o placar dos stats dos 'Jogador'.

desenhaPlacar :: [Jogador] -- ^ A lista de 'Jogador'.
              -> Float -- ^ Coordenadas de referência.
              -> Picture -- ^ 'Picture' do placar.
              -> [Picture] -- ^ Placar desenhado com os stats.
desenhaPlacar [] time pic = []
desenhaPlacar js time pic = [pic,tempo,inf]
                          where inf   = Pictures (escrevePlacar js (45,(-165)))
                                tempo = Translate 265 (-232) (Scale 0.28 0.28 (Text ((showFFloat (Just 3) time "") ++ " seg.")))

-- | Escreve os stats dos 'Jogador' no placar.

escrevePlacar :: [Jogador] -- ^ A lista de 'Jogador'.
              -> (Float,Float) -- ^ Coordenadas de referência.
              -> [Picture] -- ^ O placar de stats desenhado.
escrevePlacar [] _         = []
escrevePlacar (j:js) (x,y) = [Translate x y (Scale 0.28 0.28 (Text (show mun)))] ++ escrevePlacar js (x,y-45)


                              where mun = colaJogador j


-- * Funções principais da Tarefa 5.

-- | Janela.

dm :: Display -- ^ Janela
dm = InWindow "Motosauro" (1280, 720) (0, 0)

-- | Função principal.

main :: IO () -- ^ IO.
main = do
     rtte <- loadBMP "textures/Layers/LayerRectaTerra.bmp"
     rtre <- loadBMP "textures/Layers/LayerRectaRelva.bmp"
     rtla <- loadBMP "textures/Layers/LayerRectaLama.bmp"
     rtco <- loadBMP "textures/Layers/LayerRectaCola.bmp"
     rtbo <- loadBMP "textures/Layers/LayerRectaBoost.bmp"
     unte <- loadBMP "textures/Layers/LayerUnderTerra.bmp"
     unre <- loadBMP "textures/Layers/LayerUnderRelva.bmp"
     unla <- loadBMP "textures/Layers/LayerUnderLama.bmp"
     unco <- loadBMP "textures/Layers/LayerUnderCola.bmp"
     unbo <- loadBMP "textures/Layers/LayerUnderBoost.bmp"
     rpte <- loadBMP "textures/Layers/LayerRampaTerra.bmp"
     rpre <- loadBMP "textures/Layers/LayerRampaRelva.bmp"
     rpla <- loadBMP "textures/Layers/LayerRampaLama.bmp"
     rpco <- loadBMP "textures/Layers/LayerRampaCola.bmp"
     rpbo <- loadBMP "textures/Layers/LayerRampaBoost.bmp"
     star <- loadBMP "textures/Layers/LayerStart.bmp"
     fini <- loadBMP "textures/Layers/LayerFinish.bmp"
     mota <- loadBMP "textures/Others/mota.bmp"
     mot2 <- loadBMP "textures/Others/mota2.bmp"
     mot3 <- loadBMP "textures/Others/mota3.bmp"
     mot4 <- loadBMP "textures/Others/mota4.bmp"
     mort <- loadBMP "textures/Others/morto.bmp"
     men1 <- loadBMP "textures/Menu/men1.bmp"
     men2 <- loadBMP "textures/Menu/men2.bmp"
     men3 <- loadBMP "textures/Menu/men3.bmp"
     men4 <- loadBMP "textures/Menu/men4.bmp"
     men5 <- loadBMP "textures/Menu/men5.bmp"
     men6 <- loadBMP "textures/Menu/men6.bmp"
     men7 <- loadBMP "textures/Menu/men7.bmp"
     men8 <- loadBMP "textures/Menu/men8.bmp"
     men9 <- loadBMP "textures/Menu/men9.bmp"
     men10 <- loadBMP "textures/Menu/men10.bmp"
     men11 <- loadBMP "textures/Menu/men11.bmp"
     men12 <- loadBMP "textures/Menu/men12.bmp"
     men13 <- loadBMP "textures/Menu/men13.bmp"
     men14 <- loadBMP "textures/Menu/men14.bmp"
     men15 <- loadBMP "textures/Menu/men15.bmp"
     men16 <- loadBMP "textures/Menu/men16.bmp"
     pause1 <- loadBMP "textures/Menu/pause1.bmp"
     pause2 <- loadBMP "textures/Menu/pause2.bmp"
     pause3 <- loadBMP "textures/Menu/pause3.bmp"
     pause4 <- loadBMP "textures/Menu/pause4.bmp"
     fundo <- loadBMP "textures/Others/background.bmp"
     time3 <- loadBMP "textures/Others/time3.bmp"
     time2 <- loadBMP "textures/Others/time2.bmp"
     time1 <- loadBMP "textures/Others/time1.bmp"
     finish <- loadBMP "textures/Menu/finish.bmp"
     stats <- loadBMP "textures/Others/stats.bmp"

     play dm         -- janela onde irá correr o jogo
      background     -- côr do fundo da janela
      fr              -- frame rate
      estadoInicial  -- estado inicial
      (desenhaEstado [rtte,rtre,rtla,rtco,rtbo,unte,unre,unla,unco,unbo,rpte,rpre,rpla,rpco,rpbo,
                      star,fini,mota,mot2,mot3,mot4,mort,men1,men2,men3,men4,men5,men6,men7,men8,
                      men9,men10,men11,men12,men13,men14,men15,men16,pause1,pause2,pause3,pause4,
                      fundo,time3,time2,time1,finish,stats])  -- desenha o estado do jogo
      reageEvento     -- reage a um evento
      reageTempoGloss      -- reage ao passar do tempo