-- | Este módulo auxilia a Main na execução do Jogo.
module Control where

import Tarefa4_2019li1g002
import Tarefa2_2019li1g002
import Tarefa1_2019li1g002
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI11920
import LITestes_002

-- | O EstadoGloss pode ser um Jogo ou Menu.

data EstadoGloss = Jogo { estadoJogo :: EstadoJogo , teclas :: [Maybe Key] }
                 | Menu { botão :: Tipo }
                 deriving (Show,Eq)

-- | Tipo de Menu ou Tipo de (Jogo Pause).

data Tipo = Start
             | Exit 
             | Players Int
             | DefaultMaps Int
             | CreateMaps Int
             | Maps Int Int 
             | Length Int Int
             | Seed Int Int Int 
             | ControlsB
             | Controls 
             | Resume
             | ControlsPause
             | ControlsBPause
             | MainMenu
             deriving (Show,Eq)

-- | O EstadoJogo pode ser StartJ, Play, Pause ou Finish.

data EstadoJogo = StartJ { estado :: Estado , ncontrols :: Int, timestart :: Float}
                | Play { estado :: Estado , ncontrol :: Int, timer :: Float } 
                | Pause { estado :: Estado ,ncontrolp :: Int , bottom :: Tipo, timefrezzer :: Float }
                | Finish { player :: Int , timefinish :: Float } deriving (Show,Eq)

-- | Calcula o EstadoGloss reagido do Enter.

reageKeyEnter :: EstadoGloss -- ^ O 'EstadoGloss'.
              -> EstadoGloss
reageKeyEnter (Menu Start)                               = (Menu (Players 1))
reageKeyEnter (Menu ControlsB)                           = (Menu Controls)
reageKeyEnter (Menu Exit)                                = undefined
reageKeyEnter (Menu (Players n))                         = (Menu (DefaultMaps n))
reageKeyEnter (Menu (DefaultMaps n))                     = (Menu (Maps n 1))
reageKeyEnter (Menu (CreateMaps n))                      = (Menu (Length n 10))
reageKeyEnter (Menu (Maps n n2))                         = (Jogo (StartJ (Estado (escolheMapa n2) jsJogo) (n-1) (-3)) [Nothing])
reageKeyEnter (Menu (Length n n2))                       = (Menu (Seed n n2 1))
reageKeyEnter (Menu (Seed n n2 n3))                      = (Jogo (StartJ (Estado (gera 4 n2 n3) jsJogo) (n-1) (-3)) [Nothing])
reageKeyEnter (Jogo (Pause e n (Resume) time) t)         = (Jogo (Play e {jogadoresEstado = desacelarados} n time) [])
                                                         where desacelarados = desaceleraJogadores (jogadoresEstado e)
reageKeyEnter (Jogo (Pause e n (ControlsBPause) time) t) = (Jogo (Pause e n (ControlsPause) time) t)
reageKeyEnter (Jogo (Pause e n (MainMenu) time) _)       = (Menu Start)
reageKeyEnter m                                          = m

-- | Desacelera todos os Jogadores.

desaceleraJogadores :: [Jogador] -> [Jogador]
desaceleraJogadores []     = []
desaceleraJogadores (j:js) | testaChao j =  mudaDesacelera j : desaceleraJogadores js
                           | otherwise   = j : desaceleraJogadores js

-- | Escolhe o Mapa a ser executado no jogo.

escolheMapa :: Int -> Mapa
escolheMapa 1 = mJogo1
escolheMapa 2 = mJogo2
escolheMapa 3 = mJogo3
escolheMapa 4 = mJogo4

-- | Calcula o EstadoGloss reagido do KeyDown.

reageKeyDown :: EstadoGloss -> EstadoGloss
reageKeyDown (Menu Start)                               = (Menu ControlsB)
reageKeyDown (Menu ControlsB)                           = (Menu Exit)
reageKeyDown (Menu Exit)                                = (Menu Start)
reageKeyDown (Jogo (Pause e n (Resume) time) t)         = (Jogo (Pause e n (ControlsBPause) time) t)
reageKeyDown (Jogo (Pause e n (ControlsBPause) time) t) = (Jogo (Pause e n (MainMenu) time) t)
reageKeyDown (Jogo (Pause e n (MainMenu) time) t)       = (Jogo (Pause e n (Resume) time) t)

reageKeyDown e                                          = e

-- | Calcula o EstadoGloss reagido do KeyUp.

reageKeyUp :: EstadoGloss -> EstadoGloss
reageKeyUp (Menu ControlsB)                           = (Menu Start) 
reageKeyUp (Menu Exit)                                = (Menu ControlsB)
reageKeyUp (Menu Start)                               = (Menu Exit)
reageKeyUp (Jogo (Pause e n (Resume) time) t)         = (Jogo (Pause e n (MainMenu) time) t)
reageKeyUp (Jogo (Pause e n (ControlsBPause) time) t) = (Jogo (Pause e n (Resume) time) t)
reageKeyUp (Jogo (Pause e n (MainMenu) time) t)       = (Jogo (Pause e n (ControlsBPause) time) t)
reageKeyUp e                                          = e

-- | Calcula o EstadoGloss reagido do KeyLeft.

reageKeyLeft :: EstadoGloss -> EstadoGloss
reageKeyLeft (Menu (CreateMaps n))  = (Menu (DefaultMaps n))
reageKeyLeft (Menu (DefaultMaps n)) = (Menu (CreateMaps n))
reageKeyLeft (Menu (Players n))     = case n>1 of
                                      True  -> (Menu (Players (n-1)))
                                      False -> (Menu (Players 1))
reageKeyLeft (Menu (Maps n n2))     = case n2>1 of
                                      True  -> (Menu (Maps n (n2-1)))
                                      False -> (Menu (Maps n 4)) 
reageKeyLeft (Menu (Length n n2))   = case n2>10 of
                                      True  -> (Menu (Length n (n2-1)))
                                      False -> (Menu (Length n 100)) 
reageKeyLeft (Menu (Seed n n2 n3))  = case n3>1 of
                                      True  -> (Menu (Seed n n2 (n3-1)))
                                      False -> (Menu (Seed n n2 1000)) 
reageKeyLeft e                      = e

-- | Calcula o EstadoGloss reagido do KeyRight.

reageKeyRight :: EstadoGloss -> EstadoGloss
reageKeyRight (Menu (DefaultMaps n)) = (Menu (CreateMaps n))
reageKeyRight (Menu (CreateMaps n))  = (Menu (DefaultMaps n))
reageKeyRight (Menu (Players n))     = case n<4 of
                                       True  -> (Menu (Players (n+1)))
                                       False -> (Menu (Players 4)) 
reageKeyRight (Menu (Maps n n2))     = case n2<4 of
                                       True  -> (Menu (Maps n (n2+1)))
                                       False -> (Menu (Maps n 1)) 
reageKeyRight (Menu (Length n n2))   = case n2<100 of
                                       True  -> (Menu (Length n (n2+1)))
                                       False -> (Menu (Length n 10)) 
reageKeyRight (Menu (Seed n n2 n3))  = case n3<1000 of
                                       True  -> (Menu (Seed n n2 (n3+1)))
                                       False -> (Menu (Seed n n2 1)) 
reageKeyRight e                      = e

-- | Calcula o EstadoGloss reagido do F1.

reageKeyF1 :: EstadoGloss -> EstadoGloss
reageKeyF1 (Menu Controls)                           = (Menu ControlsB)
reageKeyF1 (Menu (DefaultMaps n))                    = (Menu (Players n)) 
reageKeyF1 (Menu (CreateMaps n))                     = (Menu (Players n))
reageKeyF1 (Menu (Players _))                        = (Menu Start)
reageKeyF1 (Menu (Maps n _))                         = (Menu (DefaultMaps n))
reageKeyF1 (Menu (Length n _))                       = (Menu (CreateMaps n))
reageKeyF1 (Menu (Seed n n2 _))                      = (Menu (Length n n2))
reageKeyF1 (Jogo (Pause e n (ControlsPause) time) t) = (Jogo (Pause e n (ControlsBPause) time) t)
reageKeyF1 m                                         = m 

-- | Calcula o EstadoGloss reagido de um Char.

reageKeyChar :: Char -> EstadoJogo -> EstadoJogo
reageKeyChar c ej = case verifPlay ej of
                            True -> case c of
                                    'w' -> aplicaEstJogo 1 (Movimenta C) ej
                                    'a' -> aplicaEstJogo 1 (Movimenta E) ej
                                    's' -> aplicaEstJogo 1 (Movimenta B) ej
                                    'd' -> aplicaEstJogo 1 (Movimenta D) ej
                                    'z' -> aplicaEstJogo 1 Acelera ej
                                    'x' -> aplicaEstJogo 1 Dispara ej
                                    'i' -> aplicaEstJogo 2 (Movimenta C) ej
                                    'j' -> aplicaEstJogo 2 (Movimenta E) ej
                                    'k' -> aplicaEstJogo 2 (Movimenta B) ej
                                    'l' -> aplicaEstJogo 2 (Movimenta D) ej
                                    'b' -> aplicaEstJogo 2 Acelera ej
                                    'n' -> aplicaEstJogo 2 Dispara ej
                                    'f' -> aplicaEstJogo 0 Desacelera ej
                                    'g' -> aplicaEstJogo 1 Desacelera ej
                                    'c' -> aplicaEstJogo 2 Desacelera ej
                                    'v' -> aplicaEstJogo 3 Desacelera ej
                                    '-' -> aplicaEstJogo 0 Dispara ej
                                    '.' -> aplicaEstJogo 0 Acelera ej
                                    '8' -> aplicaEstJogo 3 (Movimenta C) ej
                                    '4' -> aplicaEstJogo 3 (Movimenta E) ej
                                    '6' -> aplicaEstJogo 3 (Movimenta D) ej
                                    '2' -> aplicaEstJogo 3 (Movimenta B) ej
                                    '1' -> aplicaEstJogo 3 Acelera ej
                                    '0' -> aplicaEstJogo 3 Dispara ej
                                    'p' -> pauseGame ej
                                    oth -> ej
                            False -> ej

-- | Calcula o EstadoGloss reagido do KeySpecial.

reageKeySpecial :: SpecialKey -> EstadoJogo -> EstadoJogo
reageKeySpecial k ej = case verifPlay ej of
                       True -> case k of
                               KeyRight -> aplicaEstJogo 0 (Movimenta D) ej
                               KeyLeft  -> aplicaEstJogo 0 (Movimenta E) ej
                               KeyDown  -> aplicaEstJogo 0 (Movimenta B) ej
                               KeyUp    -> aplicaEstJogo 0 (Movimenta C) ej
                               oth      -> ej
                       False -> ej

-- | Altera o Menu consoante um evento.

reageEventoMenu :: Event -> EstadoGloss -> EstadoGloss
reageEventoMenu (EventKey (SpecialKey k) Down _ _) m = case k of
                                                       KeyEnter -> reageKeyEnter m
                                                       KeyUp    -> reageKeyUp m
                                                       KeyDown  -> reageKeyDown m
                                                       KeyLeft  -> reageKeyLeft m
                                                       KeyRight -> reageKeyRight m
                                                       KeyF1    -> reageKeyF1 m
                                                       other    -> m
reageEventoMenu _ m                                  = m

-- | Aplica uma Jogada ao 'EstadoJogo'.

aplicaEstJogo :: Int -> Jogada -> EstadoJogo -> EstadoJogo
aplicaEstJogo n jog ej | n<=nc     = ej {estado = jogada n jog (estado ej)}
                       | otherwise = ej
                       where nc    = ncontrol ej

-- | Verifica se um EstadoGloss está no Menu.

verifMenu :: EstadoGloss -> Bool
verifMenu (Menu _) = True
verifMenu _        = False

-- | Verifica se um EstadoGloss está no Pause.

verifPause :: EstadoJogo -> Bool
verifPause (Pause _ _ _ _) = True
verifPause _               = False

-- | Verifica se um EstadoGloss está no Play.

verifPlay :: EstadoJogo -> Bool
verifPlay (Play _ _ _) = True
verifPlay _            = False

-- | Adiciona uma key.

addKey :: [Maybe Key] -> Key -> [Maybe Key]
addKey l k | elem (Just k) l = l
           | otherwise       = l ++ [Just k] 

-- | Remove uma key.

removeKey :: [Maybe Key] -> Key -> [Maybe Key]
removeKey [] _            = []
removeKey ((Nothing):t) k = removeKey t k
removeKey ((Just h):t) k  | h == k = t
                          | otherwise = (Just h):removeKey t k

-- | Reseta uma keyList.

resetKeyLista :: [Maybe Key] -> [Maybe Key]
resetKeyLista [] = []
resetKeyLista ((Nothing):t) = resetKeyLista t
resetKeyLista ((Just h):t) | verificaKey h = resetKeyLista t
                           | otherwise = (Just h):resetKeyLista t

-- | Verifica se key é de movimentação ou de disparo. 

verificaKey :: Key -> Bool
verificaKey k = case k of
               (SpecialKey KeyLeft) -> True
               (SpecialKey KeyRight) -> True
               (SpecialKey KeyDown) -> True
               (SpecialKey KeyUp)   -> True
               (Char '-')           -> True
               (Char '8')           -> True
               (Char '2')           -> True
               (Char '4')           -> True
               (Char '6')           -> True
               (Char '0')           -> True
               (Char 'w')           -> True
               (Char 's')           -> True
               (Char 'k')           -> True
               (Char 'i')           -> True
               (Char 'x')           -> True
               (Char 'n')           -> True
               (Char 'f')           -> True
               (Char 'g')           -> True
               (Char 'c')           -> True
               (Char 'v')           -> True
               (Char 'a')           -> True
               (Char 'd')           -> True
               (Char 'j')           -> True
               (Char 'l')           -> True
               oth                  -> False

-- | Verifica se uma key é de aceleração.

keyDesacelera :: Key -> (Bool,Char)
keyDesacelera k = case k of
                  (Char '.') -> (True,'f')
                  (Char 'z') -> (True,'g')
                  (Char 'b') -> (True,'c')
                  (Char '1') -> (True,'v')
                  oth        -> (False,'r')


-- | Calcula o 'EstadoGloss' reagido de um evento.

reageEventoPlay :: Event -> EstadoGloss -> EstadoGloss
reageEventoPlay (EventKey k Up _ _) jogo = case verifPlay (estadoJogo jogo) of
                                           True  -> case bool of
                                                    False -> jogo {teclas = removeKey (teclas jogo) k}
                                                    True  -> jogo {teclas = addKey (removeKey (teclas jogo) k) (Char nk)}
                                           
                                           False -> jogo
                                           where (bool,nk) = keyDesacelera k

reageEventoPlay (EventKey k Down _ _) jogo = case verifPlay (estadoJogo jogo) of
                                             True  -> jogo {teclas = addKey (teclas jogo) k}
                                             False -> case verifPause (estadoJogo jogo) of
                                                      True  -> case k of
                                                               (SpecialKey KeyEnter)-> reageKeyEnter jogo
                                                               (SpecialKey KeyUp)   -> reageKeyUp jogo
                                                               (SpecialKey KeyDown) -> reageKeyDown jogo
                                                               (SpecialKey KeyF1)   -> reageKeyF1 jogo
                                                               oth                  -> jogo
                                                      False -> case verifStart (estadoJogo jogo) of
                                                               True -> jogo
                                                               False -> case k of
                                                                     (Char 'm') -> (Menu Start)
                                                                     oth        -> jogo
reageEventoPlay _ s = s


-- | Calcula o 'EstadoGloss' reagido à medida do tempo.

reageTempoPlay :: EstadoGloss -> EstadoGloss
reageTempoPlay (Jogo estJogo [])            = (Jogo estJogo [])
reageTempoPlay (Jogo estJogo ((Just k):ks)) = reageTempoPlay (Jogo estJogoNovo ks)
                                            where estJogoNovo = aplicaKey k estJogo
reageTempoPlay s                            = s

-- | Aplica uma Key ao EstadoJogo.

aplicaKey :: Key -> EstadoJogo -> EstadoJogo
aplicaKey k ej = case k of
                    (SpecialKey kesp) -> reageKeySpecial kesp ej
                    (Char c)          -> reageKeyChar c ej
                    other             -> ej

-- | Calcula o 'Jogador' mais longe.

maisLonge :: Int -> [Jogador] -> (Jogador,Int)
maisLonge n [j]      = (j,n) 
maisLonge n1 (j1:js) = comparaDist n1 n2 j1 j2 
                   
                   where (j2,n2) = (maisLonge (n1+1) js) 

-- | Compara a distância entre 2 'Jogador' es.

comparaDist :: Int -> Int -> Jogador -> Jogador -> (Jogador,Int)
comparaDist n1 n2 j1 j2 | x1 >= x2  = (j1,n1)
                        | otherwise = (j2,n2)

                    where x1 = distanciaJogador j1
                          x2 = distanciaJogador j2

-- | Transforma um 'EstadoJogo' Play em Pause.

pauseGame :: EstadoJogo -> EstadoJogo
pauseGame (Play e nc time) = (Pause e nc Resume time)
pauseGame s                = s

-- | Transforma um 'EstadoJogo' StartJ em Play.

startaJogo :: EstadoJogo -> EstadoJogo
startaJogo (StartJ e nc time) = (Play e nc time)
startaJogo s                  = s

-- | Verifica se é um 'EstadoJogo' StartJ.

verifStart :: EstadoJogo -> Bool
verifStart (StartJ _ _ _) = True
verifStart _              = False