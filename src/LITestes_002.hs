-- | Este módulo define os testes das Tarefas 1,2 e 3 do trabalho prático.
module LITestes_002 where

import LI11920

-- *Testes para a Tarefa 1.

-- | Lista com testes para efetuar na Tarefa 1.
testes1 :: [(Int,Int,Int)]
testes1 = [(1,2,3),(3,2,1),(1,1,3),
           (4,6,7),(1,5,7),(1,3,(-2)),
           (2,8,2),(5,2,2),(4,2,1),
           (3,1,3),(2,6,7),(3,5,8),
           (2,3,(-2)),(5,8,2),(2,3,7),
           (9,4,5),(0,4,(-5)),(3,12,23),
           (5,7,(-3)),(3,8,3),(22,5,90),
           (3,5,8),(1,9,10),(7,1,9)]


-- *Testes para a Tarefa 2.

-- | Lista com todas as possibilidades entre as Jogadas Testes Possíveis e o Estado Teste 1 para testar na Tarefa 2.
testes2 :: [(Int,Jogada,Estado)]
testes2 = [(0,j1,e1),(1,j1,e1),(2,j1,e1),(3,j1,e1),(4,j1,e1),(5,j1,e1),
           (0,j2,e1),(1,j2,e1),(2,j2,e1),(3,j2,e1),(4,j2,e1),(5,j2,e1),
           (0,j3,e1),(1,j3,e1),(2,j3,e1),(3,j3,e1),(4,j3,e1),(5,j3,e1),
           (0,j4,e1),(1,j4,e1),(2,j4,e1),(3,j4,e1),(4,j4,e1),(5,j4,e1),
           (0,j5,e1),(1,j5,e1),(2,j5,e1),(3,j5,e1),(4,j5,e1),(5,j5,e1),
           (0,j6,e1),(1,j6,e1),(2,j6,e1),(3,j6,e1),(4,j6,e1),(5,j6,e1),
           (0,j7,e1),(1,j7,e1),(2,j7,e1),(3,j7,e1),(4,j7,e1),(5,j7,e1)]

-- | Lista com todas as possibilidades entre as Jogadas Testes Possíveis e o Estado Teste 9 para testar na Tarefa 2.
testes2' :: [(Int,Jogada,Estado)]
testes2' = [(0,j1,e9),(1,j1,e9),(2,j1,e9),(3,j1,e9),(4,j1,e9),(5,j1,e9),(6,j1,e9),(7,j1,e9),(8,j1,e9),(9,j1,e9),
            (0,j2,e9),(1,j2,e9),(2,j2,e9),(3,j2,e9),(4,j2,e9),(5,j2,e9),(6,j2,e9),(7,j2,e9),(8,j2,e9),(9,j2,e9),
            (0,j3,e9),(1,j3,e9),(2,j3,e9),(3,j3,e9),(4,j3,e9),(5,j3,e9),(6,j3,e9),(7,j3,e9),(8,j3,e9),(9,j3,e9),
            (0,j4,e9),(1,j4,e9),(2,j4,e9),(3,j4,e9),(4,j4,e9),(5,j4,e9),(6,j4,e9),(7,j4,e9),(8,j4,e9),(9,j4,e9),
            (0,j5,e9),(1,j5,e9),(2,j5,e9),(3,j5,e9),(4,j5,e9),(5,j5,e9),(6,j5,e9),(7,j5,e9),(8,j5,e9),(9,j5,e9),
            (0,j6,e9),(1,j6,e9),(2,j6,e9),(3,j6,e9),(4,j6,e9),(5,j6,e9),(6,j6,e9),(7,j6,e9),(8,j6,e9),(9,j6,e9),
            (0,j7,e9),(1,j7,e9),(2,j7,e9),(3,j7,e9),(4,j7,e9),(5,j7,e9),(6,j7,e9),(7,j7,e9),(8,j7,e9),(9,j7,e9)]



--  *Testes para a Tarefa 3.
-- | Lista com 10 testes para a Tarefa 3.

testes3 :: [Mapa]
testes3 = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15]

-- *Testes para a Tarefa 4.

-- | Lista com testes para a Tarefa 4, compostos por 1 'Double' (tempo), 1 'Mapa' e 1 'Jogador'.

testes4 :: [(Double,Mapa,Jogador)]
testes4 = [(0.5,m1,(js1!!0)),(0.6,m1,(js1!!1)),(0.5,m1,(js1!!2)),(0.5,m1,(js1!!3)),(0.5,m2,(js2!!1)),
           (0.7,m1,(js1!!4)),(0.8,m1,(js1!!5)),(1.1,m9,(js9!!8)),(0.3,m9,(js9!!2)),(0.9,m9,(js9!!0)),
           (0.5,m9,(js9!!7)),(0.7,m2,(js2!!2)),(0.8,m2,(js2!!0)),(0.6,m7,(js7!!0)),(0.9,m7,(js7!!1)),
           (0.5,m7,(js7!!2)),(0.3,m7,(js7!!3))]

-- *Estados Testes.

-- | Estado para o Jogo.

eJogo = Estado m11 jsJogo

-- | Estado Teste 1 (Inlcui o Mapa Teste 1 e a Lista de Jogadores Teste 1).

e1 = Estado m2 js1  

-- | Estado Teste 9 (Inlcui o Mapa Teste 9 e a Lista de Jogadores Teste 9).

e9 = Estado m9 js9

-- *Mapas Testes.

-- | Mapa Teste 1.

m1 = [[Recta Terra 0,Rampa Terra 0 1,Recta Lama 1,Rampa Boost 1 0,Recta Lama 0],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0],
      [Recta Terra 0,Rampa Boost 0 1,Recta Terra 1, Rampa Terra 1 0, Recta Lama 0],
      [Recta Terra 0,Rampa Lama 0 1,Recta Boost 1,Rampa Terra 1 0,Rampa Terra 0 2],
      [Recta Terra 0,Rampa Cola 0 1,Recta Terra 1, Rampa Terra 1 0, Recta Lama 0],
      [Recta Terra 0,Rampa Boost 0 2,Rampa Terra 2 0,Recta Lama 0,Recta Boost 0]]

-- | Mapa Teste 2.

m2 = [[Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 3,Recta Terra 3,Rampa Terra 3 1],
      [Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Recta Relva 1,Rampa Terra 1 0],
      [Recta Terra 0,Recta Cola 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]]

-- | Mapa Teste 3.

m3 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0],
      [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0],
      [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
      [Recta Terra 0,Rampa Terra 0 1,Recta Relva 1,Rampa Terra 1 0],
      [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0],
      [Recta Terra 0,Rampa Terra 0 1,Recta Relva 1,Recta Relva 1]]

-- | Mapa Teste 4.

m4 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0],
      [Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0],
      [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
      [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],
      [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0]]

-- | Mapa Teste 5.

m5 = [[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0],
      [Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Terra 0]]

-- | Mapa Teste 6.

m6 = [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
      [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
      [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
      [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
      [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
      [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0]]

-- | Mapa Teste 7.

m7 = [[Recta Terra 0,Rampa Relva 0 1,Rampa Cola 1 3,Rampa Lama 3 1,Rampa Boost 1 2,Rampa Relva 2 3, Recta Boost 3, Rampa Terra 3 4, Rampa Relva 4 1, Rampa Terra 1 0]]


-- | Mapa Teste 8.

m8 = [[Recta Terra 0,Recta Boost 0,Recta Lama 0,Recta Lama 0, Recta Boost 0, Recta Lama 0, Recta Boost 0, Recta Relva 0],
      [Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Boost 0, Recta Boost 0, Recta Lama 0, Recta Boost 0, Recta Boost 0],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0, Recta Boost 0, Recta Boost 0, Recta Boost 0, Recta Boost 0],
      [Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0, Recta Boost 0, Recta Lama 0, Recta Boost 0, Recta Relva 0]]

-- | Mapa Teste 9.

m9 = [[Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Boost 0,Rampa Boost 0 1,Recta Lama 1,Recta Terra 1,Rampa Relva 1 3,Recta Terra 3],
      [Recta Terra 0,Recta Boost 0,Rampa Boost 0 1,Rampa Boost 1 3,Rampa Boost 3 5,Recta Relva 5,Rampa Relva 5 3,Recta Relva 3,Rampa Relva 3 1],
      [Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 4,Rampa Terra 4 2],
      [Recta Terra 0,Rampa Terra 0 2,Recta Relva 2,Recta Relva 2,Rampa Lama 2 3,Rampa Lama 3 2,Recta Terra 2,Recta Terra 2,Recta Terra 2],
      [Recta Terra 0,Recta Relva 0,Rampa Relva 0 2,Rampa Terra 2 4,Rampa Terra 4 2,Recta Lama 2,Recta Relva 2,Rampa Terra 2 1,Rampa Terra 1 2],
      [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 2,Rampa Relva 2 3,Rampa Relva 3 4,Rampa Relva 4 2],
      [Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Recta Terra 2,Rampa Relva 2 4,Rampa Relva 4 5,Recta Lama 5,Rampa Lama 5 3,Recta Lama 3],
      [Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 2,Rampa Relva 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Lama 2 4],
      [Recta Terra 0,Rampa Terra 0 1,Recta Relva 1,Rampa Relva 1 3,Rampa Relva 3 1,Recta Relva 1,Recta Boost 1,Recta Terra 1,Rampa Terra 1 3],
      [Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Rampa Terra 1 0,Recta Terra 0,Rampa Terra 0 1,Recta Terra 1,Recta Terra 1,Rampa Terra 1 0]]

-- | Mapa Teste 10.

m10 = [[Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0],
       [Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 3,Recta Terra 3,Recta Lama 3],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]]

-- | Mapa Teste 11.

m11 = [[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Rampa Boost 0 1,Recta Boost 1],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Cola 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Cola 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Rampa Boost 0 1,Recta Boost 1],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Cola 0,Recta Terra 0,Recta Relva 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Rampa Boost 0 1,Recta Boost 1], 
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Rampa Boost 0 1,Recta Boost 1]]
-- | Mapa Teste 12

m12 = [[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 2,Recta Relva 2,Recta Relva 2],
       [Recta Terra 0,Recta Boost 0,Rampa Boost 0 1,Rampa Relva 1 0,Recta Relva 0,Recta Relva 0,Recta Terra 0]]

-- | Mapa Teste 13

m13 = [[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Lama 0,Recta Lama 0],
       [Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Relva 0],
       [Recta Terra 0,Recta Lama 0,Recta Lama 0,Rampa Lama 0 2,Rampa Boost 2 0,Recta Boost 0],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0]]

-- | Mapa Teste 14

m14 = [[Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 1,Rampa Terra 1 3,Rampa Lama 3 4,Rampa Lama 4 5,Recta Terra 5],
       [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Boost 0,Recta Terra 0,Recta Terra 0],
       [Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Recta Relva 1]]

-- | Mapa Teste 15

m15 = [[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],
       [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],
       [Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Terra 0 1],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],
       [Recta Terra 0,Rampa Relva 0 2,Recta Relva 2,Recta Relva 2],
       [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0],
       [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0]]

-- | 1º Mapa Pre-defenido usado no Jogo.

mJogo1 :: Mapa
mJogo1 = [[Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Relva 2 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Lama 0,Recta Boost 0,Rampa Lama 0 1,Recta Boost 1],
          [Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Cola 0,Recta Relva 0,Rampa Terra 0 1,Rampa Lama 1 0,Recta Relva 0,Recta Terra 0,Recta Lama 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Relva 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Relva 1 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Recta Boost 0,Recta Lama 0,Rampa Boost 0 1,Recta Lama 1],
          [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Cola 0,Recta Relva 0,Recta Relva 0,Rampa Boost 0 1,Rampa Terra 1 0,Recta Lama 0,Recta Relva 0,Recta Boost 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Lama 0,Recta Relva 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Boost 0,Recta Lama 0,Recta Boost 0,Rampa Lama 0 1,Recta Boost 1], 
          [Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Relva 0 2,Rampa Lama 2 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Boost 0,Recta Lama 0,Rampa Boost 0 1,Recta Lama 1]]

-- | 2º Mapa Pre-defenido usado no Jogo.

mJogo2 :: Mapa
mJogo2 = [[Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Relva 0,Rampa Relva 0 2,Rampa Lama 2 0,Recta Relva 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Lama 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Relva 0,Rampa Lama 0 2,Rampa Relva 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Terra 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Lama 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Lama 0,Rampa Lama 0 2,Rampa Relva 2 0,Recta Terra 0,Recta Lama 0,Rampa Lama 0 1,Recta Terra 1],
          [Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Boost 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Lama 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Relva 0,Rampa Lama 0 2,Rampa Lama 2 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Relva 0,Recta Terra 0,Rampa Relva 0 1,Recta Terra 1],
          [Recta Terra 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Terra 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Relva 0,Rampa Lama 0 2,Rampa Relva 2 0,Recta Relva 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Relva 0,Rampa Boost 0 2,Rampa Lama 2 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Relva 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Lama 0,Recta Boost 0,Rampa Relva 0 1,Recta Terra 1],
          [Recta Terra 0,Rampa Terra 0 1,Rampa Relva 1 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Lama 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Boost 0,Rampa Relva 0 2,Rampa Relva 2 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Terra 0,Rampa Terra 0 2,Rampa Lama 2 0,Recta Relva 0,Rampa Boost 0 2,Rampa Terra 2 0,Recta Terra 0,Rampa Relva 0 2,Rampa Lama 2 0,Recta Boost 0,Recta Relva 0,Rampa Terra 0 1,Recta Terra 1]]

-- | 4º Mapa Pre-defenido usado no Jogo.

mJogo4 :: Mapa
mJogo4 = [[Recta Terra 0,Rampa Lama 0 1,Recta Lama 1,Recta Lama 1,Recta Lama 1,Rampa Lama 1 0,Recta Relva 0,Rampa Lama 0 1,Recta Lama 1,Rampa Lama 1 0,Recta Boost 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Recta Terra 1,Recta Boost 1,Recta Boost 1,Recta Boost 1,Recta Boost 1],
          [Recta Terra 0,Rampa Lama 0 1,Recta Terra 1,Recta Lama 1,Recta Terra 1,Rampa Lama 1 0,Recta Boost 0,Rampa Lama 0 1,Recta Relva 1,Rampa Lama 1 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Relva 0,Recta Boost 0,Recta Lama 0,Recta Relva 0,Recta Lama 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0],       
          [Recta Terra 0,Rampa Lama 0 1,Recta Relva 1,Recta Boost 1,Recta Relva 1,Rampa Lama 1 0,Recta Relva 0,Rampa Lama 0 1,Recta Terra 1,Rampa Lama 1 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0],
          [Recta Terra 0,Rampa Lama 0 1,Recta Terra 1,Recta Terra 1,Recta Terra 1,Rampa Lama 1 0,Recta Terra 0,Rampa Lama 0 1,Recta Lama 1,Rampa Lama 1 0,Recta Terra 0,Recta Boost 0,Recta Lama 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0]]

-- | 3º Mapa Pre-defenido usado no Jogo.

mJogo3 :: Mapa
mJogo3 = [ [Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Boost 0,Recta Lama 0,Rampa Relva 0 1,Rampa Terra 1 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,
         Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Rampa Relva 1 0,Recta Boost 0,Recta Lama 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,
         Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Recta Relva 1,Recta Lama 1,Rampa Lama 1 2,Recta Relva 2,Recta Boost 2,Rampa Lama 2 3,Rampa Lama 3 2,Recta Terra 2,Recta Terra 2,Recta Terra 2],
        [Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Boost 0,Rampa Boost 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Relva 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Rampa Terra 1 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,
         Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,
         Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Boost 0 1,Recta Relva 1,Recta Boost 1,Rampa Terra 1 2,Recta Relva 2,Recta Boost 2,Rampa Lama 2 3,Rampa Lama 3 2,Recta Relva 2,Recta Lama 2,Recta Terra 2],
        [Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 1,Rampa Lama 1 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Relva 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Rampa Terra 1 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,
         Recta Relva 0,Recta Lama 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Relva 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Rampa Terra 1 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,
         Recta Relva 0,Recta Relva 0,Recta Lama 0,Recta Relva 0,Rampa Relva 0 1,Recta Relva 1,Recta Terra 1,Rampa Terra 1 2,Recta Relva 2,Recta Relva 2,Rampa Lama 2 3,Rampa Lama 3 2,Recta Relva 2,Recta Terra 2,Recta Terra 2],
        [Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Rampa Lama 0 1,Rampa Terra 1 0,Recta Lama 0,Recta Relva 0,Recta Terra 0,Rampa Lama 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 1,Rampa Relva 1 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Rampa Terra 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,
         Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Boost 0,Recta Boost 0,Recta Terra 0,Rampa Terra 0 1,Rampa Terra 1 0,Recta Boost 0,Recta Lama 0,Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Terra 0 1,Rampa Relva 1 0,Recta Terra 0,Recta Relva 0,Recta Terra 0,Rampa Relva 0 2,Rampa Terra 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,
         Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Relva 0,Rampa Relva 0 1,Recta Relva 1,Recta Lama 1,Rampa Terra 1 2,Recta Relva 2,Recta Relva 2,Rampa Lama 2 3,Rampa Lama 3 2,Recta Terra 2,Recta Relva 2,Recta Terra 2]]






-- *Listas de Jogadores Testes.

-- | Lista de Jogadores Teste 1.

js1 :: [Jogador]
js1 = [jog1,jog2,jog3,jog4,jog5,jog6]
    where jog1 = Jogador 0 0.8 1.15 5 (Chao True)
          jog2 = Jogador 1 0.8 1 5 (Chao True)
          jog3 = Jogador 2 2.1 1.5 5 (Ar 1.3 (-77) 0)
          jog4 = Jogador 3 2.8 1.3 4 (Chao True)
          jog5 = Jogador 4 1.7 0.4 5 (Chao True)
          jog6 = Jogador 5 1.4 0.3 0 (Chao True)

-- | Lista de Jogadores Teste 2.

js2 = [jog1,jog2,jog3]
    where jog1 = Jogador 0 0 0 4 (Chao False)
          jog2 = Jogador 1 1.9 0.7 5 (Chao True)
          jog3 = Jogador 2 1.5 0 5 (Morto 0.5)

-- | Lista de Jogadores Teste 7.

js7 = [jog1,jog2,jog3,jog4]
    where jog1 = Jogador 0 3.1 0.8 3 (Ar 3.1 45 0)
          jog2 = Jogador 0 3.7 0.2 3 (Chao False)
          jog3 = Jogador 0 3.9 1 4 (Ar 1.5 40 1.0)
          jog4 = Jogador 0 3.2 0.8 3 (Ar 3.3 (-65) 0.5)


-- | Lista de Jogadores Teste 9.

js9 = [jog1,jog2,jog3,jog4,jog5,jog6,jog7,jog8,jog9,jog10]
    where jog1 = Jogador 0 4.3 1.2 0 (Chao True)
          jog2 = Jogador 1 3.5 2.3 1 (Chao False)
          jog3 = Jogador 2 5.2 1.1 3 (Chao True)
          jog4 = Jogador 3 2 0 5 (Morto 1.0)
          jog5 = Jogador 4 2.2 2.1 2 (Ar 2.5 23 0.6)
          jog6 = Jogador 5 3.3 1 0 (Chao False)
          jog7 = Jogador 6 5.4 2.3 2 (Chao True)
          jog8 = Jogador 7 2.3 0 1 (Morto 1.0)
          jog9 = Jogador 8 4.5 0.5175 4 (Ar 2.8 (-45) 0.6)
          jog10 = Jogador 9 5.6 0.8 3 (Chao False)

-- | Lista de Jogadores para o Jogo.

jsJogo = [jog1,jog2,jog3,jog4]
    where jog1 = Jogador 0 0 0 4 (Chao False)
          jog2 = Jogador 1 0 0 4 (Chao False)
          jog3 = Jogador 2 0 0 4 (Chao False)
          jog4 = Jogador 3 0 0 4 (Chao False)    

-- *Jogadas Testes Possíveis.

-- | Jogada Teste Acelera.
j1 = Acelera

-- | Jogada Teste Desacelera.
j2 = Desacelera

-- | Jogada Movimenta B.
j3 = Movimenta B

-- | Jogada Movimenta C.
j4 = Movimenta C

-- | Jogada Movimenta D.
j5 = Movimenta D

-- | Jogada Movimenta E.
j6 = Movimenta E

-- | Jogada Dispara.
j7 = Dispara