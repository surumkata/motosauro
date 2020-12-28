-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g002 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show

-- | Um ângulo em graus.
type Angulo = Double

-- | Um ãnglulo em radianos.
type Radianos = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- | Converte um Ponto Polar em Ponto Cartesiano
converte :: Vetor -> Vetor
converte (Polar r a) = (Cartesiano x y)
                      where x = r * (cos (toRadianos a))
                            y = r * (sin (toRadianos a))
converte x = x

-- | Converte ângulos em Graus para ângulos em Radianos.
toRadianos :: Angulo -> Radianos
toRadianos a = (a*pi)/180

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores p1 p2 = aux (converte p1) (converte p2)
                    where aux :: Vetor -> Vetor -> Vetor
                          aux (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1+x2) (y1+y2))

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores p1 p2 = aux (converte p1) (converte p2)
                     where aux :: Vetor -> Vetor -> Vetor
                           aux (Cartesiano x1 y1) (Cartesiano x2 y2) = (Cartesiano (x1-x2) (y1-y2))

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor x p1 = aux x (converte p1)
                     where aux :: Double -> Vetor -> Vetor
                           aux a (Cartesiano x1 y1) = (Cartesiano (x1*a) (y1*a))

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano xa ya),(Cartesiano xb yb)) ((Cartesiano xc yc),(Cartesiano xd yd)) = (0<=t0 && t0<=1) && (0<=t1 && t1<=1)
                                                                                           where t0 = (ycd*xac+xdc*yac)/(xdc*yab-xab*ydc)
                                                                                                 t1 = (yab*xac+xba*yac)/(xdc*yab-xab*ydc)
                                                                                                 yab = ya-yb
                                                                                                 ydc = yd-yc
                                                                                                 ycd = yc-yd
                                                                                                 xab = xa-xb
                                                                                                 xba = xb-xa
                                                                                                 xdc = xd-xc
                                                                                                 xac = xa-xc
                                                                                                 yac = ya-yc

intersetam (p1,p2) (p3,p4) = intersetam ((converte p1),(converte p2)) ((converte p3),(converte p4)) 
                                                                                                    
-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano xa ya),(Cartesiano xb yb)) ((Cartesiano xc yc),(Cartesiano xd yd)) 
 | (intersetam ((Cartesiano xa ya),(Cartesiano xb yb)) ((Cartesiano xc yc),(Cartesiano xd yd)) == True) = (Cartesiano x y)                          
 |otherwise = error "As retas não se intersetam"
                                                                                           where x = (b2 - b1)/(m1 - m2)
                                                                                                 y = m1*x + b1
                                                                                                 m1 = (yb-ya)/(xb-xa)
                                                                                                 m2 = (yc-yd)/(xc-xd)
                                                                                                 b1 = ya - m1*xa
                                                                                                 b2 = yc - m2*xc
intersecao (p1,p2) (p3,p4) = intersecao ((converte p1),(converte p2)) ((converte p3),(converte p4))

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x l = if (x>((length l)-1) || x<0) then False else True 

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [] = (0,0)
dimensaoMatriz ([]:s) = (0,0)
dimensaoMatriz matriz = (length matriz, length (head matriz))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (x,y) [] = False
ePosicaoMatrizValida (x,y) matriz = if ((x+1>(length matriz))) || (y+1>((length (head matriz)))) || (x<0) || (y<0) then False else True

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo x | (x >= 0) && (x < 360) = x
                  | x < 0 = normalizaAngulo (x+360)
                  | x >= 360 = normalizaAngulo (x-360)
-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista n l = l!!n

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista a x l = hs++(x:ts)
                          where (hs,t:ts) = splitAt a l

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (l,c) m = encontraIndiceLista c (encontraIndiceLista l m)

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (l,c) x m = atualizaIndiceLista l nl m
                                where ol = encontraIndiceLista l m
                                      nl = atualizaIndiceLista c x ol


