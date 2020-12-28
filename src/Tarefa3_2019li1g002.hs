-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g002 where

import LI11920
import LITestes_002

{-
= Introdução

  Neste módulo estão contidas as funções referentes à construção de um mapa, executada por um bulldozer.
  O bulldozer cumpre esta tarefa através das instruções que lhe são fornecidas.
  
= Objetivos

  A base desta solução passa por transformar o mapa numa sequência de instruções básica, ineficaz e repetitiva (desconstroiMapa/Pista/Peca), 
mas que facilita a restante resolução.
  A esta primeira lista de instruções são aplicados diferentes processos:
    A procura de padrões que se repitam horizontalmente, ou seja, na mesma pista(repeteHorPad).
    A procura de padrões verticais, ou seja, peças que se repitam num mesmo índice em várias pistas distintas 
    Esta é sempre seguida da procura de padrões horizontais, para minimizar o número de instruções usadas, pois no pior dos casos fica igual.
    Por vezes a existência de padrões verticais impossibilita que certos padrões horizontais sejam descobertos, devido à alteracão de certas 
instruções e é por isso usada uma comparação (comparaComp) entre o número de instruções usadas com e sem os padrões verticais, sendo escolhida 
a opção comprimida.

= Conclusão

  A taxa de compressão obtida foi bastante aceitável.
  Esta deve-se em grande parte ao facto de sermos capazes de encontrar padroẽs mistos, ao invés de padrões apenas com a mesma peça repetida.
  A pequena comparação no final contribui também que a compressão final é sempre a melhor de que a nossa tarefa é capaz.
  Concluindo, o resultado obtido nesta tarefa foi satisfatório, sendo que o único problema é não termos sido capazes de implementar a teleporta de
forma útil.
-}

-- * Testes.

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = testes3

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.

desconstroi :: Mapa -- ^ O 'Mapa'.
            -> Instrucoes -- ^ 'Mapa' tranformado em 'Instrucoes'.
desconstroi m = comparaComp horAposVert hor
 
              where hor         = repeteHorPad 1 [] inst
                    horAposVert = repeteHorPad 1 [] vert
                    vert        = repeteVert inst
                    inst        = desconstroiMapa 0 (tailmapa m)

-- | Compara duas 'Instrucoes' através da função 'tamanhoInstrucoes' e diz qual a mais comprimida.

comparaComp :: Instrucoes -- ^ 'Instrucoes' 1.
            -> Instrucoes -- ^ 'Instrucoes' 2.
            -> Instrucoes -- ^ As 'Instrucoes' mais comprimidas.
comparaComp l1 l2 | (tamanhoInstrucoes l1) > (tamanhoInstrucoes l2) = l2
                  | otherwise                                       = l1

-- | Retira ao 'Mapa' as primeiras 'Peca' de todas as 'Pista'.

tailmapa :: Mapa -- ^ O 'Mapa'.
         -> Mapa -- ^ O 'Mapa' sem a 1º 'Peca' de todas as 'Pistas'.
tailmapa []    = []
tailmapa (h:t) = [tail h] ++ tailmapa t

-- | Dá uma sequência de 'Instrucoes' básicas de um 'Mapa'.

desconstroiMapa :: Int -- ^ Contador (Nº da Pista|Começa sempre com 0).
                -> Mapa -- ^ O 'Mapa'.
                -> Instrucoes -- ^ 'Mapa' tranformado em 'Instrucoes' básicas.
desconstroiMapa _ []      = []
desconstroiMapa npi (h:t) = (desconstroiPista npi h) ++ (desconstroiMapa (npi+1) t)

-- | Dá uma sequência de 'Instrucoes' básicas de uma 'Pista'.

desconstroiPista :: Int -- ^ Nº da Pista.
                 -> Pista -- ^ A 'Pista'.
                 -> Instrucoes -- ^ 'Pista' tranformada em 'Instrucoes' básicas.
desconstroiPista _ []      = []
desconstroiPista npi (h:t) = [desconstroiPeca npi h] ++ (desconstroiPista (npi) t)

-- | Dá uma sequência de 'Instrucoes' básicas de uma 'Peca'

desconstroiPeca :: Int -- ^ Nº da Pista.
                -> Peca -- ^ A 'Peca'.
                -> Instrucao -- ^ 'Peca' transforma em 'Instrucao'.
desconstroiPeca npi (Recta p y)                 = Anda [npi] p
desconstroiPeca npi (Rampa p yi yf) | yf > yi   = Sobe [npi] p (yf-yi)
                                    | otherwise = Desce [npi] p (yi-yf)


-- | Comprime 'Instrucoes' horizontalmente utilizando do Repete.

repeteHorPad :: Int -- ^ Contador (Repetes|Começa sempre com 1).
             -> Instrucoes -- ^ Padrão 'Instrucoes' (Começa com []).
             -> Instrucoes -- ^ As 'Instrucoes' para serem comprimidas.
             -> Instrucoes -- ^ 'Instrucoes' comprimidas horizontalmente.
repeteHorPad 1 [] (h:t)                              = repeteHorPad 1 [h] t
repeteHorPad n padrao []                             = padrao
repeteHorPad n padrao inst | ver1 && notpad2 == []   = notpad1++repeteHorPad (n+1) pad1 (deletaInst pad1 inst)
                           | ver1                    = notpad1++[Repete (n+1) pad1]++repeteHorPad 1 [] (deletaInst pad1 inst)
                           | n==1                    = repeteHorPad 1 (notpad1++[head inst]) ((tail inst)++pad1)
                           where (notpad1,ver1,pad1) = achaPadrao padrao inst
                                 (notpad2,ver2,pad2) = achaPadrao pad1 (deletaInst pad1 inst)

-- | Verifica se alguma parte de um determinado padrão de 'Instrucoes' existe numas 'Instrucoes'

achaPadrao :: Instrucoes -- ^ Padrão 'Instrucoes'.
           -> Instrucoes -- ^ As 'Instrucoes' para verificar se existe o Padrão nelas.
           -> (Instrucoes,Bool,Instrucoes) -- ^ (Parte do Padrão que não existe nas 'Instrucoes',True se existe|False se não, Parte do Padrão que existe nas 'Instrucoes')
achaPadrao [h] (h1:t)      | h==h1                           = ([],True,[h])
                           | otherwise                       = ([h],False,[])
achaPadrao l []                                              = (l,False,[])
achaPadrao (h1:t1) (h2:t2) | h1==h2 && verif1 && notpad1==[] = ([],True,(h1:t1))
                           | h1/=h2 && verif2                = ([h1]++notpad2,True,pad2)
                           | otherwise                       = ((h1:t1),False,[])
                           where (notpad1,verif1,pad1)       = achaPadrao t1 t2
                                 (notpad2,verif2,pad2)       = achaPadrao t1 (h2:t2)

-- | Pega numas 'Instrucoes' e retira-lhe um determinado padrão de 'Instrucoes'.

deletaInst :: Instrucoes -- ^ Padrão 'Instrucoes'.
           -> Instrucoes -- ^ As 'Instrucoes' para remover o Padrão nelas.
           -> Instrucoes -- ^ As 'Instrucoes' depois de remover o Padrão.
deletaInst [] l             = l
deletaInst (pad:pads) (h:t) = deletaInst pads t

-- | Comprime 'Instrucoes' verticalmente.

repeteVert :: Instrucoes -- ^ 'Instrucoes' anterior.
           -> Instrucoes -- ^ 'Instrucoes' comprimidas verticalmente.
repeteVert is = colapsa (transposta 0 (divideP 0 is))

-- | Faz transposta de uma matriz 'Instrucoes'

transposta :: Int -- ^ Contador (Posição|Começa sempre no 0)
           -> [Instrucoes] -- ^ Lista de 'Instrucoes' anterior.
           -> [Instrucoes] -- ^ A transposta da lista de 'Instrucoes'.
transposta n l  | n <= (length(head l))-1 = daElementoPista n l:transposta (n+1) l
                | otherwise               = []

-- | Divide 'Instrucoes' numa matriz de 'Instrucoes' dividida pelo número de pistas.

divideP :: Int -- ^ Contador (Nº de 'Pistas'|Começa sempre no 0).
        -> Instrucoes -- ^ As 'Instrucoes'.
        -> [Instrucoes] -- ^ Matriz de 'Instrucoes' divida por 'Pistas'.
divideP _ [] = []
divideP npi l  = (filtro npi l):divideP (npi+1) (deletaInst (filtro npi l) l)

-- | Filtra as 'Instrucoes' de uma determinada 'Pista'.

filtro :: Int -- ^ Nº da Pista.
       -> Instrucoes -- ^ As 'Instrucoes'.
       -> Instrucoes -- ^ As 'Instrucoes' apenas da 'Pista' pedida.
filtro x []                   = []
filtro x (h:t) | verifNPI x h = h:filtro x t  
               | otherwise    = filtro x t

-- | Verifica se uma determinada 'Instrucao' é de uma determinada 'Pista'.

verifNPI :: Int -- ^ Nº da Pista.
         -> Instrucao -- ^ A 'Instrucao'.
         -> Bool -- ^ True se a 'Instrucao' é da Pista pedida | False se não é.
verifNPI npi (Anda [pi] p)    = npi == pi
verifNPI npi (Sobe [pi] p a)  = npi == pi
verifNPI npi (Desce [pi] p a) = npi == pi


-- | Junta todas as 'Instrucao' de uma determinada posicao de todas as 'Instrucoes' de uma matriz de 'Instrucoes'.

daElementoPista :: Int -- ^ Posição.
                -> [Instrucoes] -- ^ Matriz de 'Instrucoes'.
                -> Instrucoes -- ^ Todas as 'Instrucao' na posição dada, de todas as 'Instrucoes' de uma matriz.
daElementoPista _  []   = []
daElementoPista n (h:t) = (h!!n):daElementoPista n t 

-- | Junta todas as 'Instrucoes' de uma matriz de 'Instrucoes'.

colapsa :: [Instrucoes] -- ^ Matriz de 'Instrucoes'.
        -> Instrucoes -- ^ Junção de todas as 'Instrucoes' da matriz.
colapsa []    = []
colapsa (h:t) = comprimeV h ++ colapsa t

-- | Comprime verticalmente todas as 'Instrucao' iguais de uma lista de 'Instrucoes'.

comprimeV :: Instrucoes -- ^ As 'Instrucoes'.
          -> Instrucoes -- ^ As 'Instrucoes' comprimidas verticalmente.

comprimeV []    = []
comprimeV (h:t) = yes:comprimeV not
                
                where (not,yes) = comparaInstV h t

-- | Compara uma 'Instrucao' com 'Instrucoes' e dá-me (As 'Instrucoes' difrentes, A 'Instrucao' comprimida).

comparaInstV :: Instrucao -- ^ A 'Instrucao' "modelo".
             -> Instrucoes -- ^ As 'Instrucoes'
             -> (Instrucoes,Instrucao) -- ^ (As 'Instrucoes' difrentes, A 'Instrucao' comprimida).

comparaInstV i1 []                                                          = ([],i1)

comparaInstV (Anda np1 p1) ((Anda np2 p2):t)         | p1 == p2             = comparaInstV (Anda (np1++np2) p1) t
                                                     | otherwise            = (((Anda np2 p2):not),yes)

                                                     where (not,yes)        = comparaInstV (Anda np1 p1) t

comparaInstV (Sobe np1 p1 y1) ((Sobe np2 p2 y2):t)   | p1 == p2 && y1 == y2 = comparaInstV (Sobe (np1++np2) p1 y1) t
                                                     | otherwise            = (((Sobe np2 p2 y2):not),yes)

                                                     where (not,yes)        = comparaInstV (Sobe np1 p1 y1) t

comparaInstV (Desce np1 p1 y1) ((Desce np2 p2 y2):t) | p1 == p2 && y1 == y2 = comparaInstV (Desce (np1++np2) p1 y1) t
                                                     | otherwise            = (((Desce np2 p2 y2):not),yes)

                                                     where (not,yes)        = comparaInstV (Desce np1 p1 y1) t

comparaInstV i1 (h:t)                                                       = ((h:not),yes)

                                                     where (not,yes)        = comparaInstV i1 t