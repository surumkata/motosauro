# Projeto de Laboratórios de Informática 1 (LI1) - Motosauro - Recriação do jogo "ExciteBike"- Ano letivo de 2019/2020

Excitebike é um jogo de corrida de motocross desenvolvido pela Nintendo. Foi lançado no Japão para o Family Computer em 1984 e foi um dos títulos de lançamento para o NES em 1985.

O objetivo deste projeto de **Laboratórios de Informática 1** era a recriação desse jogo, utilizando a linguagem de programação **Haskell**, assim, colocando em prática os ensinamentos obtidos ao longo das diferentes cadeiras do semestre, como **Programação Informal**.  Por conta das mudanças gráficas decidimos apelidar a nossa nova versão de **"Motosauro"**.

Este trabalho foi realizado por Tiago Silva e Hugo Fernandes, estudantes do 1º ano de engenharia informática da Universidade do Minho no ano letivo de 2019/2020.

## Jogabilidade

No Motosauro as corridas são de sempre 4 motas, pudendo elas serem controladas por jogadores em tela compartilhada e/ou por bots.
O jogo é simples, o jogador pode movimentar a moto entre pistas (**cima** e **baixo**), **acelerar** e **desacelerar**, inclinar a moto enquanto estiver no ar (**esquerda** e **direita**), e por fim, pode ainda **disparar** **cola** (4 munições por corrida) para o piso atrás dele de modo a atrasar as outras motas. De realçar que, se o jogador cai do ar numa inclinação muito torta, este fica **morto** durante alguns segundos.
Por falar em pisos, o jogo conta com 5 tipos de pisos diferentes, que servem para alterar a velocidade da mota. Do piso mais lento para o mais rápido são: **Cola, Lama, Relva, Terra e Boost**
Finalizando, o jogo conta ainda com 2 tipos de mapas, os **padrões**, criados por nós, e os **gerados** que necessitam de uma **seed** e um **tamanho**.

## Parte Gráfica

### Menu inicial do jogo
![menu1](https://user-images.githubusercontent.com/57015073/191846460-a72741d2-11bf-4511-8422-035e65ad3734.gif)
### Menu dos controlos
![men4](https://user-images.githubusercontent.com/57015073/191835109-abf92924-0ab2-4291-858d-314f401bb86f.png)
### Menu de escolha de tipos de mapas
![menu2](https://user-images.githubusercontent.com/57015073/191846465-e8460021-9b26-4513-8528-bdcaae0f0473.gif)
### Menu de escolha de (default) mapa
![menu 4](https://user-images.githubusercontent.com/57015073/191846470-a5464980-c1aa-4ffa-b43c-c4ad99205586.gif)
### Menu de escolha da quantidade de jogadores
![menu3](https://user-images.githubusercontent.com/57015073/191846468-5a973e1c-a23c-46e8-998c-74bd90b90c81.gif)
### Decorrer de uma corrida
![game on going](https://user-images.githubusercontent.com/57015073/191835897-4e4e8333-1174-4b78-957b-0ef616d87577.png)
### Fim de uma corrida
![Sem Título](https://user-images.githubusercontent.com/57015073/191842314-612d62c3-30a3-4964-9106-c79399d9ad12.png)
### Motos e corpo morto
![mota](https://user-images.githubusercontent.com/57015073/191842538-12dd76be-c368-4418-8e10-d3c4d1ed6e16.png)![mota2](https://user-images.githubusercontent.com/57015073/191842540-cc1f2a31-adb7-4c81-8555-49862e960aa8.png)![mota3](https://user-images.githubusercontent.com/57015073/191842543-b77f9d0d-82e2-4f37-be05-31cbb89ce2e0.png)![mota4](https://user-images.githubusercontent.com/57015073/191842545-6e3b4fe7-b4dd-4d8f-abf5-98eb570aee33.png)![morto](https://user-images.githubusercontent.com/57015073/191842826-0d1fa6a2-b57f-49d8-ae4f-19297a5f6f80.png)
### Pisos
#### Terra:
![LayerStart](https://user-images.githubusercontent.com/57015073/191843692-6fdbfcc7-644a-4785-a821-c2eeef3f997c.png) ![LayerRectaTerra](https://user-images.githubusercontent.com/57015073/191843690-29cc72d6-3577-4ad1-8f9a-325de5aef8a8.png)  ![LayerRampaTerra](https://user-images.githubusercontent.com/57015073/191843712-b1a61c7a-95f9-4e86-b620-2f78cc882e7d.png) ![LayerUnderTerra](https://user-images.githubusercontent.com/57015073/191843702-b53598e0-7e5c-4ab8-9474-ece459aef02b.png) ![LayerFinish](https://user-images.githubusercontent.com/57015073/191843703-c7225efe-dcfb-4c0c-909e-2b482e9661ff.png)
#### Cola:
![LayerRectaCola](https://user-images.githubusercontent.com/57015073/191843681-110d37b5-5776-4e97-ada6-14846bcfc5c0.png) ![LayerRampaCola](https://user-images.githubusercontent.com/57015073/191843708-441f0e52-1925-4c02-b4ea-11ff2fc5d9c9.png) ![LayerUnderCola](https://user-images.githubusercontent.com/57015073/191843695-8f523082-3da1-4b7a-9e48-a05d381d46c1.png)
#### Lama:
![LayerRectaLama](https://user-images.githubusercontent.com/57015073/191843685-2a8bc469-fee9-4b6a-8768-9220be985563.png) ![LayerRampaLama](https://user-images.githubusercontent.com/57015073/191843709-afd3204c-b312-43b3-968a-ead11fe72334.png) ![LayerUnderLama](https://user-images.githubusercontent.com/57015073/191843697-61b8b9b6-f9f6-4813-9fb2-af4701a4f87a.png)
#### Relva:
![LayerRectaRelva](https://user-images.githubusercontent.com/57015073/191843687-436273ff-6d4f-435f-ae84-698803823924.png) ![LayerRampaRelva](https://user-images.githubusercontent.com/57015073/191843711-81d23779-490e-414f-968b-4e7559bc3103.png) ![LayerUnderRelva](https://user-images.githubusercontent.com/57015073/191843700-3504348e-208f-40c9-8898-a6b6eb6304d1.png)
#### Boost:
![LayerRectaBoost](https://user-images.githubusercontent.com/57015073/191843713-f5fde91a-4c57-431c-bdf6-22a0d771e4ae.png) ![LayerRampaBoost](https://user-images.githubusercontent.com/57015073/191843705-6d614595-e985-4697-8824-b1d193de44b8.png) ![LayerUnderBoost](https://user-images.githubusercontent.com/57015073/191843693-87da0d7c-76ed-4bf9-af96-d626b3b941ec.png)

## Glossário das tarefas

**TAREFA1:** ``Gerar um mapa aleatório``

**TAREFA2:** ``Criação dos tipos de jogadas (Andar para todas as direções, acelerar, desacelerar e disparar munições de cola para a retaguarda)``

**TAREFA3:** ``Compressão de mapas``

**TAREFA4:** ``Efeito da passagem do tempo num estado do jogo``

**TAREFA5:** ``Recriação gráfica do jogo através da biblioteca Gloss ``

**TAREFA6:** ``Criação do bot``
