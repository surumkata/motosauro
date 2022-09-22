<p align="center">
  <img src=https://user-images.githubusercontent.com/57015073/191850180-7445bf00-fbc9-46c0-b8dc-68c9e83e7016.png>
</p>

# Motosauro - Projeto de Laboratórios de Informática 1 (LI1) - Recriação do jogo "ExciteBike"- Ano letivo de 2019/2020

**Excitebike** é um jogo de corrida de motocross desenvolvido pela Nintendo. Foi lançado no Japão para o Family Computer em 1984 e foi um dos títulos de lançamento para o NES em 1985.

O objetivo deste projeto de **Laboratórios de Informática 1** era a recriação desse jogo, utilizando a linguagem de programação **Haskell**, assim, colocando em prática os ensinamentos obtidos ao longo das diferentes cadeiras do semestre, como **Programação Informal**.  Por conta das mudanças gráficas decidimos apelidar a nossa nova versão de **"Motosauro"**.

Este trabalho foi realizado por Tiago Silva e Hugo Fernandes, estudantes do 1º ano de engenharia informática da Universidade do Minho no ano letivo de 2019/2020.

## Jogabilidade

No Motosauro as corridas são de sempre 4 motas, pudendo elas serem controladas por jogadores em tela compartilhada e/ou por bots.
O jogo é simples, o jogador pode movimentar a moto entre pistas (**cima** e **baixo**), **acelerar** e **desacelerar**, inclinar a moto enquanto estiver no ar (**esquerda** e **direita**), e por fim, pode ainda **disparar** **cola** (4 munições por corrida) para o piso atrás dele de modo a atrasar as outras motas. De realçar que, se o jogador cai do ar numa inclinação muito torta, este fica **morto** durante alguns segundos.
Por falar em pisos, o jogo conta com 5 tipos de pisos diferentes, que servem para alterar a velocidade da mota. Do piso mais lento para o mais rápido são: **Cola, Lama, Relva, Terra e Boost**
Finalizando, o jogo conta ainda com 2 tipos de mapas, os **padrões**, criados por nós, e os **gerados** que necessitam de uma **seed** e um **tamanho**.

## Parte Gráfica

### [Menus](https://github.com/surumkata/Motosauro/tree/main/textures/Menu)
### [Motos](https://github.com/surumkata/Motosauro/tree/main/textures/Others)
### [Pisos](https://github.com/surumkata/Motosauro/tree/main/textures/Layers)

## Glossário das tarefas

**TAREFA 1:** ``Gerar um mapa aleatório``

**TAREFA 2:** ``Criação dos tipos de jogadas (Andar para todas as direções, acelerar, desacelerar e disparar munições de cola para a retaguarda)``

**TAREFA 3:** ``Compressão de mapas``

**TAREFA 4:** ``Efeito da passagem do tempo num estado do jogo``

**TAREFA 5:** ``Recriação gráfica do jogo através da biblioteca Gloss ``

**TAREFA 6:** ``Criação do bot``
