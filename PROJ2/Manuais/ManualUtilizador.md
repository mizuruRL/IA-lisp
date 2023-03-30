# **Dots and Boxes**: Manual Utilizador
Inteligência Artificial - Escola Superior de Tecnologia de Setúbal

André Dias - 201901690

João Caetano - 201901961

2022/2023

Prof. Joaquim Filipe
Eng. Filipe Mariano

## 2. Introdução

Este documento servirá como guia de instalação, utilização e troubleshooting do utilizador comum. Visa explicar de forma sucinta o objetivo do projeto **Dots and Boxes**, feito para simular uma versão simplificada funcional do jogo **Dots and Boxes**.

## 3. Instalação e utilização

Para que o programa corra, sugere-se a instalação de um ambiente de programação lisp

- [Lispworks](http://www.lispworks.com/)
- [GNU CLISP](https://clisp.sourceforge.io/)

Após a sua instalação, abra a linha de comandos na pasta onde está localizado o programa (listener no lispworks / escrever clisp na linha de comandos de escolha caso opte por GNU CLISP) e escreva o seguinte comando:

```lisp
(load (compile-file "jogo.lisp"))
```

Ao carregar em enter, se não houve qualquer tipo de erro, o programa fica pronto a executar! Se ocorrer algum erro durante a compilação do ficheiro "jogo.lisp" verifique se os ficheiros que vieram distribuídos com este programa estão todos na mesma pasta e que a terminal está a ser executada dentro dessa mesma pasta.

Para executar o programa, basta escrever:

```lisp
(start)
```

E seguir as instruções que aparecem na linha de comandos.

## 4. Input/Output

Após execução do comando anterior, deverá estar perante o seguinte output:

```
Welcome to the Dots and Boxes game!
Developed by: Andre Dias and Joao Caetano

Pick the desired game mode:
1 - You Vs AI
2 - Clash of bots (AI vs AI)
```

Ao teclar 1 e premir enter no teclado, será redirecionado/a para o modo de jogo base, onde tem de competir contra o computador. Ao fechar 4 lados com as suas peças (representadas por -) ganha um ponto. O computador é representado pelo simbolo #. O jogo decorrerá até não haver mais jogadas possíveis. Digite **quit** e pressione enter para sair a qualquer momento.

Ao teclar 2 e pressionar enter, correrá o mesmo jogo, mas com 2 computadores a disputar pelo tabuleiro entre si.

No final de cada jogada, esta é registada num ficheiro "log.dat".

## 5 Exemplo de aplicação

O funcionamento expectável segue um caminho parecido a este:

```
Welcome to the Dots and Boxes game!
Developed by: Andre Dias and Joao Caetano

Pick the desired game mode:
1 - You Vs AI
2 - Clash of bots (AI vs AI)

>1



Board:
.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .


Player 1 score: 0
Player 2 score: 0
Node cuts: 0
Explored-nodes: 0

Your turn

Pick arc type (v - Vertical, h - Horizontal):
>h

Pick line:
>1

Pick column:
>1
Board:
.----.    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .


Player 1 score: 0
Player 2 score: 0
Node cuts: 0
Explored-nodes: 0



Board:
.----.    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .
                              #
.    .    .    .    .    .    .


Player 1 score: 0
Player 2 score: 0
Node cuts: 69
Explored-nodes: 2185

Your turn

Pick arc type (v - Vertical, h - Horizontal):
>h

Pick line:
>2

Pick column:
>1
Board:
.----.    .    .    .    .    .

.----.    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .
                              #
.    .    .    .    .    .    .


Player 1 score: 0
Player 2 score: 0
Node cuts: 69
Explored-nodes: 2185



Board:
.----.    .    .    .    .    .

.----.    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .

.    .    .    .    .    .    .
                              #
.    .    .    .    .    .####.


Player 1 score: 0
Player 2 score: 0
Node cuts: 66
Explored-nodes: 2061

Your turn

Pick arc type (v - Vertical, h - Horizontal):
>
```
Caso aconteça algum erro excecional (crash), recomenda-se o reinício do programa. Qualquer erro que faça, o programa está pronto para lidar com ele, e mostra no output no que errou.

## 6 Limitações

Não é possível especificar um arco e a sua posição numa só entrada de input, pelo que no programa terá de escolher primeiro o tipo de arco que quer jogar e qual a linha e coluna. Também não será possível retroceder após a escolha de arco, linha ou coluna.
