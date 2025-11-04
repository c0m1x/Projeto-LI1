{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Tiago José Pereira Martins <a106927@alunos.uminho.pt>
              Gabriel Pinto Couto <a107373@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1

{-|
Função principal que chama todas as outras funções da Tarefa "encadeando-as com "$" de modo a só chamar a variável j uma vez
-}
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo j@(Jogo mapa inimigos colecionaveis jogador) = emEscadaJogadorJogo $ vidaInimigosJogo $ inimigoMortoJogo $ aplicaGravidadeJogo tempo $  calcVidaJogadorJogo $ aplicarColecionaveisJogo $ jogadorPisaAlcapaoJogo $ colisaoPersonagemJogo $ colisaoParaInimigosJogo $
                                                                        movimentaInimigosXJogo tempo $ movimentaPersonagemXJogo tempo $ movimentaPersonagemYJogo tempo $ countdownJogo tempo j


{-|
Função que atualiza se um jogador se encontra em escadas ou não
-}

emEscadaJogador :: Mapa -> Personagem -> Personagem 
emEscadaJogador m@(Mapa _ _ matriz) jogador             
         | encontraBloco (x,y) matriz == Escada = jogador {emEscada = True }             
         | otherwise = jogador {emEscada = False}         
         where (x,y) = posicao jogador  

{-| 
  Função que muda a função emEscadaJogador de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
-}
emEscadaJogadorJogo :: Jogo -> Jogo
emEscadaJogadorJogo jogo = Jogo {mapa = mapa jogo, inimigos = inimigos jogo, colecionaveis = colecionaveis jogo, jogador = emEscadaJogador (mapa jogo) (jogador jogo)}

encontraBloco :: Posicao -> [[Bloco]] -> Bloco
encontraBloco (x,y) blocos = (blocos !! floor y) !! floor x

{-| 
  Função que muda a função moveTodosInimigos de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
-}

movimentaInimigosXJogo ::  Tempo -> Jogo -> Jogo
movimentaInimigosXJogo t j = j {inimigos = moveTodosInimigos t (mapa j) (inimigos j)}


{-| 
  Função que muda a função movimentaPersonagemX de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
-}
movimentaPersonagemXJogo :: Tempo -> Jogo -> Jogo
movimentaPersonagemXJogo tempo j = j {jogador = movimentaPersonagemX tempo (mapa j) (jogador j)}


{-| 
  Função que muda a função movimentaPersonagemY de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
-}
movimentaPersonagemYJogo :: Tempo -> Jogo -> Jogo
movimentaPersonagemYJogo tempo j = j {jogador = movimentaPersonagemY tempo (mapa j) (jogador j)}

{-| 
  Função que aplica a função moveInimigosX a todos os inimigos do jogo. Visto que os inimigos não sobem nem descem, não necessitamos de criar outra função semelhante
  que calculasse as suas posições para as coordenas Y
-}
moveTodosInimigos :: Tempo -> Mapa -> [Personagem] -> [Personagem]
moveTodosInimigos tempo mapa listaInimigos = map (moveInimigosX tempo mapa) listaInimigos

{-|
Função que calcula as novas coordenas dos inimigos apenas para os valores de x
-}
moveInimigosX :: Tempo -> Mapa -> Personagem -> Personagem
moveInimigosX variacaotempo mapa@(Mapa _ _ matrizDeBlocos) personagem
  | colisoesParede mapa atualizaInimigo || blocoVazio = reverteInimigo   -- se o inimigo estiver a embater com uma parede ou se passar por cima de um bloco vazio altera a sua direção
  | otherwise = moveInimigosX' variacaotempo personagem  -- se não, aplica a função auxiliar ao inimigo

    where
        atualizaInimigo = moveInimigosX' variacaotempo personagem -- calcula as novas coordenadas do inimigos após ser movimentado
        direcao' = direcao personagem                                  -- indica a direção da variável personagem
        (velXInimigo, _) = velocidade personagem                       -- indica a velocidade da variável personagem
        blocoVazio = personagemSobreBloco atualizaInimigo Vazio matrizDeBlocos -- verifica se o personagem está a passar por cima de um bloco vazio
        reverteInimigo = personagem { direcao = trocaDirecao direcao', velocidade = (-velXInimigo, 0) } --função que inverte a direção e velocidade de um inimigo
{-| 
Função que calcula se um personagem se encontra sobre um determinado bloco,
-}
personagemSobreBloco :: Personagem -> Bloco -> [[Bloco]] -> Bool
personagemSobreBloco jogador bloco matrizDeBlocos =
    let
        ((x, y), (xs, ys)) = calchitbox jogador     -- calcula a hitbox da variável jogador, sendo "(x,y)" o ponto superior esquerdo e "(xs,ys)" o ponto superior direito
        (x',y') =  (floor xs,  floor (ys + 1))
        (x'',y'') = (floor x , floor (ys + 1))
        blocoNaPosicao x y = matrizDeBlocos !! y !! x == bloco
    in
        blocoNaPosicao x' y'|| blocoNaPosicao x'' y''

{-|
Função auxiliar simples para calcular a direção oposta e diminuir o número de linhas de código
-}
trocaDirecao :: Direcao -> Direcao
trocaDirecao Este = Oeste
trocaDirecao Oeste = Este
trocaDirecao Norte = Sul
trocaDirecao Sul = Norte


{-|
Função que movimenta o inimigo apenas para os valores de X
-}

moveInimigosX' :: Tempo -> Personagem -> Personagem
moveInimigosX' variacaotempo inimigo = inimigo {posicao = (x + v * variacaotempo, y)}
  where (x,y) = posicao inimigo
        (v,v') = velocidade inimigo



{-|
--função que verifica se o personagem está a colidir com o limite lateral esquerdo do mapa 
-}
limiteEsquerdo :: Mapa -> Personagem -> Bool
limiteEsquerdo mapa jogador =
 let (x,y) = posicao jogador
 in x < 1


{-|
--função que verifica se o personagem está a colidir com o limite lateral direito do mapa 
-}
limiteDireito :: Mapa -> Personagem -> Bool
limiteDireito mapa@((Mapa _ _ matrizDeBlocos)) jogador =
  let larguramapa = fromIntegral (length (head matrizDeBlocos)-1)
      (x, _) = posicao jogador
  in x > larguramapa

{-|
--função para movimentar o personagem apenas nas coordenadas X
-}
movimentaPersonagemX :: Tempo -> Mapa -> Personagem -> Personagem
movimentaPersonagemX variacaotempo mapa jogador
              | limiteDireito mapa jogador && vx > 0  = jogador
              | limiteEsquerdo mapa jogador && vx < 0 = jogador
              | otherwise = jogador'
              where jogador' = movimentaPersonagemXaux variacaotempo jogador
                    (vx, _ ) = velocidade jogador

{-|
--função auxiliar que calcula as novas coordenadas do jogador após o movimento
-}
movimentaPersonagemXaux :: Tempo -> Personagem -> Personagem
movimentaPersonagemXaux variacaotempo jogador = jogador { posicao = ( x + vx * variacaotempo,y)}
  where  (x,y) = posicao jogador
         (vx, _ ) = velocidade jogador

{-|                   
--função que faz o mesmo que a movimentaPersonagemX mas para as coordenadas y
-}
movimentaPersonagemY :: Tempo -> Mapa -> Personagem -> Personagem
movimentaPersonagemY variacaotempo mapa jogador
                | colisoesParede mapa jogador' = jogador
                | otherwise = jogador'
                where jogador' = movimentaPersonagemYaux variacaotempo jogador
{-|
--função que calcula as novas coordenadas do jogador tendo em conta a velocidade e a passagem do tempo
-}
movimentaPersonagemYaux:: Tempo -> Personagem -> Personagem
movimentaPersonagemYaux variacaotempo jogador = jogador { posicao = ( x, y + vy * variacaotempo)}
   where (x,y) = posicao jogador
         (_,vy) = velocidade jogador





--Funções da lista da Tarefa3


--1)
{-|
  Função que muda a função vidaInimigos de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
vidaInimigosJogo :: Jogo -> Jogo
vidaInimigosJogo j = j {inimigos = vidaInimigos (inimigos j) ( jogador j )}


{-|
--função que calcula a vida dos inimigos. Se o jogador não estiver armado devolve automaticamente False, mas se estiver armado, chama a função danoInimigo a todos os 
inimigos
 -}

vidaInimigos:: [Personagem] -> Personagem -> [Personagem]
vidaInimigos inimigos jogador
           | not (armadoAux jogador) = inimigos
           | otherwise =  map (danoInimigo jogador) inimigos
{-| 
--função que aplica e calcula o dano aplicado nos inimigos, usando a função interseta e calchitbox da tarefa1 
-}

danoInimigo :: Personagem -> Personagem -> Personagem
danoInimigo jogador inimigo =
                    if armadoAux jogador && interseta (hitboxDeDano jogador) (calchitbox inimigo) then inimigo {vida = max 0 (vida inimigo -1) } -- assumindo que cada inimigo morre com 1 hit
                    else inimigo
{-|
--função que verifica se um jogador está armado
-}
armadoAux :: Personagem -> Bool
armadoAux jogador = dano && tempoRestante > 0 --normalmente o facto de dano ser True siginifica que o tempo restante é sempre maior que 0 mas decidimos colocar pelo sim pelo não
                        where
                            (dano, tempoRestante) = aplicaDano jogador

{-|
--função que calcula a hitbox de dano de um jogador, aumentando metade do tamanho do personagem para os lados, dependendo da sua direção, visto que o martelo apenas aplica
dano quando o personagem está "virado" para o inimigo.
-}

hitboxDeDano :: Personagem -> Hitbox
hitboxDeDano personagem
             | d == Oeste = ((x1-largura,y1),(x2-largura,y2))
             | d == Este =((x1+largura,y1),(x2+largura,y2))
             | otherwise =((0,0),(0,0))
             where d = direcao personagem
                   ((x1,y1),(x2,y2)) = calchitbox personagem
                   (altura,largura) = tamanho personagem


--2)

{-|
  Função que muda a função inimigoMorto de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
inimigoMortoJogo :: Jogo -> Jogo
inimigoMortoJogo j = j {inimigos = retiraSeMorto (inimigos j)}

{-| 
--função que testa se um inimigo está vivo ( ou não) e retira-o do mapa, decidimos colocá-lo num lugar não visível, pois não o poderíamos retirar da lista de inimigos
-}
inimigoMorto :: Personagem -> Personagem
inimigoMorto fantasma =
    if vida fantasma == 0 then fantasma {posicao = (-9999 , -9999)}
    else fantasma
{-|
Função que aplica a função anterior a todos os inmigos
-}
retiraSeMorto :: [Personagem] -> [Personagem]
retiraSeMorto listaInimigos = map inimigoMorto listaInimigos

--3)

{-|
  Função que muda a função aplicaGravidade de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}

aplicaGravidadeJogo :: Tempo -> Jogo -> Jogo
aplicaGravidadeJogo t j =  j { jogador = aplicaGravidade t ( jogador j)}

{-|
--Função que aplica o efeito Gravidade ao jogador, tendo em conta a variação do tempo
-}
aplicaGravidade :: Tempo -> Personagem -> Personagem
aplicaGravidade tempo jogador
  | emEscada jogador = jogador
  | otherwise =
    let
      (velocidadex, velocidadey) = velocidade jogador
      novavelocidadey = velocidadey + (snd gravidade * 10) * tempo -- assumindo que o gravidade tem valor 10
    in
      jogador { velocidade = (velocidadex, novavelocidadey) } 
              
        


--4)

{-|
  Função que muda a função calcVidaJogador de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
calcVidaJogadorJogo :: Jogo -> Jogo
calcVidaJogadorJogo j = j {jogador = calcVidaJogador (mapa j) (jogador j) (inimigos j)}

{-|
Função que calcula a vida e muda a sua posição para a posição inicial de um jogador depois de sofrer ( ou não) dano de todos os inimigos no mapa
-}
calcVidaJogador :: Mapa -> Personagem -> [Personagem] -> Personagem
calcVidaJogador _ j [] = j
calcVidaJogador m@(Mapa (pi,_) _ _)  jogador listaDeInimigos@(h:t)
               | colisoesPersonagens jogador h = jogador {vida = max 0 (vida jogador -1), posicao = pi}
               | otherwise = calcVidaJogador m jogador t

{-|
--Função que calcula a vida de um jogador depois de se intersetar com um inimigo apenas
-}
calcVidaJogadorAux ::Personagem -> Personagem -> Personagem
calcVidaJogadorAux jogador fantasma =
    if interseta (calchitbox jogador) (calchitbox fantasma)  && not (armadoAux jogador) then jogador {vida = max 0 (vida jogador -1)} -- utilizamos "max" pois o jogador não pode ter vida negativa
    else jogador

--5) 

{-|
  Função que muda a função aplicarColecionaveisJogo de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
aplicarColecionaveisJogo :: Jogo -> Jogo
aplicarColecionaveisJogo j = j {jogador = aplicarColecionaveis (jogador j) (colecionaveis j)}

{-|
--Função que calcula a hitbox de um Colecionável, assumindo também que o seu tamanho é unitário
-}
hitboxColecionavel :: (Colecionavel, Posicao) -> Hitbox
hitboxColecionavel (_, (x,y)) = ((x-0.5, y-0.5),(x+0.5,y+0.5))

{-|
--função que calcula a aplicação de um colecionável ao jogador
-}
aplicaColecionavel :: Personagem -> (Colecionavel, Posicao) -> Personagem
aplicaColecionavel jogador (colecionavel, _)
        | colecionavel == Martelo = jogador {aplicaDano = (True,10)} --assumimos que quando um personagem coleta um martelo o tempo de utilização passa para 10 em vez de adicionar 10
        | colecionavel == Moeda = jogador {pontos = pontos jogador +1}
{-|
--Função que aplica os colecionáveis aos jogadores 
-}
aplicarColecionaveis :: Personagem -> [(Colecionavel,Posicao)] -> Personagem
aplicarColecionaveis jogador t = foldl aplicaColecionavel jogador t

{-|
Resolução alternativa: 
aplicarColecionaveis jogador [] = jogador
aplicarColecionaveis jogador (h:t) =  aplicarColecionaveis (aplicaColecionavel jogador h) t
-}

--6) 

{-|
  Função que muda a função jogadorPisaAlcapao de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
jogadorPisaAlcapaoJogo :: Jogo -> Jogo
jogadorPisaAlcapaoJogo j = j {mapa = pisarAlcapao (jogador j) (mapa j)}

{-|
Função que calcula o efeito de um personagem pisar um Alçapão, ou seja, mudar o bloco para Vazio 
-}
efeitoAlcapao :: Personagem -> ((Int, Int), Bloco) -> Bloco
efeitoAlcapao jogador ((x, y), bloco)
  | bloco == Alcapao && y == inferiorY1 && x == inferiorX1 = Vazio
  | bloco == Alcapao && y == inferiorY2 && x == inferiorX2 = Vazio
  | otherwise = bloco
  where
    ((x', y'), (xs, ys)) = calchitbox jogador
    inferiorX1 = floor xs
    inferiorY1 = floor ys
    inferiorX2 = floor x'
    inferiorY2 = floor ys

{-| 
Função que aplica a função anterior a um mapa inteiro
-}
pisarAlcapao :: Personagem -> Mapa -> Mapa
pisarAlcapao jogador mapa@(Mapa x y matrizDeBlocos) =
  Mapa x y (map (map (efeitoAlcapao jogador)) matrizDeBlocosComPosicoes)
  where
    matrizDeBlocosComPosicoes =
      zipWith (\linha y' -> zipWith (\bloco x' -> ((x', y'), bloco)) linha [0 ..]) matrizDeBlocos [0 ..]

{-|
  Função que muda a função countdown de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
countdownJogo :: Tempo -> Jogo -> Jogo
countdownJogo tempo j = j {jogador = countdown tempo (jogador j)}

{-| 
Função que calcula a passagem do tempo 
-}
countdown :: Tempo -> Personagem -> Personagem
countdown tempo jogador = if b > 0 then jogador {aplicaDano = (True, b-tempo)} else jogador {aplicaDano = (False,0) }
                        where (a,b) = aplicaDano jogador



--7) 

{-|
  Função que muda a função  colisaoPersonagem de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
colisaoPersonagemJogo :: Jogo -> Jogo
colisaoPersonagemJogo j = j {jogador = colisaoPersonagem (mapa j) (jogador j)}

{-|
  Função que muda a função colisaoParaInimigos de modo a receber Jogo e devolver Jogo, para assim encadear na função movimenta
 -}
colisaoParaInimigosJogo :: Jogo -> Jogo
colisaoParaInimigosJogo j = j {inimigos = colisaoParaInimigos (mapa j) (inimigos j)}

{-|
--função que calcula se um jogador está a colidir com uma parede ou com os limites do mapa e altera a sua velocidade para 0 para parar
-}
colisaoPersonagem :: Mapa -> Personagem -> Personagem
colisaoPersonagem mapa jogador
                | colisaolimitesmapa mapa jogador || colisaoJogadorBloco jogador mapa = jogador {velocidade=(0,y)}
                | otherwise = jogador
                where y = snd (velocidade jogador)

{-|
--função que calcula se um inimigo está a colidir com uma parede ou com os limites do mapa e altera a sua direção 
-}
colisaoParaInimigo :: Mapa -> Personagem -> Personagem
colisaoParaInimigo mapa inimigo =
    let invertedirecao = if direcao inimigo == Este then Oeste else Este
    in if colisaolimitesmapa mapa inimigo || colisaoJogadorBloco inimigo mapa then inimigo {direcao = invertedirecao}
       else inimigo
{-|
-- função que vai aplicar a função anterior a todos os inimigos do jogo
-}
colisaoParaInimigos :: Mapa -> [Personagem] -> [Personagem]
colisaoParaInimigos _ [] = []
colisaoParaInimigos mapa (inimigo:resto) = colisaoParaInimigo mapa inimigo : colisaoParaInimigos mapa resto



