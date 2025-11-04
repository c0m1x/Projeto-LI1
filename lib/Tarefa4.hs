{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Tiago José Pereira Martins <a106927@alunos.uminho.pt>
              Gabriel Pinto Couto <a107373@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
import Tarefa1
import Tarefa2
import Tarefa3


{-| 
velocidadedojogador diz a velocidade horizontal do jogador.
-}
velocidadedojogador :: Double
velocidadedojogador = 5

{-|
velocidadenaescada diz velocidade na escada do jogador.
-}
velocidadenaescada :: Double
velocidadenaescada = 3


{-|
Função velocidadegravidade que devolve a velocidade vertical da gravidade.

-}
velocidadegravidade :: Velocidade -> Double
velocidadegravidade (_,x) = snd gravidade

{-|
Função colisaoEntreJogadorEescada que vericia se o jogador e a escada estao a colidir.
-}
colisaoEntreJogadorEescada :: Jogo -> Personagem -> Mapa-> Bool
colisaoEntreJogadorEescada jogo jogador  a = intersetam (calchitbox jogador) (map calculaHitboxBloco (sitiosdasescadas (0.5,0.5) matrizdosblocos)) 
  where  a@(Mapa _ _ matrizdosblocos) = mapa jogo

{-|
Função atualiza que atualiza as ações do jogador a cada frame do jogo.
-}
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza [] Nothing jogo = jogo
atualiza _ acoesdojogador jogo = jogo {jogador = marioatualizado} 
 where  marioatualizado = atualizajogador acoesdojogador jogo
       
       
       
{-|
Função atualizajogador, auxiliar da a Função atualiza.
-}
atualizajogador :: Maybe Acao -> Jogo -> Personagem
atualizajogador acao jogo = case acao of
        Just Saltar -> atualizasalto jogo
        Just AndarDireita -> atualizaandardireita jogo
        Just AndarEsquerda -> atualizaandaresquerda jogo
        Just Subir -> atualizasubir jogo
        Just Descer -> atualizadescer jogo
        Just Parar -> atualizaparar jogo
        Nothing -> mario
 where mario = jogador jogo


{-|
Função verxmario que devolve a posição horizontal do personagem.
-}
verxmario :: Personagem -> Double
verxmario ( Personagem _ _ (x,y) _ _ _ _ _ _ _) = x


{-|
Função atualizasalto auxiliar da atualiza que quando o jogador tenta saltar verifica se pode e caso possa fa-lo saltar.
-}
atualizasalto :: Jogo -> Personagem
atualizasalto jogo = if not marionaescada || vy == 0
      then mario {velocidade = (vx,-10)} 
           else  mario
    where mario = jogador jogo
          (vx,vy) = velocidade mario
          marionaescada = emEscada mario
          a@(Mapa _ _ matrizdosblocos) = mapa jogo

{-|
Função atualizaandardireita auxiliar da atualiza que quando o jogador tenta andar para a direita verifica se pode e caso possa fa-lo andar para a direita.
-}
atualizaandardireita :: Jogo -> Personagem
atualizaandardireita jogo = if not (limiteDireito a mario)
     then mario {velocidade = (5,vy)} {direcao = Este} else  mario
    where mario = jogador jogo
          (vx,vy) = velocidade mario
          marionaescada = emEscada mario
          a@(Mapa _ _ matrizdosblocos) = mapa jogo
          larguraMapa = length (head matrizdosblocos)

{-|
Função atualizaandaresquerda auxiliar da atualiza que quando o jogador tenta andar para a esquerda verifica se pode e caso possa fa-lo andar para a esquerda.
-}
atualizaandaresquerda :: Jogo -> Personagem
atualizaandaresquerda jogo = if not (limiteEsquerdo a mario)
     then mario {velocidade = (-5,vy)} {direcao = Oeste}else  mario
    where mario = jogador jogo
          (vx,vy) = velocidade mario
          marionaescada = emEscada mario
          a@(Mapa _ _ matrizdosblocos) = mapa jogo
          larguraMapa = 0



{-|
Função atualizasubir auxiliar da atualiza que quando o jogador tenta subir uma escada verifica se pode e caso possa fa-lo subir.
-}
atualizasubir :: Jogo -> Personagem
atualizasubir jogo 
          | emEscada mario  = mario { velocidade = (0,-4),direcao=Norte}
          | otherwise = mario
    where mario = jogador jogo
          (vx,vy) = velocidade mario
          a = mapa jogo
          


{-|
Função atualizadescer auxiliar da atualiza que quando o jogador tenta descer uma escada verifica se pode e caso possa fa-lo descer.
-}
atualizadescer :: Jogo -> Personagem
atualizadescer jogo = if marionaescada || colisaoEntreJogadorEescada jogo mario a
    then mario {velocidade = (vx,3)} {emEscada = True} else  mario
    where mario = jogador jogo
          (vx,vy) = velocidade mario
          marionaescada = emEscada mario
          a@(Mapa _ _ matrizdosblocos) = mapa jogo
          larguraMapa = length (head matrizdosblocos)


{-|
Função atualizaparar auxiliar da atualiza que quando o jogador tenta parar verifica se pode e caso possa fa-lo parar.
-}
atualizaparar :: Jogo -> Personagem
atualizaparar jogo 
     | not $ emEscada j = j { velocidade = (0,0)}
     | otherwise        = j 
     where j = jogador jogo




