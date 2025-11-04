{-
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Tiago José Pereira Martins <a106927@alunos.uminho.pt>
              Gabriel Pinto Couto <a107373@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324



type Ponto = (Double, Double)

{-|
 Função que calcula a hitbox de um personagem, pegando nas sua posição (x1,y1) e no seu tamanho (x2,y2). A hitbox do personagem é composta por 2 pontos,
sendo estes o canto superior esquerdo e o canto inferior direito.
-}
calchitbox :: Personagem -> Hitbox
calchitbox personagem =
      ((x1-(x2/2),y1-y2/2), (x1 +(x2/2),y1 + y2/2))
      where (x1,y1) = posicao personagem
            (x2,y2) = tamanho personagem

{-|
 Função que calcula se duas hitboxes se intersetam, ou seja, se os valores dos cantos ( superior esquerdo e inferior direito) colidem ou sobrepoẽm, por isso
 é que consediramos que para valores iguais o resultado também é True.
-}

interseta :: Hitbox -> Hitbox -> Bool
interseta ((x1,y1), (x2,y2)) ((x3,y3), (x4,y4)) = x1 <= x4 && x2 >= x3 && y1 <= y4 && y2 >= y3

{-|
 Função que verifica se um personagem ultrapassa os limites do mapa. Como a origem do referencial do mapa é o ponto (0,0) no canto supeior esquerdo da janela,
 um personagem apenas ultrapassa o limite esquerdo do mapa se o seu valor x for inferior a 0 e maior que a largura do mapa. Usamos o mesmo raciocínio par os valores de y
 -}
colisaolimitesmapa:: Mapa -> Personagem -> Bool
colisaolimitesmapa (Mapa _ _ matrizDeBlocos) personagem  = (x1-x2/2) <= 0 || (x1+x2/2) >= larguraMapa || (y1-y2/2) <= 0 || (y1+y2/2) >= alturaMapa
                                     where (x1,y1) = posicao personagem
                                           (x2,y2) = tamanho personagem
                                           larguraMapa = fromIntegral $ length (head matrizDeBlocos)
                                           alturaMapa = fromIntegral $ length matrizDeBlocos
{-|
Função que verifica a colisão entre jogador e bloco, assumindo que a posição do primeiro bloco é (0.5,0.5), pois o canto supeior esquerdo do primeiro bloco é 
(0,0) e nós queríamos a posição do valor central do bloco
-}
colisaoJogadorBloco :: Personagem -> Mapa -> Bool
colisaoJogadorBloco jogador mapa@(Mapa _ _ matrizblocos) = colisaoJogadorBlocoAux jogador (blocosPosicoes (0.5, 0.5) matrizblocos)

{-|
-- Função que dada uma posição inicial, nos devolve uma lista com as posições das Plataformas, percorrendo " linha a linha" usando a auxiliar
-}

blocosPosicoes :: Posicao -> [[Bloco]] -> [Posicao]
blocosPosicoes _ [] = []
blocosPosicoes (x, y) (h:t) = blocosPosicoesAux (x, y) h ++ blocosPosicoes (x, y + 1) t

{-|
--função que verifica se um bloco é uma plataforma, e se for, devolve a sua posição no mapa
-}
blocosPosicoesAux :: Posicao -> [Bloco] -> [Posicao]
blocosPosicoesAux (x, y) listablocos =
  [(x', y) | (Plataforma, x') <- zip listablocos [x..]]

{-|
-- função que dada a posicao do jogador e uma lista de posicoes dos blocos, verifica se a hitbox do jogador, esta a colidir com a hitbox de qualquer bloco
-}
colisaoJogadorBlocoAux :: Personagem -> [Posicao] -> Bool
colisaoJogadorBlocoAux _ [] = False
colisaoJogadorBlocoAux personagem (h:t)
  | interseta (calchitbox personagem) (calculaHitboxBloco h) = True
  | otherwise = colisaoJogadorBlocoAux personagem t

{-|
-- função que dada a posição de um bloco, calcula a sua hitbox, ou seja, assumindo que o tamanho do bloco é (1,1)
-}
calculaHitboxBloco :: Posicao -> Hitbox
calculaHitboxBloco (x, y) = ((x - 0.5, y - 0.5), (x + 0.5, y + 0.5))

{-|
--Função que verifica se duas personagens de intersetam, ou seja, chama a função interseta para quaisquer duas personagens, calculando as suas hitboxes
-}
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens personagem1 personagem2 = interseta hitbox1 hitbox2
                                            where hitbox1 = calchitbox personagem1
                                                  hitbox2 = calchitbox personagem2

{-|
--Função que verifica de uma personagem interseta-se com um mapa, ou seja, verifica se a personagem colide com os limites do mapa ou com qualquer plataforma do mapa
-}
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede mapa1@(Mapa _ _ matrizDeBlocos) personagem1 = colisaolimitesmapa mapa1 personagem1 ||  colisaoJogadorBloco  personagem1 mapa1


