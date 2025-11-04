module Ajudas where 

import Graphics.Gloss.Juicy (loadJuicy)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (fromMaybe)
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import MapaExemplo
import DadosAuxiliar 
import GestorTeclas
import DadosAuxiliar
import GestorTeclas



{-|
Função que coloca no ecrã as ajudas
-}
criaAjudas :: EstadoProjeto -> Picture
criaAjudas estadoProjeto = 
    let 
        ((xi, yi), (xs, ys)) = calchitbox (jogador (jogo estadoProjeto))
        (x, y) = posicao (jogador (jogo estadoProjeto))
    in 
        pictures [ 
            translate (convertX x) (convertY y) $ imagemHitbox (calchitbox (jogador (jogo estadoProjeto))),
            translate (convertX 5) (convertY 5) $ ajudaJogador (jogador (jogo estadoProjeto))
        ]

{-|
Função que calcula e desenha a hitbox do jogador 
-}
imagemHitbox :: Hitbox -> Picture
imagemHitbox ((xi, yi), (xs, ys)) = color green $ rectangleWire (realToFrac ((xs * fromIntegral tamanhoBloco) - (xi * fromIntegral tamanhoBloco))) (realToFrac ((ys * fromIntegral tamanhoBloco) - (yi * fromIntegral tamanhoBloco)))

ajudaJogador :: Personagem -> Picture
ajudaJogador personagem = color white $ imagemDoTexto (textoDePersonagem personagem)

imagemDoTexto :: [String] -> Picture
imagemDoTexto l = pictures $ zipWith (\y linha -> scale 0.1 0.1 $ translate 0 y $ text linha) [0, -120..] l

textoDePersonagem :: Personagem -> [String]
textoDePersonagem personagem =
    [ "Hitbox: " ++ show (calchitbox personagem),
       "ColisõesParede: " ++ show (colisoesParede mapa1 personagem)
    ]

pixeisPorBloco :: Float
pixeisPorBloco = 32 --tamanho em float de cada bloco por pixeis

{-|
Tamanho inteiro de cada bloco por pixeis
-}
tamanhoBloco :: Int
tamanhoBloco = 32

cantoXSuperior :: Int -> Float
cantoXSuperior larguraJanela = - (fromIntegral larguraJanela / 2)

cantoYSuperior :: Int -> Float
cantoYSuperior alturaJanela = (fromIntegral alturaJanela / 2)

{-|
Funções que fazem a conversão da posição x e y para Gloss 
-}
convertX :: Double -> Float
convertX x = realToFrac (( x * fromIntegral tamanhoBloco)) 

convertY :: Double -> Float
convertY y = - realToFrac (y * fromIntegral tamanhoBloco)
