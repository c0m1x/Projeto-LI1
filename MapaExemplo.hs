module MapaExemplo where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (fromMaybe)
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import GHC.ForeignPtr (ForeignPtrContents(PlainForeignPtr))


mapa1 :: Mapa
mapa1 = Mapa ((10, 10.99), Este) (5, 1.5) blocos1

blocos1 :: [[Bloco]]
blocos1 = [ [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma,Plataforma, Plataforma ]
          , [ Plataforma,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma]
          , [ Plataforma, Vazio,Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Escada,Plataforma]
          , [ Plataforma, Vazio,Vazio, Vazio, Escada, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma,Escada, Plataforma]
          , [ Plataforma, Vazio,Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Escada, Plataforma]
          , [ Plataforma, Vazio,Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Escada,Plataforma]
          , [ Plataforma, Vazio,Vazio, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Escada, Alcapao, Plataforma,Plataforma]
          , [ Plataforma, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Escada, Vazio, Vazio,Plataforma]
          , [ Plataforma, Vazio,Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio,Vazio, Plataforma]
          , [ Plataforma, Vazio,Escada, Plataforma, Plataforma, Alcapao, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Alcapao,Plataforma]
          , [ Plataforma, Vazio,Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio, Plataforma]
          , [ Plataforma, Vazio,Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Plataforma]
          , [ Plataforma, Plataforma,Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]