module DadosAuxiliar where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (fromMaybe)
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4


data EstadoProjeto = EstadoProjeto {
        tempoTotal :: Float,
        estadoAtual :: Estado,
        menu :: Menu,
        jogo :: Jogo,
        blocos :: ImagensBloco,
        imagensPersonagem :: ImagensPersonagem,
        alturaJanela :: Int,
        larguraJanela :: Int
      } deriving (Eq,Show)


data Menu = Menu {
                  itens :: [Menuescolher],
                  opcaoEscolhida :: Menuescolher,
                  imagensMenu:: ImagensMenu
                  } deriving (Eq, Show)

data Menuescolher = Jogar | Sair deriving (Eq, Show)

data Estado = Estadodomenu | EstadoJogo
              deriving (Eq,Show, Bounded)

data ImagensMenu = ImagensMenu
                   {
                    imagemJogar ::  Picture,
                    imagemSair ::  Picture
                   } deriving (Eq, Show)

data ImagensBloco = ImagensBloco
                  {
                    imagemPlataforma :: Picture,
                    imagemAlcapao :: Picture,
                    imagemEscada :: Picture,
                    imagemVazio :: Picture,
                    imagemEstrela :: Picture
                     } deriving (Eq, Show)

data ImagensPersonagem = ImagensPersonagem
                        {
                         imagemJogador :: Picture,
                         imagemFantasma :: Picture,
                         imagemMacacoMalvado :: Picture
                         } deriving (Eq, Show)
