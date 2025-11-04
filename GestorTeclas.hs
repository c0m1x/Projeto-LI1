module GestorTeclas where 


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (fromMaybe)
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import DadosAuxiliar

{-|
Função para gerir as teclas 
-}
handleTeclas :: Event -> EstadoProjeto -> EstadoProjeto
handleTeclas evento estado@(EstadoProjeto {estadoAtual = Estadodomenu }) =
  handleTeclasMenu evento estado

handleTeclas evento estado@(EstadoProjeto {estadoAtual = EstadoJogo }) =
  handleTeclasJogo evento estado


{-|
Função para gerir as teclas dentro do menu
-}
handleTeclasMenu :: Event -> EstadoProjeto -> EstadoProjeto
handleTeclasMenu (EventKey (SpecialKey KeyRight) Down _ _) estadoProjeto =
    estadoProjeto  {menu = opcaoAtualizada}
     where
         opcaoAtualizada = (menu estadoProjeto) { opcaoEscolhida = proximaOpcao (opcaoEscolhida (menu estadoProjeto)) }

handleTeclasMenu (EventKey (SpecialKey KeyLeft) Down _ _) estadoProjeto =
    estadoProjeto {menu = opcaoAtualizada}
     where
          opcaoAtualizada = (menu estadoProjeto) { opcaoEscolhida =  opcaoAnterior (opcaoEscolhida (menu estadoProjeto)) }


handleTeclasMenu (EventKey (SpecialKey KeyEnter) Down _ _) estadoProjeto
  | opcaoEscolhida m == Sair = error "O jogo foi encerrado"
  | opcaoEscolhida m == Jogar = estadoProjeto {estadoAtual = EstadoJogo}
  | otherwise = estadoProjeto
  where
      m = menu estadoProjeto

handleTeclasMenu _ estadoProjeto = estadoProjeto -- se nenhum caso acima for feito, manter o mesmom estado do Menu

{-|
Função auxiliar para obter a próxima opção do menu
-}
proximaOpcao :: Menuescolher -> Menuescolher
proximaOpcao Jogar  = Sair
proximaOpcao Sair = Jogar

{-|
Função auxiliar para obter a opção anterior do menu
-}
opcaoAnterior :: Menuescolher -> Menuescolher
opcaoAnterior Jogar  = Sair
opcaoAnterior Sair = Jogar


{-|
Função para gerir as teclas em jogo
-}
--função para gerir as teclas em jogo
handleTeclasJogo :: Event -> EstadoProjeto -> EstadoProjeto
handleTeclasJogo evento estado =
    case evento of
        (EventKey (SpecialKey bind) Down _ _) ->
            case bind of
                KeyLeft -> estado { jogo = atualiza [] (Just AndarEsquerda) (jogo estado)}
                KeyRight -> estado { jogo = atualiza [] (Just AndarDireita) (jogo estado)}
                KeySpace -> estado { jogo = atualiza [] (Just Saltar) (jogo estado)}
                KeyUp -> estado { jogo = atualiza [] (Just Subir) (jogo estado)}
                KeyDown -> estado { jogo = atualiza [] (Just Descer) (jogo estado)}
                KeyEnter -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyAltL -> estado { estadoAtual = Estadodomenu}
                _ -> estado
        (EventKey (SpecialKey bind) Up _ _) ->
            case bind of
                KeyRight -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyLeft -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyUp -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyDown -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                _ -> estado
        _ -> estado

