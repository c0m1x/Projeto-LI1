module Main where

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
import Ajudas
import DadosAuxiliar (Estado, EstadoProjeto, ImagensBloco (imagemEstrela, ImagensBloco))





{-|
função que renderiza o estado inicial do Jogo 
-}

renderizaestadoInicial :: IO EstadoProjeto
renderizaestadoInicial = do
    imagemJogar' <- loadJuicy "Imagens/IconeJogar.bmp"
    imagemSair' <- loadJuicy "Imagens/IconeSair.bmp"
    blocos <- renderizaBlocos
    imagensPersonagem <- renderizaPersonagens
    let mapa = mapa1
    let imagemDefault = blank
    let imagemJogar = fromMaybe imagemDefault imagemJogar'
    let imagemSair = fromMaybe imagemDefault imagemSair'
    return $ EstadoProjeto {
      tempoTotal = 0,
      estadoAtual = Estadodomenu,
      menu = Menu { itens = [Jogar, Sair] :: [Menuescolher], opcaoEscolhida = Jogar, imagensMenu = ImagensMenu imagemJogar imagemSair},
      jogo = Jogo {
        mapa = mapa1,
        inimigos = criaInimigosIniciais,
        jogador = criaJogadorInicial mapa1,
        colecionaveis = colecionaveisInicial
      },
      blocos = blocos,
      imagensPersonagem = imagensPersonagem,
      larguraJanela = length (head blocos1) * tamanhoBloco,
      alturaJanela = length blocos1 * tamanhoBloco
    }

{-| 
Função main que renderiza o estado inicial
-}
main :: IO ()
main = do
  estadoInicial <- renderizaestadoInicial
  let
    largura = fromIntegral (larguraJanela estadoInicial)
    altura = fromIntegral (alturaJanela estadoInicial)
  play
       (InWindow "My Donkey Kong" (900,900) (100,100) )
        corfundo
        60
        estadoInicial
        renderizarProjeto
        handleTeclas
        atualizarJogo


{-|
Cor de fundo para depois utilizar na função main
-}
corfundo :: Color
corfundo = makeColor 0 0 0 0

{-|
Função que tendo em conta o estado do Projeto determina se o menu ou o jogo são renderizados
-}
renderizarProjeto :: EstadoProjeto -> Picture
renderizarProjeto estado@(EstadoProjeto { estadoAtual = EstadoJogo}) = renderizarJogo estado
renderizarProjeto estado@(EstadoProjeto{ estadoAtual = Estadodomenu}) = criaMenu  estado

{-|
Função que renderiza o menu
-}
criaMenu :: EstadoProjeto -> Picture
criaMenu estado =
  case estadoAtual estado of
    Estadodomenu -> opcoesParaSelecionar (imagemJogar (imagensMenu (menu estado))) (imagemSair (imagensMenu (menu estado))) estado
    _ -> Blank

{-| 
Função sobre as opções para selecionar no menu
-}
opcoesParaSelecionar :: Picture -> Picture -> EstadoProjeto -> Picture
opcoesParaSelecionar imgJogar imgSair estado = pictures
  [ translate (-250) (-200) (scaleImage Jogar (imagemJogar (imagensMenu (menu estado))) estado),
    translate 250 (-200) (scaleImage Sair (imagemSair (imagensMenu (menu estado))) estado)
  ]
  where
    scaleImage :: Menuescolher -> Picture -> EstadoProjeto -> Picture
    scaleImage opcao img est =
      scale (if opcaoEscolhida (menu est) == opcao then 0.6 else 0.5)
            (if opcaoEscolhida (menu est) == opcao then 0.6 else 0.5)
            (selecionar img (opcaoEscolhida (menu est) == opcao))

{-|
--função que transforma uma Maybe Picture em Picture
-}
loadJuicyToPicture :: IO (Maybe Picture) -> IO Picture
loadJuicyToPicture = fmap (fromMaybe blank)

{-|
--função que, tendo em conta se uma opcção está selecionada ou não, altera a cor da imagem
-}
selecionar :: Picture -> Bool -> Picture
selecionar img estaSelecionado = pictures
  [ color (if estaSelecionado then blue else red) $ translate (-50) 50 img
  ]

{-|
--função que renderiza o jogo ajustando as coordenadas da janela
-}
renderizarJogo :: EstadoProjeto -> Picture
renderizarJogo estado = translate (cantoXSuperior (larguraJanela estado)) (cantoYSuperior (alturaJanela estado)) $
                        pictures [ 
                          criaMapa estado, 
                          renderizarInimigos estado,
                          renderizarJogador estado,
                          desenhaEstrela estado ]

{-|
 Função para renderizar o mapa 
-}
criaMapa :: EstadoProjeto -> Picture
criaMapa estado =  pictures $ concatMap renderizaLinha (zip [0..] blocos1)
  where
    (largura, altura) = (length (head blocos1), length blocos1)

    renderizaLinha :: (Int, [Bloco]) -> [Picture]
    renderizaLinha (l, b) = zipWith (curry (renderizaBloco l)) [0..] b

    renderizaBloco :: Int -> (Int, Bloco) -> Picture
    renderizaBloco l (c, b) =
      translate
        (fromIntegral c * realToFrac pixeisPorBloco + fromIntegral tamanhoBloco/2 )
        (fromIntegral l * realToFrac (-pixeisPorBloco) - fromIntegral tamanhoBloco/2 ) $
        case b of
          Vazio -> imagemVazio (blocos estado)
          Plataforma -> imagemPlataforma (blocos estado)
          Escada -> imagemEscada (blocos estado)
          Alcapao -> imagemAlcapao (blocos estado)





{-|
Função que renderiza os inimigos
-}
renderizarInimigos :: EstadoProjeto -> Picture
renderizarInimigos estadoprojeto = pictures listaInimigosPic
               where
                      imagemDoFantasma = imagemFantasma (imagensPersonagem estadoprojeto)
                      imagemDoMacacoMalvado = imagemMacacoMalvado (imagensPersonagem estadoprojeto)
                      listaInimigosPic = [ translate 230 (-175) imagemDoFantasma,  translate 220 (-70) imagemDoMacacoMalvado]
                                   
{-|
Função que renderiza o jogador
-}
renderizarJogador :: EstadoProjeto -> Picture
renderizarJogador estadoProjeto = 
  case direcaoJogador of
      Este -> translate (convertX x) (convertY y ) $ scale (-2) 2 jogadorPic
      Oeste -> translate (convertX x) (convertY y) $ scale 2 2 jogadorPic
      Norte ->  translate (convertX x) (convertY y) $ scale 2 2 jogadorPic
      Sul ->  translate (convertX x) (convertY y) $ scale 2 2 jogadorPic
  where 
    (x, y) = posicao (jogador (jogo estadoProjeto))
    jogadorPic = imagemJogador (imagensPersonagem estadoProjeto)
    direcaoJogador = direcao (jogador (jogo estadoProjeto))
    (lx, ly) = (fromIntegral (larguraJanela estadoProjeto), fromIntegral (alturaJanela estadoProjeto))
  
{-|
Local onde se vão localizar os colecionaveis iniciais e o tipo de colecionáveis que são
-}
colecionaveisInicial :: [(Colecionavel,Posicao)]
colecionaveisInicial = [(Martelo,(1.5,1.5)),(Moeda,(5.5,5.5))]

{-|
Os inimigos iniciais do jogo
-}
criaInimigosIniciais :: [Personagem]
criaInimigosIniciais = [macacoMalvado] ++ [fantasma1]

{-|
Exemplo de Macaco Malvado
-}
macacoMalvado :: Personagem
macacoMalvado = Personagem {
                velocidade = (0,0),
                tipo = MacacoMalvado,
                posicao = ( 3.0 ,1.0),
                direcao = Oeste,
                tamanho = (1,1),
                emEscada = False,
                ressalta = True,
                vida = 1,
                pontos = 0,
                aplicaDano = (True,0)
               }

{-|
Exemplo de fantasma
-}
fantasma1 :: Personagem
fantasma1 = Personagem {
                velocidade = (5,0),
                tipo = Fantasma,
                posicao = (160,100),
                direcao = Este,
                tamanho = (0.5,0.5),
                emEscada = False,
                ressalta = True,
                vida = 1,
                pontos = 0,
                aplicaDano = (True,0)
                  }

{-|
Função que caracteriza o jogador inicial
-}
criaJogadorInicial :: Mapa -> Personagem
criaJogadorInicial (Mapa(posicaoInicial, direcao) _ _) = Personagem
 {
  velocidade = (0, 0),
  tipo = Jogador,
  posicao = posicaoInicial,
  direcao = direcao,
  tamanho = (0.9, 0.9),
  emEscada = False,
  ressalta = False,
  vida = 0,
  pontos = 0,
  aplicaDano = (False, 0)
}



{-|
Função que renderiza os blocos, ou seja, vai buscar determinada imagem tendo em conta o tipo de bloco
-}
renderizaBlocos :: IO ImagensBloco
renderizaBlocos = do
    maybeimagemPlataforma <- loadJuicy "ImagemPlataforma.bmp"
    maybeimagemAlcapao <- loadJuicy "ImagemAlcapao.bmp"
    maybeimagemVazio <- loadJuicy "ImagemEscada.bmp"
    maybeimagemEscada <- loadJuicy "ImagemVazio.bmp"
    imagemEstrela <- loadJuicy "imagemEstrela.png"

    let imagemDefault = blank
    let blocoPlataformaPic = scale 4 4 $ fromMaybe imagemDefault maybeimagemPlataforma
    let blocoAlcapaoPic = scale 4 4 $ fromMaybe imagemDefault maybeimagemAlcapao
    let blocoVazioPic =  scale 4 4 $ fromMaybe imagemDefault maybeimagemVazio
    let blocoEscadaPic = scale 4 4 $ fromMaybe  imagemDefault maybeimagemEscada
    let imagemEstrelaPic = scale 1 1 $ fromMaybe imagemDefault imagemEstrela
    let blocos = ImagensBloco blocoPlataformaPic  blocoAlcapaoPic blocoVazioPic blocoEscadaPic  imagemEstrelaPic
    return blocos
{-|
Função que renderiza todos os personagens
-}
renderizaPersonagens :: IO ImagensPersonagem
renderizaPersonagens = do
  maybejogadorPics <- loadJuicy "imagemMarioParado.bmp"
  maybefantasmaPics <- loadJuicy "ImagemFantasma.bmp"
  maybemacacoPics <- loadJuicy "ImagemMacaco.bmp"

  let imagemDefault = blank
  let jogadorPic =  scale 1 1 $ fromMaybe imagemDefault maybejogadorPics
  let fantasmaPic = scale 2 2 $ fromMaybe imagemDefault maybefantasmaPics
  let macacoPic = scale 1.5 1.5 $ fromMaybe imagemDefault maybemacacoPics
  let imagensPersonagem = ImagensPersonagem { imagemJogador = jogadorPic,
                                              imagemFantasma = fantasmaPic,
                                              imagemMacacoMalvado = macacoPic }

  return imagensPersonagem

{-|
Função que atualiza o jogo tendo em conta a passagem do tempo
-}
atualizarJogo :: Float -> EstadoProjeto -> EstadoProjeto
atualizarJogo variacaotempo estado@(EstadoProjeto {estadoAtual = estadoJogo}) = estado {
  jogo = jogoAtualizado,
  tempoTotal = tempoTotalAtualizado
}
  where jogoAtualizado = movimenta 1 (realToFrac variacaotempo) ( jogo estado)
        tempoTotalAtualizado = tempoTotal estado + variacaotempo
        atualizarJogo _ estado = estado

desenhaEstrela :: EstadoProjeto -> Picture 
desenhaEstrela estado = translate ( convertX x) (convertY y) imagemEstrelaPic
         where 
          (x,y) = (10.5,2)
          imagemEstrelaPic = imagemEstrela ( blocos estado)
