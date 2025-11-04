{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant where" #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use map" #-}
{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Tiago José Pereira Martins <a106927@alunos.uminho.pt>
              Gabriel Pinto Couto <a107373@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1
import Control.Arrow (Arrow(first))


{-| 


Função que valida um jogo.
 -}
valida :: Jogo -> Bool
valida jogo@(Jogo mapa inimigos colecionaveis jogador )= 
 chaos mapa 
 && validaressalta jogador inimigos
 && posicoesiniciais jogador inimigos 
 && numerodeinimigosminimo jogo inimigos
 && vidadosfantasmas inimigos
 && escadasdireitinhas jogo
 && validaalcapao jogo mapa jogador
 &&  colecionaveisnovazio jogo mapa colecionaveis
 && personagensnovazio jogo (jogador:inimigos) mapa
 where 
       Mapa _ _ matrizdosblocos = mapa 


{-| 
Funcao chaos que verifica se o mapa tem um chao que impede os inimigos de cairem fora do mapa.
 -}
chaos :: Mapa -> Bool
chaos mapa = all (== Plataforma) (last matrizdosblocos)
  where Mapa _ _ matrizdosblocos = mapa 

{-|
Função validaressalta que verifica se todos os inimigos em a propriedade ressalta True e se o jogador a tem False.
  -}
validaressalta :: Personagem -> [Personagem] -> Bool
validaressalta mario maus = validaressaltamario mario && validaressaltainimigos maus
 
{-| 
Função validaressaltainimigos, auxiliar da validaressalta que verifica que os inimigos tem a propriedade ressalta True.
 -}
validaressaltainimigos :: [Personagem] -> Bool
validaressaltainimigos = all ressalta

{-| 
Função validaressaltamario auxiliar da validaressalta que verifica que o jogador tem a propriedade ressalta False.
 -}
validaressaltamario :: Personagem -> Bool
validaressaltamario mario = not (ressalta mario)

{-|
Função posicoesiniciais que verifica que nenhum inimigo tem a posiçao inicial a colidir com um inimigo
-}
posicoesiniciais :: Personagem -> [Personagem]  -> Bool
posicoesiniciais _ [] = True
posicoesiniciais  mario (fantasminha:a) = colisoesPersonagens fantasminha mario  && posicoesiniciais mario a

{-|
4.Função numerodeinimigosminimo que verifica que não existem menos de 2 inimigos.
-}

numerodeinimigosminimo :: Jogo -> [Personagem] -> Bool
numerodeinimigosminimo jogo inimigos = length inimigos >= 2
{-| 
5.Função vidadosfantasmas que verifica que os inimigos têm apenas 1 vida.
-}

vidadosfantasmas :: [Personagem] -> Bool
vidadosfantasmas fantasmas = all (\j -> vida j ==1 ) bismalas
        where bismalas = filter (\h -> tipo h == Fantasma) fantasmas



{-|  
6.Função escadasdireitinhas que verifica se as escadas estão corretas e se respeitam as condições:

*Começam ou terminham numa plataforma. 
*Não começam nem terminam num alçapão.
-}
escadasdireitinhas :: Jogo -> Bool
escadasdireitinhas jogo = acabaroucomecaremplataforma jogo (h:t) && acabarouccomecouemalcapao jogo (h:t)
 where Mapa _ _ matrizdosblocos = mapa jogo
       (h:t) = comecoefimdeescadas jogo ((x,y):a) ((x1,y1):b)
       ((x,y):a) = iniciodeescadas jogo a
       ((x1,y1):b) =fimdeescadas jogo a


{-| 
Função sitiosdasescadas que lista a posiçao de todas as escadas.
-}
sitiosdasescadas :: Posicao -> [[Bloco]] -> [Posicao]
sitiosdasescadas _ [] = []
sitiosdasescadas (x,y) (h:t) = sitiosdasescadaslinha (x,y) h ++ sitiosdasescadas (x,y-1) t
{-|  
Função sitiosdasescadasliha auxiliar da sitiosdasescadas.
-}
sitiosdasescadaslinha :: Posicao -> [Bloco] -> [Posicao]
sitiosdasescadaslinha _ [] = []
sitiosdasescadaslinha (x,y) (h:t) = if h==Escada then (x,y) : sitiosdasescadaslinha (x+1,y) t else sitiosdasescadaslinha (x+1,y) t

{-|
Função que lista a posiçao de todos os alçapões.
-}
sitiosdosalcapoes :: Posicao -> [[Bloco]] -> [Posicao]
sitiosdosalcapoes _ [] = []
sitiosdosalcapoes (x,y) (h:t) = sitiosdosalcapoeslinha (x,y) h ++ sitiosdosalcapoes (x,y+1) t

{-| 
Função sitiosdosalcapoeslinha auxiliar da sitiosdosalcapoes.
 -}
sitiosdosalcapoeslinha:: Posicao -> [Bloco] -> [Posicao]
sitiosdosalcapoeslinha _ [] = []
sitiosdosalcapoeslinha (x,y) (h:t) = if h==Alcapao then (x,y) : sitiosdosalcapoeslinha (x+1,y) t else sitiosdosalcapoeslinha (x+1,y) t

{-|
Função sitiosdasplataformas que lista a posiçao de todas as plataformas.
  -}
--função que lista a posiçao de todas as plataformas
sitiosdasplataformas :: Posicao -> [[Bloco]] -> [Posicao]
sitiosdasplataformas  _ [] = []
sitiosdasplataformas  (x,y) (h:t) = sitiosdasplataformaslinha (x,y) h ++ sitiosdasplataformas (x,y+1) t
{-| 
Função sitiosdasplataformaslinha auxiliar da sitiosdasplataformas.
 -}
sitiosdasplataformaslinha :: Posicao -> [Bloco] -> [Posicao]
sitiosdasplataformaslinha _ [] = []
sitiosdasplataformaslinha (x,y) (h:t) = if h==Plataforma then (x,y) : sitiosdasplataformaslinha (x+1,y) t else sitiosdasplataformaslinha (x+1,y) t
{-|
Função fimdeescadas que lista a posição de todos os sitios finais das escadas.
  -}
fimdeescadas :: Jogo -> [Posicao] -> [Posicao]
fimdeescadas jogo ((x,y):b) = if elem (x,y+1) b then fimdeescadas jogo b else (x,y) : fimdeescadas jogo b
 where ((x,y):b) = sitiosdasescadas (x,y) matrizdosblocos
       Mapa _ _ matrizdosblocos = mapa jogo 

{-| 
Função iniciodeescadas que lista a posição de todos os sitios inicios das escadas.
 -}
iniciodeescadas :: Jogo -> [Posicao] -> [Posicao]
iniciodeescadas _ [] = []
iniciodeescadas jogo ((x,y):b) = if elem (x,y-1) b then fimdeescadas jogo b else (x,y) : fimdeescadas jogo b
 where ((x,y):b) = sitiosdasescadas (x,y) matrizdosblocos
       Mapa _ _ matrizdosblocos = mapa jogo

{-| 
Função comecoefimdeescadas que devolve uma lista de pares com todos os inicios e finais de cada escada.
 -}
comecoefimdeescadas :: Jogo -> [Posicao] -> [Posicao] -> [(Posicao,Posicao)]
comecoefimdeescadas _ [] _ = []
comecoefimdeescadas _ _ [] = []
comecoefimdeescadas _ [] [] = []
comecoefimdeescadas jogo ((x,y):a) ((x1,y1):b) = if x==x1 then ((x,y),(x1,y1)) : comecoefimdeescadas jogo a b 
                                                 else comecoefimdeescadas jogo ((x,y):a) (b++ [(x1,y1)])
 where ((x,y):a) = iniciodeescadas jogo a
       ((x1,y1):b) =fimdeescadas jogo a

{-|  
Função acabaroucomecaremplataforma que verifica se uma escada começou ou acabou com uma plataforma e usa a acabaroucomecaremplataformaa como auxiliar.
-}
--funçao que verifica se uma escada começou ou acabou com uma plataforma
acabaroucomecaremplataforma :: Jogo -> [(Posicao,Posicao)] -> Bool
acabaroucomecaremplataforma jogo (b : a) = acabaroucomecaremplataformaa b || acabaroucomecaremplataforma jogo a
 where
 acabaroucomecaremplataformaa :: (Posicao,Posicao) -> Bool
 acabaroucomecaremplataformaa ((x,y),(x1,y1)) =  ( fst ( verificaBloco (x,y-1) matrizdosblocos ) == Plataforma )
                                             ||( fst (verificaBloco (x1,y1+1) matrizdosblocos) == Plataforma )
 Mapa _ _ matrizdosblocos = mapa jogo
 

{-|  
Função verificaBloco auxiliar da acabaroucomecaremplataforma que recebe uma posição e devolve o tipo de bloco nesta posição.
-}
verificaBloco :: (Double,Double) -> [[Bloco]] -> (Bloco, Posicao)
verificaBloco (x, y) matriz = ((matriz !! round y) !! round x, (x,y))

{-| 
7. Função validaalcapao que verifica se os alçapões são mais largos que o jogador.
 -}
validaalcapao :: Jogo -> Mapa -> Personagem -> Bool
validaalcapao jogo a b = acabaroucomecaremplataforma jogo (comecoefimdeescadas jogo (iniciodeescadas jogo (sitiosdasescadas (0,0) matrizdosblocos)) (fimdeescadas jogo (sitiosdasescadas (0,0) matrizdosblocos)))
  && acabarouccomecouemalcapao jogo (comecoefimdeescadas jogo (iniciodeescadas jogo (sitiosdasescadas (0,0) matrizdosblocos)) (fimdeescadas jogo (sitiosdasescadas (0,0) matrizdosblocos))) 
  && tamanhodealcapoescerto jogo (comecoefimdeescadas jogo (iniciodeescadas jogo (sitiosdasescadas (0,0) matrizdosblocos)) (fimdeescadas jogo (sitiosdasescadas (0,0) matrizdosblocos))) b
  where  a@(Mapa _ _ matrizdosblocos) = mapa jogo


{-| 
Função acabarouccomecouemalcapao que verifica que uma escada nao começa nem acaba com um alçapão e usa como auxiliar a função acabarouccomecouemalcapaoo.
 -}
acabarouccomecouemalcapao :: Jogo -> [(Posicao,Posicao)] -> Bool
acabarouccomecouemalcapao jogo (b : a) = acabarouccomecouemalcapaoo jogo b || acabarouccomecouemalcapao jogo a
 where
 acabarouccomecouemalcapaoo :: Jogo -> (Posicao,Posicao) -> Bool
 acabarouccomecouemalcapaoo jogo ((x,y),(x1,y1)) = ( fst ( verificaBloco (x,y-1) matrizdosblocos ) == Alcapao )
                                             ||( fst (verificaBloco (x1,y1+1) matrizdosblocos) == Alcapao )
 Mapa _ _ matrizdosblocos = mapa jogo


 


{-| 
Função comecosdealcapoes que lista todos os começos de alcapoes.
 -}
comecosdealcapoes :: Jogo -> [Posicao] -> [Posicao]
comecosdealcapoes _ [] = []
comecosdealcapoes jogo ((x,y):a) = if elem (x-1,y) a then comecosdealcapoes jogo ((x-1,y):a) else (x,y) : comecosdealcapoes jogo a
 where ((x,y):a) = sitiosdosalcapoes i matrizdosblocos
       i = (0.5,0.5)
       Mapa _ _ matrizdosblocos = mapa jogo



{-| 
Função fimdealcapoes que lista todos os finais de alcapões.
 -}
fimdealcapoes :: Jogo -> [Posicao] -> [Posicao]
fimdealcapoes _ [] = []
fimdealcapoes jogo ((x,y):a) = if elem (x+1,y) a then fimdealcapoes jogo ((x+1,y):a) else (x,y) : fimdealcapoes jogo a
 where ((x,y):a) = sitiosdosalcapoes i matrizdosblocos
       i = (0.5,0.5)
       Mapa _ _ matrizdosblocos = mapa jogo

{-| 
Função comecosefinsdealcapoes que devolve uma lista de pares com os inicios e finais de todos os alçapões.
 -}
comecosefinsdealcapoes :: Jogo -> [Posicao] -> [Posicao]-> [(Posicao,Posicao)]
comecosefinsdealcapoes _ [] [] = []
comecosefinsdealcapoes jogo ((x,y):a) ((x1,y1):(x2,y2):b)   = if (x1-x)<(x2-x) then ((x,y),(x1,y1)) : comecosefinsdealcapoes jogo a ((x2,y2):b)
              else comecosefinsdealcapoes jogo ((x,y):a)  ((x2,y2):b++[(x1,y1)])
  where ((x,y):a) = comecosdealcapoes jogo (sitiosdosalcapoes i matrizdosblocos)
        ((x1,y1):(x2,y2):b) = fimdealcapoes jogo (sitiosdosalcapoes i matrizdosblocos)
        i = (0.5,0.5)
        Mapa _ _ matrizdosblocos = mapa jogo


{-|
Função larguradepersonagem que diz qual é a largura do personagem.
  -}
larguradepersonagem :: Hitbox -> Double
larguradepersonagem ((x,y),(x1,y1)) = abs (x1-x)


{-| 
Função tamanhodealcapoescerto que valida se o alçapão tem o caminho correto.
 -}
tamanhodealcapoescerto :: Jogo -> [(Posicao,Posicao)] -> Personagem -> Bool
tamanhodealcapoescerto jogo (((x,y),(x1,y1)):o) a = abs (x-x1) < larguradomario  && tamanhodealcapoescerto jogo o a
 where (((x,y),(x1,y1)):o) = comecosefinsdealcapoes jogo h t
       (larguradomario,_) = tamanho a
       h = comecosdealcapoes jogo (sitiosdosalcapoes i matrizdosblocos)
       i = (0.5,0.5)
       t = fimdealcapoes jogo (sitiosdosalcapoes i matrizdosblocos)
       Mapa _ _ matrizdosblocos = mapa jogo



{-| 
Função sitiosvazios que lista todas as posiçoes dos sitios vazios no mapa.
 -}
sitiosvazios :: Posicao -> [[Bloco]] -> [Posicao]
sitiosvazios _ [] = []
sitiosvazios (x,y) (h:t) = sitiosvazioslinha (x,y) h ++ sitiosvazios (x,y+1) t

{-| 
Função sitiosvazioslinha auxiliar da função sitios vazios.
-}
sitiosvazioslinha :: Posicao -> [Bloco] -> [Posicao]
sitiosvazioslinha _ [] = []
sitiosvazioslinha (x,y) (h:t) = if h==Vazio then (x,y) : sitiosvazioslinha (x+1,y) t else sitiosvazioslinha (x+1,y) t


{-|
8. Função colecionaveisnovazio que verifica que todos os colecionaveis estao no vazio.
  -}
colecionaveisnovazio:: Jogo -> Mapa ->[(Colecionavel, Posicao)] -> Bool
colecionaveisnovazio jogo a colecionaveis  = all (colecionavelnovazio jogo a) colecionaveis

{-| 
Função colecionavelnovazio auxiliar da função colecionaveisnovazio.
 -}
colecionavelnovazio :: Jogo -> Mapa ->(Colecionavel, Posicao)-> Bool
colecionavelnovazio jogo b (c,(x,y))  = elem (x,y) a
 where --((_,(x,y)):_) = (\h colecionaveis h =     
       a = sitiosvazios i matrizdosblocos
       i = (0,0)
       b@(Mapa _ _ matrizdosblocos) = mapa jogo


{-| 
Função intersetam que verifica se uma hitbox se interseta com uma lista de hitboxs.
 -}
intersetam :: Hitbox -> [Hitbox] -> Bool
intersetam _ [] = True
intersetam a (h:t) = interseta a h && intersetam a t  



{-| 
Função personagemnovazio que verifica que o jogador spawna no vazio.
 -}
personagemnovazio :: Jogo -> Personagem -> Mapa -> Bool
personagemnovazio jogo a  u = all (Vazio==) (blocosDentroDaHitboxDoPersonagem jogo a u) 


{-| 
Funcão personagensnovazio para garantir que todos os personagens tao no vazio apartir de uma lista com todos os personagens e uma lista com todos os blocos do mapa.
 -}
personagensnovazio :: Jogo -> [Personagem] -> Mapa -> Bool
personagensnovazio _ [] _ = True
personagensnovazio jogo (h:t) a = personagemnovazio jogo h a && personagensnovazio jogo t a 


{-| 
Funcão blocosDentroDaHitboxDoPersonagem que ve que blocos estao dentro da hitbox do personagem.
 -}
blocosDentroDaHitboxDoPersonagem :: Jogo -> Personagem -> Mapa -> [Bloco]
blocosDentroDaHitboxDoPersonagem jogo personagem a = [matrizdosblocos !! y !! x | x <- [xI..xS], y <- [yI.. yS]]
                                 where  ((xi,yi),(xs,ys)) = calchitbox personagem
                                        a@(Mapa _ _ matrizdosblocos) = mapa jogo
                                        larguradamatriz = fromIntegral (length (head matrizdosblocos))
                                        alturadamatriz = fromIntegral (length matrizdosblocos)
                                        xI = max 0 ( ceiling xi)
                                        yI = max 0 (ceiling yi)
                                        xS = min larguradamatriz (floor xs)
                                        yS = min alturadamatriz (floor ys)
                                        
                                        
                                        
                                        

