{-|
Module : Main
Description : Módulo Haskell que contém o código do Jogo Sokoban
Copyright : José Carlos Lima Martins <zeclmartins@gmail.com>

Módulo contendo a realização do jogo Sokoban.
-}
module Main where

import Graphics.Gloss -- interface principal gloss
import Graphics.Gloss.Data.Picture -- para desenhar @Picture@s
import Graphics.Gloss.Interface.Pure.Game -- para reagir a @Event@s
import qualified Data.Text as T
import Data.Char
import System.Directory

-- | Estado do jogo:
--
-- * Dimensões do mapa
-- * lista de Coordenadas do boneco no mapa
-- * lista de lista de Coordenadas
-- * Score
-- * Mapa após mover
-- * Mapa sem cardinais
-- * Se está ou não completo o nível
-- * lista com os movimentos já feitos
-- * número do mapa
type Mapa = ((Int,Int),[(Int,Int)],[[(Int,Int)]],Int,[String],[String],Bool,Int)

-- | tipo criado em que U (Up), D(Down), L(Left), R(Right) e Nada(caso não seja nenhum dos casos)
data Command = U | D | L | R | Nada
     deriving (Show,Eq)

-- | largura e altura do boneco, das caixas e das paredes em Int
tBCPi :: Int
tBCPi = 40

-- | largura e altura do boneco, das caixas e das paredes em Float
tBCPf :: Float
tBCPf = 40

-- | função principal que invoca o jogo.
main :: IO ()
main = do
         title <- loadBMP "./images/title.bmp"
         mapas <- loadMapas
         theme <- escolheTema 
         (mcp,lMapas,nMapa) <- escolheNivel mapas
         mapaInicial <- return (defMI mcp nMapa)
         joga mapaInicial (desenhaMapa theme title) (reageF lMapas)

-- | load dos mapas a partir de uma lista com o nome dos mesmos
loadMapaRec :: [FilePath] -> Int -> IO ([(String,Int)])
loadMapaRec [] n = return []
loadMapaRec [path] n = do
                        mapa <- readFile ("./levels/" ++ path)
                        let linhasM = lines mapa
                        let erros = verificaMapa linhasM
                        if(erros/=[]) then putStr (path ++ " errors in lines:" ++ concat erros ++ "\nLevel not loaded!\n")
                        else putStr [] 
                        if (erros==[]) then return [(mapa,n)]
                        else return []
loadMapaRec (path:paths) n = do
                            mapa <- readFile ("./levels/" ++ path)
                            let linhasM = lines mapa
                            let erros = verificaMapa linhasM
                            if(erros/=[]) then putStr (path ++ " errors in lines:" ++ concat erros ++ "\nLevel not loaded!\n")
                            else putStr []
                            mapas <- if(erros==[]) then loadMapaRec paths (n+1)
                                                   else loadMapaRec paths n
                            if (erros==[]) then return ((mapa,n):mapas)
                                           else return mapas

-- | load dos mapas para o jogo
loadMapas :: IO ([(String,Int)])
loadMapas = do
              paths <- listDirectory "./levels/"
              mapas <- loadMapaRec paths 0
              if(mapas==[]) then error "No levels or invalid levels on levels folder!!"
              else return mapas

-- | possibilita o jogador escolher o tema
escolheTema :: IO (Picture,Picture,Picture,Picture,Picture)
escolheTema = do
                putStrLn "Select theme (Classic,Mario,Luigi,Zelda,Link): "
                theme <- getLine
                if theme=="Classic" 
                then do
                       boneco <- loadBMP "./images/classicB.bmp"
                       caixa <- loadBMP "./images/classicC.bmp"
                       parede <- loadBMP "./images/classicP.bmp"
                       pf <- loadBMP "./images/classicPF.bmp"
                       caixaf <- loadBMP "./images/classicCF.bmp"
                       return (boneco,caixa,parede,pf,caixaf)
                else if (theme=="Mario"  || theme=="Luigi")
                     then do
                            boneco <- if(theme=="Mario")
                                      then loadBMP "./images/marioB.bmp"
                                      else loadBMP "./images/luigiB.bmp"
                            caixa <- loadBMP "./images/marioC.bmp"
                            parede <- loadBMP "./images/marioP.bmp"
                            pf <- loadBMP "./images/marioPF.bmp"
                            caixaf <- loadBMP "./images/marioCF.bmp"
                            return (boneco,caixa,parede,pf,caixaf)
                     else if (theme=="Zelda" || theme=="Link")
                          then do
                                  boneco <- if(theme=="Zelda")
                                            then loadBMP "./images/ZeldaB.bmp"
                                            else loadBMP "./images/linkB.bmp"
                                  caixa <- loadBMP "./images/linkC.bmp"
                                  parede <- loadBMP "./images/linkP.bmp"
                                  pf <- loadBMP "./images/linkPF.bmp"
                                  caixaf <- loadBMP "./images/linkCF.bmp"
                                  return (boneco,caixa,parede,pf,caixaf)
                          else escolheTema                             

escolheNivel :: [(String,Int)] -> IO ((String,[(String,Int)],Int))
escolheNivel ((mapa,n):mapas) = return (mapa,((mapa,n):mapas),n)

-- | define o mapa inicial e alguns valores necessários
defMI :: String -> Int -> Mapa
defMI mcp nMapa = (tMapa,[posBoneco],[coordsCaixas],0,mSimplificado,removeTab,verificarCI mSimplificado,nMapa)
            where
            mcpL = lines mcp
            (tab,coords) = parteMapa mcpL
            tt = contaListasMapa tab
            (bx,by) = head (tuploCoord coords)
            coordsCaixas = drop 1 (tuploCoord (remInc coords))
            removeTab = removeCardinais tab (daCoords 0 tab 0) tab
            mSimplificado = colocaCaixas removeTab coordsCaixas 
            ts = let (x:xs)=tab
                 in length x
            tMapa = (ts*tBCPi,tt*tBCPi)
            posBoneco = (-(div (fst tMapa) 2)+bx*tBCPi,-(div (snd tMapa) 2)+(by+1)*tBCPi) 

-- | Função que cria um jogo.
joga :: mundo -> (mundo -> Picture) -> (Event -> mundo -> mundo) -> IO ()
joga mapaInicial desenha reage = play
    (InWindow "Sokoban" (1024, 768) (0, 0)) -- Tamanho da janela do jogo
    (white) -- Côr do fundo da janela
    45 -- refresh rate
    mapaInicial -- mapa inicial
    desenha -- função que desenha o mapa
    reage -- função que reage a um evento (carregar numa tecla, mover o rato, etc)
    reageTempo -- função que reage ao passar do tempo

-- | Não reage ao passar do tempo.
reageTempo :: Float -> mundo -> mundo
reageTempo t m = m

-- | Desenha o jogo dentro da janela
desenhaMapa :: (Picture,Picture,Picture,Picture,Picture) -> Picture -> Mapa -> Picture
desenhaMapa (boneco,caixa,parede,pf,caixaf) title ((xMapa,yMapa),((x,y):t),_,n,ms,_,fon,_) = Pictures [instrucoes,titulo,made,paredesCPf,figuraBoneco,score,sG]
    where
    xPosition = ((toEnum xMapa)/2)
    -- desenha Titulo do Jogo
    titulo = Translate (xPosition+115) 215 $ Scale (0.25) (0.25) $ title
    --made by
    made = Color black $ Pictures [Translate (xPosition+32) (150) $ Scale (0.15) (0.15) $ Text "made by: Jose Carlos Lima Martins"]
    -- desenhas as instruçoes
    instrucoes = Pictures [rectangle,insmapaPrevious,insmapaNext,insundo,insrestart]
    rectangle = Translate (xPosition+30) 0 $ Color (makeColor 0 0 0 0.2) $ Polygon [(0,-500),(400,-500),(400,500),(0,500)]
    insmapaPrevious = Color black $ Translate (xPosition+32) (-40) $ Scale (0.15) (0.15) $ Text "'b' : prev level"
    insmapaNext = Color black $ Translate (xPosition+32) (-80) $ Scale (0.15) (0.15) $ Text "'n' : next level"
    insundo = Color black $ Translate (xPosition+32) (-120) $ Scale (0.15) (0.15) $ Text "'u' : undo"
    insrestart = Color black $ Translate (xPosition+32) (-160) $ Scale (0.15) (0.15) $ Text "'r' : restart"
    -- desenha boneco
    figuraBoneco = Translate (toEnum x) (toEnum y) boneco
    -- desenha caixas, paredes e posiçoes finais
    paredesCPf = desenhaTab ms (-((fromIntegral xMapa)/2),(fromIntegral yMapa)/2) (parede,pf,caixa,caixaf)
    -- desenha score
    score = Color red $ Translate (xPosition+40) (0) $ Scale (0.25) (0.25) $ Text ("Moves: " ++ show n)
    -- desenha estado do nivel
    sG = stateGame (xMapa,yMapa) fon

-- | Desenha as caixas, paredes e posiçoes finais
desenhaTab :: [String] -> (Float,Float) -> (Picture,Picture,Picture,Picture) -> Picture
desenhaTab [] _ _ = (Pictures [])
desenhaTab (h:t) (x,y) figuras = Pictures [desenhaLinha h (x,y) figuras, desenhaTab t (x,y-tBCPf) figuras]

-- | Desenha uma linha de caixas, paredes e posiçoes finais
desenhaLinha :: String -> (Float,Float) -> (Picture,Picture,Picture,Picture) -> Picture
desenhaLinha [] _ _ = (Pictures [])
desenhaLinha (c:cs) (x,y) (parede,pf,caixa,caixaf)
        |(ord c)==35 = Pictures [Translate x y (parede), desenhaL]
        |(ord c)==46 = Pictures [Translate x y (pf), desenhaL]
        |(ord c)==72 = Pictures [Translate x y (caixa), desenhaL]
        |(ord c)==73 = Pictures [Translate x y (caixaf), desenhaL]
        |otherwise = desenhaL
        where
            desenhaL = desenhaLinha cs (x+tBCPf,y) (parede,pf,caixa,caixaf)

-- | desenha o estado do jogo
stateGame ::(Int,Int) -> Bool -> Picture
stateGame (xMapa,yMapa) fon = if fon==True 
                              then  Color (makeColorI 0 128 0 1) $
                                    Pictures [Translate (((toEnum xMapa)/2)+30) (80) $
                                              Scale (0.15) (0.15) $
                                              Text "LEVEL COMPLETE.", Translate (((toEnum xMapa)/2)+30) (60) $
                                              Scale (0.15) (0.15) $
                                              Text "Try another one ('b','n')."]
                              else  Translate (((toEnum xMapa)/2)+30) (80) $
                                    Scale (0.15) (0.15) $
                                    Color red $
                                    Text "Level incomplete..."

-- | funçao que reage ao 'teclar' do jogador
reageF :: [(String,Int)] -> Event -> Mapa -> Mapa
reageF lMapas (EventKey (Char 'r') Down _ _) mapaIns = restart mapaIns --restart
reageF lMapas (EventKey (Char 'R') Down _ _) mapaIns = restart mapaIns --restart
reageF lMapas (EventKey (Char 'u') Down _ _) (tMapa,coordsBoneco,[c],n,ms,tab,fon,nMapa) = (tMapa,coordsBoneco,[c],n,ms,tab,fon,nMapa) -- undo
reageF lMapas (EventKey (Char 'U') Down _ _) (tMapa,coordsBoneco,[c],n,ms,tab,fon,nMapa) = (tMapa,coordsBoneco,[c],n,ms,tab,fon,nMapa) -- undo
reageF lMapas (EventKey (Char 'u') Down _ _) (tMapa,coordsBoneco,(c:cs),n,ms,tab,fon,nMapa) = undo (tMapa,coordsBoneco,cs,n,ms,tab,fon,nMapa) -- undo
reageF lMapas (EventKey (Char 'U') Down _ _) (tMapa,coordsBoneco,(c:cs),n,ms,tab,fon,nMapa) = undo (tMapa,coordsBoneco,cs,n,ms,tab,fon,nMapa) -- undo
reageF lMapas e (tMapa,coordsBoneco,(c:cs),n,ms,tab,fon,nMapa)
      |fon==True = mcpMapa lMapas nMapa (mapaIns) e
      |otherwise = mcpMapa lMapas nMapa (reageEvento e mapaIns) e
      where mapaIns = (tMapa,coordsBoneco,(c:cs),n,ms,tab,fon,nMapa)

-- | funçao restart que reinicia o nivel
restart :: Mapa -> Mapa
restart (tMapa,coordsBoneco,[c],n,ms,tab,fon,nMapa) = (tMapa,coordsBoneco,[c],n,ms,tab,fon,nMapa)
restart (tMapa,coordsBoneco,(c:cs),n,ms,tab,fon,nMapa) = restart (undo (tMapa,coordsBoneco,cs,n,ms,tab,fon,nMapa))

-- | funçao undo que volta um passo atrás
undo :: Mapa -> Mapa
undo (tMapa,[(xBoneco,yBoneco)],[coords],n,ms,tab,fon,nMapa) = (tMapa,[(xBoneco,yBoneco)],[coords],n,ms,tab,fon,nMapa)
undo (tMapa,((xBoneco,yBoneco):t),(c:cs),n,ms,tab,fon,nMapa) = (tMapa,t,(c:cs),n-1,colocaCaixas tab c,tab,verificarCI (colocaCaixas tab c),nMapa)

-- | obtem o mapa consoante o número da lista de mapas
getMapa :: [(String,Int)] -> Int -> String
getMapa [] x = []
getMapa ((mapa,n):mapas) x = if(n==x) then mapa
                             else getMapa mapas x

-- | permite passar para o mapa anterior ou seguinte
mcpMapa :: [(String,Int)] -> Int -> Mapa -> Event -> Mapa
mcpMapa lMapas nMapa fazmove (EventKey (Char 'n') Down _ _) = if(getMapa lMapas (nMapa+1)==[]) then defMI newMapaF 0
                                                              else defMI newMapa (nMapa+1)
                                                              where (newMapaF,n) = head(lMapas)
                                                                    newMapa = getMapa lMapas (nMapa+1)
mcpMapa lMapas nMapa fazmove (EventKey (Char 'b') Down _ _) = if(nMapa==0) then defMI newMapaL nL
                                                              else defMI newMapa (nMapa-1)
                                                              where (newMapaL,nL) = last(lMapas)
                                                                    newMapa = getMapa lMapas (nMapa-1)
mcpMapa _ nMapa fazmove _ = fazmove

-- | Reage ao pressionar das setas do teclado, movendo o boneco 40 pixéis numa direção
reageEvento :: Event -> Mapa -> Mapa
reageEvento (EventKey (SpecialKey KeyDown)    Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCD1 == '#' = mapaI
                                |devolveCD1 == 'H' || devolveCD1 == 'I' = if(devolveCD2 == '#' || devolveCD2 == 'H' || devolveCD2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBD:coordsB,mudaCD:coords,n+1, mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                |otherwise = (tMapa,moveBD:coordsB,mudaCD:coords,n+1,mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                mapaComCaixas = colocaCaixas tab mudaCD
                                coordX = convX tMapa xBoneco
                                coordY = convY tMapa yBoneco
                                devolveCD1 = devolveCaracterB (contalinhasB (reverse ms) (coordY-1) 0) coordX 0
                                devolveCD2 = devolveCaracterB (contalinhasB (reverse ms) (coordY-2) 0) coordX 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBD = moveBoneco (0,-tBCPi) (xBoneco,yBoneco)
                                mudaCD = mudaCoordsCaixas coordX (coordY-1) (head coords) D
reageEvento (EventKey (SpecialKey KeyUp)  Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCU1 == '#' = mapaI
                                |devolveCU1 == 'H' || devolveCU1 == 'I' = if(devolveCU2 == '#' || devolveCU2 == 'H' || devolveCU2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBU:coordsB,mudaCU:coords,n+1,mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                |otherwise = (tMapa,moveBU:coordsB,mudaCU:coords,n+1,mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                mapaComCaixas = colocaCaixas tab mudaCU
                                coordX = convX tMapa xBoneco
                                coordY = convY tMapa yBoneco
                                devolveCU1 = devolveCaracterB (contalinhasB (reverse ms) (coordY+1) 0) coordX 0
                                devolveCU2 = devolveCaracterB (contalinhasB (reverse ms) (coordY+2) 0) coordX 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBU = moveBoneco (0,tBCPi) (xBoneco,yBoneco)
                                mudaCU = mudaCoordsCaixas coordX (coordY+1) (head coords) U
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCL1 == '#' = mapaI
                                |devolveCL1 == 'H' || devolveCL1 == 'I' = if(devolveCL2 == '#' || devolveCL2 == 'H' || devolveCL2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBL:coordsB,mudaCL:coords,n+1,mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                |otherwise = (tMapa,moveBL:coordsB,mudaCL:coords,n+1,mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                mapaComCaixas = colocaCaixas tab mudaCL
                                coordX = convX tMapa xBoneco
                                coordY = convY tMapa yBoneco
                                devolveCL1 = devolveCaracterB (contalinhasB (reverse ms) coordY 0) (coordX-1) 0
                                devolveCL2 = devolveCaracterB (contalinhasB (reverse ms) coordY 0) (coordX-2) 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBL = moveBoneco (-tBCPi,0) (xBoneco,yBoneco)
                                mudaCL = mudaCoordsCaixas (coordX-1) coordY (head coords) L
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                |devolveCR1 == '#' = mapaI
                                |devolveCR1 == 'H' || devolveCR1 == 'I' = if(devolveCR2 == '#' || devolveCR2 == 'H' || devolveCR2 == 'I') 
                                                                          then mapaI
                                                                          else (tMapa,moveBR:coordsB,mudaCR:coords,n+1,mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                |otherwise = (tMapa,moveBR:coordsB,mudaCR:coords,n+1,mapaComCaixas,tab,verificarCI mapaComCaixas,nMapa)
                                where
                                ((xBoneco,yBoneco):t) = coordsB
                                mapaComCaixas = colocaCaixas tab mudaCR
                                coordX = convX tMapa xBoneco
                                coordY = convY tMapa yBoneco
                                devolveCR1 = devolveCaracterB (contalinhasB (reverse ms) coordY 0) (coordX+1) 0
                                devolveCR2 = devolveCaracterB (contalinhasB (reverse ms) coordY 0) (coordX+2) 0
                                mapaI = (tMapa,coordsB,coords,n,ms,tab,fon,nMapa)
                                moveBR = moveBoneco (tBCPi,0) (xBoneco,yBoneco)
                                mudaCR = mudaCoordsCaixas (coordX+1) coordY (head coords) R
reageEvento _ mapa = mapa -- ignora qualquer outro evento

-- | Move o boneco uma coordenada para o lado
moveBoneco :: (Int,Int) -> (Int,Int) -> (Int,Int)
moveBoneco (x,y) (xBoneco,yBoneco) = (x + xBoneco,y + yBoneco)

-- | Converte de Pixeis para coordenada x do mapa em listas de strings
convX :: (Int,Int) -> Int -> Int
convX tMapa x = div (x+(div (fst tMapa) 2)) tBCPi

-- | Converte de Pixeis para coordenada y do mapa em listas de strings
convY :: (Int,Int) -> Int -> Int
convY tMapa y = (div (y+(div (snd tMapa) 2)) tBCPi)-1

-- | Divide o mapa em tabuleiro e coordenadas. Para isso usamos a função splitAt...
--
-- @
-- splitAt :: Int -> [a] -> ([a], [a])
-- @
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
parteMapa :: [String] -> ([String],[String])
parteMapa [] = ([],[])
parteMapa (x:xs) = splitAt (contaListasMapa (x:xs)) (x:xs)

{- | Devolve o nº de linhas do tabuleiro.

Nesta função usamos /isDigit/ pertencente ao Data.Char para verificarmos se o elemento é ou não um algarismo (0,...9). -}
contaListasMapa :: [String] -> Int
contaListasMapa [] = 0
contaListasMapa (x:xs)
        |(null x)==True || isDigit (head x)==True = contaListasMapa xs 
        |otherwise = 1 + contaListasMapa xs

-- | Converte uma String numa lista de dois números.
sToI :: String -> [Int]
sToI [] = []
sToI (x:xs) = aux (words (x:xs))
          where aux :: [String] -> [Int]
                aux [] = []
                aux (x:xs) = (read x :: Int):aux xs

-- | Recebe uma lista de dois numeros tuplos.
liToT :: [Int] -> (Int,Int)
liToT l = if (null l) then (-1,-1) 
          else (head l,last l)

-- | Dá as coordenadas de cada caracter do mapa numa lista.
daCoords :: Int -> [String] -> Int -> [(Int,Int)]
daCoords n [] p = []
daCoords n (h:t) p = (aux n h p) ++ (daCoords n t (p+1))
          where aux ::Int -> String -> Int -> [(Int,Int)]
                aux n [] p = []
                aux n (x:xs) p = (n,p):(aux (n+1) xs p) 

-- | Remove os cardinais desnecessários do tabuleiro.
removeCardinais :: [String] -> [(Int,Int)] -> [String] -> [String]
removeCardinais [] cd tb = []
removeCardinais (h:t) cd tb = (removedalinha h cd tb):(removeCardinais t (drop (aux h) cd) tb)
                  where aux :: String -> Int
                        aux [] = 0
                        aux (h:t) = 1+(aux t)

-- | Remove os cardinais desnecessários de uma linha...
removedalinha :: String -> [(Int,Int)] -> [String] -> String
removedalinha [] cd tb = [] 
removedalinha (h:t) cd tb = (aux h cd tb):(removedalinha t (drop 1 cd) tb)
                    where aux :: Char -> [(Int,Int)] -> [String] -> Char
                          aux h cd tb 
                                |ordCar == 46 = '.'
                                |ordCar == 32 = ' '
                                |ordCarD == 32 || ordCarR == 32 ||  ordCarDR == 32 || ordCarDL == 32 || ordCarL == 32 || ordCarU == 32 || ordCarUR == 32 || ordCarUL == 32 = '#'
                                |ordCarD == 46 || ordCarR == 46 ||  ordCarDR == 46 || ordCarDL == 46 || ordCarL == 46 || ordCarU == 46 || ordCarUR == 46 || ordCarUL == 46 = '#'
                                |otherwise = ' '
                              where
                                cdH = head(cd)
                                cdHF = fst(cdH)
                                cdHS = snd(cdH)
                                ordCar = ord (devolveCaracterB (contalinhasB tb cdHS 0) cdHF 0)
                                ordCarD = ord (devolveCaracterB (contalinhasB tb (cdHS-1) 0) cdHF 0)
                                ordCarR = ord (devolveCaracterB (contalinhasB tb cdHS 0) (cdHF+1) 0)
                                ordCarDR = ord (devolveCaracterB (contalinhasB tb (cdHS-1) 0) (cdHF+1) 0)
                                ordCarDL = ord (devolveCaracterB (contalinhasB tb (cdHS-1) 0) (cdHF-1) 0)
                                ordCarL = ord (devolveCaracterB (contalinhasB tb cdHS 0) (cdHF-1) 0)
                                ordCarU = ord (devolveCaracterB (contalinhasB tb (cdHS+1) 0) cdHF 0)
                                ordCarUR = ord (devolveCaracterB (contalinhasB tb (cdHS+1) 0) (cdHF+1) 0)
                                ordCarUL = ord (devolveCaracterB (contalinhasB tb (cdHS+1) 0) (cdHF-1) 0)

-- | Função que devolve o caracter de uma certa lista.
devolveCaracterB :: String -> Int -> Int -> Char
devolveCaracterB [] p n = '#'
devolveCaracterB (x:xs) p n
        |n==p = x
        |otherwise =devolveCaracterB xs p (n+1)

-- | Devolve a linha do nº introduzido contando de cima para baixo.
contalinhasB :: [String] -> Int -> Int -> String
contalinhasB [] p n = [] 
contalinhasB (x:xs) p n
        |n==p = x
        |otherwise = contalinhasB xs p (n+1)

-- | Transforma as strings das coordenadas numa lista de tuplos.
tuploCoord :: [String] -> [(Int,Int)] 
tuploCoord [] = []
tuploCoord (x:xs) = liToT (sToI x):tuploCoord xs

-- | muda as coordenadas das caixas caso o boneco empurre uma delas
mudaCoordsCaixas :: Int -> Int -> [(Int,Int)] -> Command -> [(Int,Int)]
mudaCoordsCaixas x y [] _ = []
mudaCoordsCaixas x y t Nada = t
mudaCoordsCaixas x y ((a,b):c) U
        |x==a && y==b = ((a,b+1):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c U)
mudaCoordsCaixas x y ((a,b):c) D
        |x==a && y==b = ((a,b-1):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c D)
mudaCoordsCaixas x y ((a,b):c) L
        |x==a && y==b = ((a-1,b):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c L)
mudaCoordsCaixas x y ((a,b):c) R
        |x==a && y==b = ((a+1,b):c)
        |otherwise = (a,b):(mudaCoordsCaixas x y c R)

-- | Coloca as caixas numa linha.
colocaCaixa :: [String] -> (Int,Int) -> [String]
colocaCaixa l (x,y) = reverse (pLinha (reverse l) (x,y)) 
  where
    pLinha :: [String] -> (Int, Int) -> [String]
    pLinha [] _ = []
    pLinha (h:t) (x,y) = if(y == 0) 
                         then ((pColuna h x):t) 
                         else (h:pLinha t (x,y-1)) 

    pColuna :: String -> Int -> String
    pColuna [] _ = []
    pColuna (z:zs) n = if(n == 0) then if(z == '.') 
                                       then 'I' : zs 
                                       else 'H' : zs 
                       else (z:pColuna zs (n-1))

-- | Coloca as caixas no mapa.
colocaCaixas :: [String] -> [(Int, Int)] -> [String]
colocaCaixas l [] = l
colocaCaixas l [z] = colocaCaixa l z 
colocaCaixas l (h:t) = colocaCaixas (colocaCaixa l h) t

-- | Função que remove as coordenadas incorretas (as coordenadas que só tem um número ou as coordenadas que têm caracteres inválidos).
remInc :: [String] -> [String]
remInc [] = []
remInc (h:t) 
        |h=="" || length (words h) /= 2 || aux h == False = remInc t
        |otherwise = h:(remInc t)
        where aux :: String -> Bool
              aux [] = True
              aux (x:xs) = if (isDigit x) || x==' ' 
                           then aux xs 
                           else False

-- | verifica se o mapa tem todas as caixas na posição final
verificarCI :: [String] -> Bool
verificarCI [] = True
verificarCI (x:xs) = verificalinha x && verificarCI xs

-- | verifica se numa linha linha existe alguma caixa fora da posição final.
verificalinha :: String -> Bool
verificalinha [] = True
verificalinha (x:xs) 
        |(ord x)== 72 = False
        |otherwise = verificalinha xs 

-- | verifica se um dado mapa é valido ou não.
verificaMapa :: [String] -> [String]
verificaMapa linhas = if erro<0
                 then []
                 else [show erro]
            where erro = vErros linhas

-- | Devolve o nº da linha que dá erro.
vErros :: [String] -> Int
vErros linhas = juntaErros 
                    (juntaErros okTab (juntaErros oklength okCoords)) 
                    (juntaErros (juntaErros okCoordsT okCaixas) (juntaErros okBoneco okCaixasR))
    where
    (tab,coords) = parteMapa linhas
    numL = contaListasMapa tab
    coordsCorretas = remInc coords
    coordsTuplos = tuploCoord coordsCorretas
    okTab = validaTab 1 tab numL
    okCoords = validaCoords tab coordsCorretas 1
    okCaixas = validaCaixas tab (remVazio coords)
    oklength = verificalength tab 1
    okCoordsT = validaCoordsT coords 1 numL
    okBoneco = validaBoneco (daCoords 0 (reverse tab) 0) coordsTuplos numL
    okCaixasR = validaCaixasR coordsTuplos (drop 1 coordsTuplos) numL 2

{- | Devolve o nº de linhas de coordenadas.

Nesta função usamos /isDigit/ pertencente ao Data.Char para verificarmos se o elemento é ou não um algarismo (0,...9). -}
contaListasBC :: [String] -> Int
contaListasBC [] = 0
contaListasBC (x:xs)
        |(null x)==True = contaListasBC xs
        |isDigit (head x)==True = 1 + contaListasBC xs 
        |otherwise = contaListasBC xs

-- | Dá o nº da linha com o erro apos verificar as linhas do tabuleiro
validaTab :: Int -> [String] -> Int -> Int
validaTab pos [] m = -1
validaTab pos (l:ls) m = juntaErros erroLinhaTab erroLinhasTab
        where
        erroLinhaTab = validaLinhaTab pos l m
        erroLinhasTab = validaTab (pos+1) ls m

-- | Função que verifica se o tamanho das listas no tabuleiro é sempre igual.
verificalength :: [String] -> Int -> Int
verificalength [] n = -1
verificalength [l] n = -1
verificalength [l,lt] n
            |(length l)==(length lt) = -1
            |otherwise = n+1
verificalength (a:b:ab) n
            |(length a)==(length b) = verificalength (b:ab) (n+1)
            |otherwise = n+1

{- | Testa uma linha do tabuleiro. 

Nesta função usamos /ord/ que pertence ao Data.Char...

@
ord :: Char -> Int
@

Exemplos de utilização:

Se o char 'a' for um # então:
@
ord a == 35
@
Se o char 'a' for um espaço então:
@
ord a == 32
@
Se o char 'a' for um . então:
@
ord a == 46
@
-}
validaLinhaTab :: Int -> String -> Int -> Int
validaLinhaTab pos [] m = -1
validaLinhaTab pos (a:b) m 
                    |(pos==1 || pos==m) && ord a == 35 = validaLinhaTab pos b m
                    |pos>1 && pos<m && ord a == 35 && ord(last (a:b)) == 35 = aux pos b 
                    |otherwise = pos
                  where aux :: Int -> String -> Int
                        aux pos [] = -1
                        aux pos (x:xs) |ord x == 35 || ord x == 32 || ord x == 46 = aux pos xs
                                       |otherwise = pos 

-- | Testa as coordenadas e devolve a linha em caso de haver erros.
validaCoords :: [String] -> [String] -> Int -> Int
validaCoords _ [] n = -1
validaCoords mapa (h:t) n = if fsTuplo == -1 
                            then -1
                            else if ordC == 32 || ordC == 46 
                                 then validaCoords mapa t (n+1)
                                 else numLMapa + n
                            where
                                numLMapa = contaListasMapa mapa
                                tuplo = liToT (sToI h)
                                fsTuplo = fst tuplo
                                numL = contalinhas mapa (snd tuplo) 0 numLMapa
                                caracter = devolveCaracter numL fsTuplo 0
                                ordC = ord caracter

-- | Verifica se o boneco está dentro do mapa.
validaBoneco :: [(Int,Int)] -> [(Int,Int)] -> Int -> Int
validaBoneco (x:xs) [] m = m+1
validaBoneco [] _ m = m+1 
validaBoneco (x:xs) (y:ys) m
            |snd(x)==snd(y) && fst(x)==fst(y) = -1
            |otherwise = validaBoneco xs (y:ys) m

-- | Verifica se as coordenadas são válidas, ou seja, se são dois números...
validaCoordsT :: [String] -> Int -> Int -> Int
validaCoordsT [] n m = -1
validaCoordsT (x:xs) n m
             |(length (words x)) == 0 = validaCoordsT xs (n+1) m
             |(length (words x)) /= 2 = m+n
             |aux x == False = m+n
             |otherwise = validaCoordsT xs (n+1) m
             where aux :: String -> Bool
                   aux [] = True
                   aux (x:xs) = if (isDigit x) || x==' ' 
                                then aux xs 
                                else False 

-- | Devolve o caracter de uma certa lista
devolveCaracter :: String -> Int -> Int -> Char
devolveCaracter [] p n = ' '
devolveCaracter (x:xs) p n
        |n==p = x
        |otherwise =devolveCaracter xs p (n+1) 
       
-- | Devolve a linha correspondente ao número mas contando de baixo para cima...
contalinhas :: [String] -> Int -> Int -> Int -> String
contalinhas [] p n m = [] 
contalinhas (x:xs) p n m
        |n==(m-p-1) = x
        |otherwise = contalinhas xs p (n+1) m

-- | Verifica a correspondencia entre nº de pontos no tabuleiro e coordenadas.
validaCaixas :: [String] -> [String] -> Int
validaCaixas mapa coords
        |(numLBC-1) == numP = -1
        |(numLBC-1) > numP = numL + numP + 2
        |(numLBC-1) < numP = numL + numLBC + 1
        where
            numLBC = contaListasBC coords
            numP = numeroPontos mapa
            numL = contaListasMapa mapa
validaCaixas _ _ = 1

-- | Devolve o número de pontos existentes no mapa
numeroPontos :: [String] -> Int
numeroPontos [] = 0
numeroPontos (h:t) = aux h + numeroPontos t
          where aux :: String -> Int
                aux [] = 0
                aux (h:t)
                    |ord h == 46 = 1 + aux t
                    |otherwise = aux t

-- | verifica se existe ou nao uma caixa sobreposta sobre o boneco ou sobre uma caixa.
validaCaixasR :: [(Int,Int)] -> [(Int,Int)] -> Int -> Int -> Int
validaCaixasR [] _ m n = -1
validaCaixasR _ [] m n = -1
validaCaixasR (x:xs) (y:ys) m n = juntaErros (aux x (y:ys) m n) (validaCaixasR xs ys m (n+1)) 
      where aux :: (Int,Int) -> [(Int,Int)] -> Int -> Int -> Int
            aux (x,y) [] m n = -1
            aux (x,y) (z:zs) m n
                  |x==fst(z) && y==snd(z) = m+n
                  |otherwise = aux (x,y) zs m (n+1)

-- | Dá o erro com menor
juntaErros :: Int -> Int -> Int
juntaErros i j | i<0=j
               | j<0=i
               | otherwise = min i j

-- | Remove listas vazias.
remVazio :: [String] -> [String]
remVazio [] = []
remVazio (x:xs)
        |x=="" = remVazio xs
        |otherwise =x:(remVazio xs)
