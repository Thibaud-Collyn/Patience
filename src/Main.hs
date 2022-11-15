import DataStructures
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import System.Random.Shuffle
import System.Random
import Data.Char (toLower)
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import GHC.IO (unsafePerformIO)

-- Grootte van 1 kaart
cardSize :: (Int, Int)
cardSize = (100, 150)

-- Plaats tussen 2 kaarten
cardInset :: Int
cardInset = 20

-- Positie van het gloss venster
windowPosition :: (Int, Int)
windowPosition = (200, 200)

-- Het Gloss venster
window :: Display
window = InWindow "Patience" (900, 600) windowPosition

fps :: Int
fps = 60

-- Seed voor random generator
seed :: Int
seed = 47

----------------
-- Initialisatie
----------------

-- Maakt een array van een volledig boek kaarten aan
createDeck :: Stack
createDeck = [(cardType, val, Hidden) | cardType <- [Club .. Spade], val <- [A .. K]]

-- Shuffled een lijst
shuffleList :: [a] -> [a]
shuffleList l = shuffle' l (length l) (mkStdGen seed)

flipLastCard :: Stack -> Stack
flipLastCard [] = []
flipLastCard stack | s == Hidden = init stack ++ [(t, v, Visible)]
                   | otherwise = stack
                   where lastCard@(t, v, s) = stack!!((length stack)-1)

-- Initialiseerd een nieuw Board
initBoard :: Board
initBoard = Board {
    gameStacks = [flipLastCard s | s <- reverse (splitStacks shuffledDeck 7)],
    endingStacks = [[], [], [], []],
    pile = flipLastCard (drop 28 shuffledDeck)
    }
    where shuffledDeck = shuffleList createDeck

-- Initialiseerd de selector
initSelector :: Selector
initSelector = Selector {
    position = (0, 0),
    selected = Nothing
}

-- Initialiseerd het spel
initGame :: Game
initGame = Game {
    board = initBoard,
    selector = initSelector
}

---------------------------------
-- Op voorhand inladen van images
---------------------------------

cardPictures :: [(Card, Picture)]
cardPictures = [(card, loadCardPicture cType cVal) | card@(cType, cVal, _)<-[(t, v, Visible) | (t, v, _) <- createDeck]]

-- Laad image in van een picture in lib/assets/myType/
loadCardPicture :: CardType -> CardValue -> Picture
loadCardPicture cType cVal = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ map toLower (show cType) ++ "s/" 
                            ++ map toLower (show cType) ++ "-" ++ simpleValue cVal ++ ".png")))

getPicture :: Card -> Picture
getPicture card = fromJust (lookup card cardPictures)

-- Zet de value van een kaart om in de 'simple value' die in de png notatie gebruikt wordt
simpleValue :: CardValue -> String
simpleValue val |val == Two = show 2
                |val == Three = show 3
                |val == Four = show 4
                |val == Five = show 5
                |val == Six = show 6
                |val == Seven = show 7
                |val == Eight = show 8
                |val == Nine = show 9
                |val == Ten = show 10
                |otherwise = show val

-- Array die niet-kaart images bevat
specialPictures :: [Picture]
specialPictures = [loadPicture "selector", loadPicture "selected", loadPicture "placeholder", loadPicture "back"]

-- Laad image in van een picture in lib/assets/
loadPicture :: String -> Picture
loadPicture val = fromMaybe Blank (unsafePerformIO (loadJuicyPNG ("lib/assets/" ++ val ++ ".png")))

-- Splitst een boek kaarten op in stacks met lengte n tot 1
splitStacks :: Stack -> Int -> [Stack]
splitStacks deck n | n == 0 = []
                   | otherwise = take n deck : splitStacks (drop n deck) (n-1)

----------------------
-- Rendering functions
----------------------

convertX :: Int -> Int -> Float
convertX xCoord axis = ((fromIntegral xCoord - fromIntegral axis/2) + 1/2) * (fromIntegral (fst cardSize) + fromIntegral cardInset)

convertY :: Int -> Int -> Float
convertY yCoord axis = (-(- (fromIntegral (snd cardSize - cardInset) / 2) + (fromIntegral yCoord - fromIntegral axis / 2) * 20)) - 20

-- Render 1 kaart
renderCard :: Card -> Picture
renderCard card@(cType, val, stat) | stat == Hidden = last specialPictures
                                   | stat == Visible = getPicture card

-- Render een stack van kaarten
renderStack :: Stack -> Int -> Int -> Picture
renderStack [] x y = translate (convertX x 7) (convertY y 4) (specialPictures!!2)
renderStack cardStack x y = pictures [translate (convertX x 7) (convertY (y + pos) 4) (renderCard (cardStack!!pos)) | pos<-[0..length cardStack-1]]

renderAllStacks :: [Stack] -> Picture
renderAllStacks stacks = pictures [renderStack (stacks!!x) (0+x) 0 | x<-[0..length stacks-1]]

renderSelector :: Selector -> Picture
renderSelector selector = translate (convertX x 7) (convertY y 4) (specialPictures!!0)
                        where x = fst (position selector)
                              y = snd (position selector)

renderSelected :: Selector -> Picture
renderSelected selector | isNothing (selected selector) = blank
                        | otherwise = translate (convertX x 7) (convertY y 4) (specialPictures!!1)
                        where x = fst (fromJust (selected selector))
                              y = snd (fromJust (selected selector))

renderPile :: Board -> Picture
renderPile board | length (pile board) == 0 = translate (convertX 0 7) 230 (specialPictures!!2)
                 | otherwise = translate (convertX 0 7) 230 (getPicture lastCard)
                 where lastCard = last (pile board)

renderEndStack :: Stack -> Int -> Picture
renderEndStack [] x = translate (convertX x 7) 230 (specialPictures!!2)
renderEndStack endStack x = translate (convertX x 7) 230 (getPicture (last endStack))

renderEndStacks :: Board -> Picture
renderEndStacks board = pictures [renderEndStack ((endingStacks board)!!x) (x+3) | x <- [0..3]]

render :: Game -> Picture
render game = pictures [renderAllStacks (gameStacks (board game)), renderSelector (selector game), renderSelected (selector game), renderPile (board game), renderEndStacks (board game)]

-----------
-- Gameplay
-----------
moveInDir :: Coordinate -> Direction -> Coordinate
moveInDir (x, y) dir | dir == U = (x, y-1)
                     | dir == D = (x, y+1)
                     | dir == L = (x-1, y)
                     | dir == R = (x+1, y)

-- Checks if selector can move to a certain card
canMove :: Game -> Coordinate -> Direction -> Bool
canMove game (x, y) U = y > 0
canMove game (x, y) D = y < length ((gameStacks (board game))!!x) -1
canMove game (x, y) L = x > 0
canMove game (x, y) R = x < 6

-- Function that automatically moves a position to the last element of the left or right stack
moveLR :: Coordinate -> Direction -> Board -> Coordinate
moveLR coord@(x, y) dir board | dir == L = (x-1, length ((gameStacks board)!!(x-1)) - 1)
                       | dir == R = (x+1, length ((gameStacks board)!!(x+1)) - 1)
                       | otherwise = coord

moveSelector :: Game -> Direction -> Game
moveSelector game dir | canMove game pos dir && (dir == U || dir == D) = game {selector = (selector game) {position = moveInDir pos dir}}
                      | canMove game pos dir && (dir == L || dir == R) = game {selector = (selector game) {position = moveLR pos dir (board game)}}
                      | otherwise = game
                      where pos = position (selector game)

flipCard :: Card -> Card
flipCard (t,v,Hidden) = (t,v,Visible)
flipCard card@(t,v,Visible) = card

selectCard :: Selector -> Board -> Game -> Game
selectCard sel board game | s == Visible = game{selector = sel{selected = Just (position sel)}}
                          | otherwise = game
                          where card@(_,_,s) = ((gameStacks board)!!(fst (position sel)))!!(snd (position sel))

-- Hulpfunctie om kaart waarden naar numerieke waarden om te zetten
valToNum :: Card -> Int
valToNum (_,v,_) | v == A = 1
                 | v == Two = 2
                 | v == Three = 3
                 | v == Four = 4
                 | v == Five = 5
                 | v == Six = 6
                 | v == Seven = 7
                 | v == Eight = 8
                 | v == Nine = 9
                 | v == Ten = 10
                 | v == J = 11
                 | v == Q = 12
                 | v == K = 13

-- Vervang de nde stack in een lijst van stacks
replaceNth :: Int -> Stack -> [Stack] -> [Stack]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

-- Checkt of de eerste kaart op de 2de kaart geplaats kan worden enkel op basis van kleur
difType :: Card -> Card -> Bool
difType (t1,_,_) (t2,_,_) | t1 == Club || t1 == Spade = t2 == Heart || t2 == Diamond
                          | t1 == Heart || t1 == Diamond = t2 == Club || t2 == Spade

-- Checkt of een bepaale 'substack' (vanaf selected) op een andere stack kan
canMoveStack :: Stack -> Stack -> Bool
canMoveStack mvStack dest | length dest == 0 && v == K = True
                          | otherwise = (difType mvCard dCard) && ((valToNum mvCard) + 1) == valToNum dCard
                          where mvCard@(t,v,s) = head mvStack
                                dCard = last dest

getSubStack :: Selector -> Stack -> Stack
getSubStack sel stack = reverse (take ((length stack) - y) (reverse stack)) where (x, y) = fromJust (selected sel)

removeStack :: Game -> Game
removeStack game = game{selector = (selector game){selected = Nothing},board = (board game){gameStacks = replaceNth (snd (fromJust (selected (selector game)))) (flipLastCard (take (snd (fromJust (selected (selector game)))) rmStack)) (gameStacks (board game))}} where rmStack = (gameStacks (board game))!!(fst (fromJust (selected (selector game))))

moveStack :: Selector -> Board -> Game -> Game
moveStack sel board game | canMoveStack subStack dest = game{board = board{gameStacks = replaceNth (fst (position sel)) (dest++subStack) (gameStacks board)}}
                         | otherwise = game
                         where subStack = getSubStack sel ((gameStacks board)!!(fst (fromJust (selected sel))))
                               dest = (gameStacks board)!!(fst (position sel))

-- Checkt of de waarde van een kaart 1 hoger is dan de waarde van de hoogste kaart van een stack
isOneHigher :: Card -> Stack -> Bool
isOneHigher card@(t,v,s) stack | null stack = v == A
                               | otherwise = valToNum card == succ (valToNum lCard) && t == tLast where lCard@(tLast, vLast, sLast) = last stack

-- Geeft ge ending stack terug die bij een bepaalde kaart hoort
getEndStack :: Card -> Board -> Stack
getEndStack (t,v,s) board | t == Club = (endingStacks board)!!0
                          | t == Diamond = (endingStacks board)!!1
                          | t == Spade = (endingStacks board)!!2
                          | t == Heart = (endingStacks board)!!3

-- Add een bepaalde kaart aan zijn bijhoorende ending stack
moveToEndStack :: Card -> [Stack] -> [Stack]
moveToEndStack card@(t,v,s) endingStacks | t == Club = replaceNth 0 ((endingStacks!!0)++[card]) endingStacks
                                         | t == Diamond = replaceNth 1 ((endingStacks!!1)++[card]) endingStacks
                                         | t == Spade = replaceNth 2 ((endingStacks!!2)++[card]) endingStacks
                                         | t == Heart = replaceNth 3 ((endingStacks!!3)++[card]) endingStacks
                                         | otherwise = endingStacks

-- hulpFunctie die de laatste kaart van een stack verwijderd en evt de nieuwe laatste kaart omdraait
rmLastCard :: Stack -> Stack
rmLastCard stack | length stack == 0 = stack
                 | length stack == 1 = []
                 | otherwise = flipLastCard (init stack)

handleSpace :: Game -> Coordinate -> Game
handleSpace game (x,y) = game{selector = (selector game){position = (x, y-1), selected = Nothing}, board = (board game){gameStacks = replaceNth x (rmLastCard curStack) (gameStacks (board game)), endingStacks = moveToEndStack (findCard (x,y) (board game)) (endingStacks (board game))}} where curStack = (gameStacks (board game))!!x

handleEnter :: Selector -> Board -> Game -> Game
handleEnter sel board game | isNothing (selected sel) = selectCard sel board game
                           | selected sel == Just (position sel) = game{selector = sel{selected = Nothing}}
                           | canMoveStack subStack dest = removeStack (moveStack sel board game)
                           | otherwise = game
                           where subStack = getSubStack sel ((gameStacks board)!!(fst (fromJust (selected sel))))
                                 dest = (gameStacks board)!!(fst (position sel))

-- Hulpfunctie die een kaart op een bepaalde coordinaat zoekt
findCard :: Coordinate -> Board -> Card
findCard (x,y) board = ((gameStacks board)!!x)!!y

-- Adds card from pile to the selector position
addFromPile :: Board -> Coordinate -> Board
addFromPile board pos | null (pile board) = board
                      | canMoveStack [last (pile board)] (curStack) = board{gameStacks = replaceNth (fst pos) (curStack++[last (pile board)]) (gameStacks board), pile = flipLastCard (init (pile board))}
                      | otherwise = board
                      where curStack = (gameStacks board)!!(fst pos)

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- detect if key pressed is a normal letter
isNormalKey :: Char -> Event -> Bool
isNormalKey k1 (EventKey (Char k2) Down _ _) = k1 == k2
isNormalKey _ _ = False

-- FIXME: Rotate Has not been implemented
handleInput :: Event -> Game -> Game
handleInput ev game
    | isKey KeyUp ev = moveSelector game U
    | isKey KeyDown ev = moveSelector game D
    | isKey KeyLeft ev = moveSelector game L
    | isKey KeyRight ev = moveSelector game R
    | isKey KeyEnter ev = handleEnter (selector game) (board game) game
    | isKey KeySpace ev && length (gameStacks (board game)) /= 0 && isOneHigher (findCard coord (board game)) (getEndStack (findCard coord (board game)) (board game)) = handleSpace game coord
    | isNormalKey 'p' ev = game{board = addFromPile (board game) (position (selector game))}
    | otherwise = game
    where coord@(x,y) = (position (selector game))

-- Update het bord in elke stap.
step :: Float -> Game -> Game
step _ b = b

main :: IO ()
main =  play window green fps initGame render handleInput step

