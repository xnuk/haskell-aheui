import Data.Char (ord, chr, isDigit)
import Data.List (genericIndex, elemIndex)
import System.Exit (ExitCode(..), exitWith)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Debug.Trace (trace)

if' :: Bool -> a -> a -> a
if' True a _ = a
if' False _ a = a

len :: [a] -> Integer
len = fromIntegral . length

traceIt :: Bool
traceIt = False

infixl 9 !!?
(!!?) :: [a] -> Integer -> Maybe a
a !!? b = if' (b >= 0 && len a > b) (Just (a `genericIndex` b)) Nothing

data Cho  = S가|S까|S나|S다|S따|S라|S마|S바|S빠|S사|S싸|S아|S자|S짜|S차|S카|S타|S파|S하 deriving (Enum, Eq, Show)
data Jung = M아|M애|M야|M얘|M어|M에|M여|M예|M오|M와|M왜|M외|M요|M우|M워|M웨|M위|M유|M으|M의|M이 deriving (Enum, Eq, Show)
data Jong = E아|E악|E앆|E앇|E안|E앉|E않|E앋|E알|E앍|E앎|E앏|E앐|E앑|E앒|E앓|E암|E압|E앖|E앗|E았|E앙|E앚|E앛|E앜|E앝|E앞|E앟 deriving (Enum, Eq, Show)

data Direction = U | U2 | D | D2 | L | L2 | R | R2 deriving (Eq, Show)

data Hangul = Geulja (Cho, Jung, Jong) | Space
data StoreCursor = CStack Integer | CQueue deriving (Eq, Show) -- | CExtend

data Aheui = Aheui {
    aStack :: [[Integer]],
    aStackIndex :: [Jong],
    aQueue :: [Integer],
    -- aheuiExtend :: IO SomeMagicExtendWillBeHere,
    aCode :: [[Hangul]],
    aCursor :: (Integer, Integer),
    aStore :: StoreCursor,
    aDirection :: Direction,
    aStdin :: String
    }

instance Show Hangul where
    show Space = "　"
    show (Geulja (cho, jung, jong)) = (:"") . chr $ 44032 + fromEnum cho * 588 + fromEnum jung * 28 + fromEnum jong

instance Show Aheui where
    show x = show (aDirection x) ++ '\t':show (aCursor x) ++ '\t':show (aStore x) ++ '\t':show (aQueue x) ++ '\t':show (aStack x) ++ '\t':show (aStdin x)

hangulSplit :: Char -> Hangul
hangulSplit x = if' (a < 0 || a > 11171) Space $ Geulja (toEnum b, toEnum c, toEnum d)
    where a = ord x - 44032
          (b, h) = a `divMod` 588
          (c, d) = h `divMod` 28

hangulParse :: String -> [[Hangul]]
hangulParse = map (map hangulSplit) . lines

interpret :: Aheui -> IO ExitCode
interpret ah = case code !!? y of
    Nothing -> case direction of
      _ | direction == U || direction == U2 -> interpret $ ah {aCursor = (x, len code - 1)}
        | direction == D || direction == D2 -> interpret $ ah {aCursor = (x, 0)}
        | otherwise -> interpret $ aheuiGo ah

    Just e -> case e !!? x of
        Nothing -> case direction of
          _ | direction == L || direction == L2 -> interpret $ ah {aCursor = (len e - 1, y)}
            | direction == R || direction == R2 -> interpret $ ah {aCursor = (0, y)}
            | otherwise -> interpret $ aheuiGo ah
        Just (Geulja (S하, _, _)) -> return . maybe ExitSuccess ((\z -> if' (z==0) ExitSuccess $ ExitFailure z) . fromIntegral) . fst . aheuiPop $ ah
        Just a -> aheuiDo a ah >>= interpret . aheuiGo . (\z -> if' traceIt (trace (show a ++ '\t':show z) z) z)
    where (x, y) = aCursor ah
          direction = aDirection ah
          code = aCode ah

jungDirection :: Direction -> Jung -> Direction
jungDirection d j = case j of
    M아 -> R
    M야 -> R2
    M어 -> L
    M여 -> L2
    M오 -> U
    M요 -> U2
    M우 -> D
    M유 -> D2
    M으 -> if' (d `elem` [L, L2, R, R2]) d (reversedDirection d)
    M이 -> if' (d `elem` [U, U2, D, D2]) d (reversedDirection d)
    M의 -> reversedDirection d
    _ -> d

aheuiGo :: Aheui -> Aheui
aheuiGo ah = case aDirection ah of
    U  -> ah {aCursor = (x, y-1)}
    U2 -> ah {aCursor = (x, y-2)}
    D  -> ah {aCursor = (x, y+1)}
    D2 -> ah {aCursor = (x, y+2)}
    L  -> ah {aCursor = (x-1, y)}
    L2 -> ah {aCursor = (x-2, y)}
    R  -> ah {aCursor = (x+1, y)}
    R2 -> ah {aCursor = (x+2, y)}
    where (x, y) = aCursor ah

aheuiPop :: Aheui -> (Maybe Integer, Aheui)
aheuiPop ah = case aStore ah of
    CQueue -> let q = aQueue ah in if' (len q == 0) (Nothing, ah) (Just (last q), ah {aQueue = init q})
    CStack n -> case aStack ah !!? n of
        Just (x:xs) -> (Just x, ah {aStack = stkbefore ++ xs:stkafter})
        Just [] -> (Nothing, ah)
        Nothing -> (Nothing, ah)
        where (stkbefore, ys)  = splitAt (fromIntegral n) (aStack ah)
              stkafter = tail ys

aheuiPush :: Integer -> StoreCursor -> Aheui -> Aheui
aheuiPush d cursor ah = case cursor of
    CQueue -> ah {aQueue = d:aQueue ah}
    CStack n -> case aStack ah !!? n of
        Just ys -> ah {aStack = stkbefore ++ (d:ys):stkafter}
        Nothing -> ah
        where (stkbefore, xs) = splitAt (fromIntegral n) (aStack ah)
              stkafter = if' (null xs) [] (tail xs)

aheuiPushJong :: Integer -> Jong -> Aheui -> Aheui
aheuiPushJong d jong ah = case jong of
    E앙 -> push CQueue ah
    E앟 -> ah -- here
    _ -> case elemIndex jong stkind of
        Just n -> push (CStack (fromIntegral n)) ah
        Nothing -> push (CStack 0) (ah {
            aStack = []:aStack ah,
            aStackIndex = jong:stkind,
            aStore = case aStore ah of
                CStack n -> CStack (n+1)
                x -> x
            })
    where push = aheuiPush d
          stkind = aStackIndex ah

reversedDirection :: Direction -> Direction
reversedDirection d = case d of
    L  -> R
    L2 -> R2
    U  -> D
    U2 -> D2
    R  -> L
    R2 -> L2
    D  -> U
    D2 -> U2

strokeCount :: Jong -> Integer
strokeCount j
    | j == E아 = 0
    | j `elem` [E악,E안,E앗] = 2
    | j `elem` [E앋,E앚,E앜] = 3
    | j `elem` [E앆,E앇,E암,E압,E았,E앛,E앝,E앞] = 4
    | j `elem` [E앉,E않,E알] = 5
    | j == E앖= 6
    | j `elem` [E앍,E앐] = 7
    | j == E앓= 8
    | j `elem` [E앎,E앏,E앑,E앒] = 9
    | otherwise = 31415926

aheuiDo :: Hangul -> Aheui -> IO Aheui
aheuiDo Space ah = return ah
aheuiDo (Geulja (a, b, c)) ah = case a of
        S아 -> return aheui

        S다 -> return $ popFold (+)
        S타 -> return $ popFold (-)
        S나 -> return $ popFold div
        S따 -> return $ popFold (*)
        S라 -> return $ popFold rem

        S마 -> case c of
            E앙 -> p1io' $ putStr . show
            E앟 -> p1io' $ putStr . (:"") . chr . fromIntegral
            _ -> p1io' $ const (return ())
        S바 -> case c of
            E앙 ->  uncurry push `liftM` getDigit aheui
            E앟 -> case aStdin aheui of
                "" -> uncurry push `liftM` getL aheui
                (y:ys) -> return $ push (fromIntegral . ord $ y) aheui {aStdin = ys}
            _   -> return $ push (strokeCount c) aheui
            where getDigit :: Aheui -> IO (Integer, Aheui)
                  getDigit ahi = case aStdin ahi of
                    "" -> getLine >>= (\x -> getDigit ahi {aStdin = x})
                    xs | tr == ""  -> getLine >>= (\x -> getDigit ahi {aStdin = x})
                       | otherwise -> let (x, y) = span isDigit tr in return (read x, ahi {aStdin = y})
                       where tr = dropWhile (not . isDigit) xs

                  getL :: Aheui -> IO (Integer, Aheui)
                  getL ahi = getLine >>= (\x -> case x of
                    "" -> getL ahi
                    (y:ys) -> return (fromIntegral (ord y), ahi {aStdin = ys})
                    )
        S빠 -> return $ popSingle (\_ x -> case q of
            CStack _ -> push x aheui
            CQueue   -> aheui {aQueue = aQueue aheui ++ [x]})
--        S파 -> return $ maybe revaheui (`push` (p2push $ flip const)) pop2
        S파 -> return $ popDouble $ if' (q==CQueue) (\aheui' x y -> aheui' {aQueue = aQueue aheui' ++ [y, x]}) (flip . pushTwo)
        S사 -> case c of
            E앙 -> return $ aheui {aStore = CQueue}
            E앟 -> return aheui -- here
            _ -> return $ case elemIndex c (aStackIndex aheui) of
                Just n  -> aheui {aStore = CStack (fromIntegral n)}
                Nothing -> aheui {aStore = CStack 0, aStackIndex = c:aStackIndex aheui, aStack = []:aStack aheui}
        S싸 -> return $ popSingle (\aheui' p -> aheuiPushJong p c aheui')
        S자 -> return $ popFold (\p2 p1 -> if' (p2<p1) 0 1)
        S차 -> return $ popSingle (\aheui' x -> if' (x==0) (aheui' {aDirection = reversedDirection (aDirection aheui')}) aheui')
        _ -> return aheui
    where aheui = ah {aDirection = jungDirection (aDirection ah) b}
          (pop1, ahu) = aheuiPop aheui

          q = aStore aheui
          revaheui = aheui {aDirection = reversedDirection (aDirection aheui)}

          p1io' :: (Integer -> IO ()) -> IO Aheui
          p1io' f = maybe (return revaheui) ((>> return ahu) . f) pop1

          push = (`aheuiPush` q)

          popSingle :: (Aheui -> Integer -> Aheui) -> Aheui
          popSingle func = let (x, aheui') = aheuiPop aheui
                           in maybe revaheui (func aheui') x

          popDouble :: (Aheui -> Integer -> Integer -> Aheui) -> Aheui -- f pop2 pop1 :: Aheui
          popDouble func = popSingle (\aheui' p1 -> let (y, aheui'') = aheuiPop aheui'
                                                    in maybe revaheui (flip (func aheui'') p1) y)

          popFold :: (Integer -> Integer -> Integer) -> Aheui
          popFold func = popDouble (\aheui' x y -> flip push aheui' $ func x y)

          pushTwo :: Aheui -> Integer -> Integer -> Aheui -- push p1 -> push p2
          pushTwo aheui' p1 p2 = push p2 $ push p1 aheui'

initAheui :: [[Hangul]] -> Aheui
initAheui code = Aheui {
    aStack = [[]],
    aStackIndex = [E아],
    aQueue = [],
    aCode = code,
    aCursor = (0, 0),
    aDirection = D,
    aStore = CStack 0,
    aStdin = ""
    }

main :: IO ()
main = getArgs >>= readFile . head >>= interpret . initAheui . hangulParse >>= exitWith
