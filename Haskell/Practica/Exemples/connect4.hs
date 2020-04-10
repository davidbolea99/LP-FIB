--------------------------------------------------------------------------------
import           Data.List  (foldl', maximumBy)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import           Data.Ord   (comparing)


--------------------------------------------------------------------------------
data Player = X | O
    deriving (Bounded, Enum, Eq, Ord, Show)


--------------------------------------------------------------------------------
showTile :: Maybe Player -> Char
showTile Nothing  = ' '
showTile (Just X) = 'X'
showTile (Just O) = 'O'


--------------------------------------------------------------------------------
data Field = Field
    { fieldRows    :: Int
    , fieldColumns :: Int
    , fieldTiles   :: Map (Int, Int) Player
    }


--------------------------------------------------------------------------------
instance Show Field where
    show field@(Field rows columns _) = unlines $
        [ concat [show i | i <- [0 .. columns - 1]]
        ] ++
        [ [showTile (get row column field) | column <- [0 .. columns - 1]]
        | row <- [0 .. rows - 1]
        ]


--------------------------------------------------------------------------------
emptyField :: Int -> Int -> Field
emptyField rows columns = Field rows columns M.empty


--------------------------------------------------------------------------------
get :: Int -> Int -> Field -> Maybe Player
get row column = M.lookup (row, column) . fieldTiles


--------------------------------------------------------------------------------
push :: Int -> Player -> Field -> Field
push column tile field@(Field rows columns tiles)
    | column < 0 || column >= columns = field
    | row < 0                         = field
    | otherwise                       =
        Field rows columns $ M.insert (row, column) tile tiles
  where
    row = topOfStack column field - 1


--------------------------------------------------------------------------------
topOfStack :: Int -> Field -> Int
topOfStack column field@(Field rows _ _) = go 0
  where
    go row
        | row > rows                      = rows
        | get row column field /= Nothing = row
        | otherwise                       = go (row + 1)


--------------------------------------------------------------------------------
connections :: Int -> Field -> [[(Int, Int)]]
connections len (Field rows columns _) =
    -- Horizontal
    [ [(r, c + i) | i <- is]
    | r <- [0 .. rows - 1], c <- [0 .. columns - len]
    ] ++

    -- Vertical
    [ [(r + i, c) | i <- is]
    | r <- [0 .. rows - len], c <- [0 .. columns - 1]
    ] ++

    -- Diagonal: top left to bottom right
    [ [(r + i, c + i) | i <- is]
    | r <- [0 .. rows - len], c <- [0 .. columns - len]
    ] ++

    -- Diagonal: bottom left to top right
    [ [(r + i, c - i) | i <- is]
    | r <- [0 .. rows - len], c <- [len - 1 .. columns - 1]
    ]
  where
    is = [0 .. len - 1]


--------------------------------------------------------------------------------
count :: [Maybe Player] -> Maybe (Player, Int)
count tiles = case catMaybes tiles of
    []                  -> Nothing
    (x : xs)
        | all (== x) xs -> Just (x, length xs + 1)
        | otherwise     -> Nothing


--------------------------------------------------------------------------------
frequencies :: Int -> Field -> Player -> Int -> Int
frequencies len field =
    let map' = foldl' step M.empty $ connections len field
    in \p l -> fromMaybe 0 $ M.lookup (p, l) map'
  where
    step freqs connection =
        let tiles = map (\(r, c) -> get r c field) connection
        in case count tiles of
            Just c  -> M.insertWith' (+) c 1 freqs
            Nothing -> freqs


--------------------------------------------------------------------------------
winner :: Int -> Field -> Maybe Player
winner len field = listToMaybe
    [ p
    | p <- [minBound .. maxBound]
    , frequencies' p len > 0
    ]
  where
    frequencies' = frequencies len field


--------------------------------------------------------------------------------
score :: Int -> Field -> Player -> Int
score len field me = sum
    [ sign * score' * frequencies' p l
    | p <- [minBound .. maxBound]
    , l <- [1 .. len]
    , let sign   = if p == me then 1 else -1
    , let score' = l ^ (2 * l)
    ]
  where
    frequencies' = frequencies len field


--------------------------------------------------------------------------------
ai :: Int -> Field -> Player -> Int
ai len field me = fst $ maximumBy (comparing snd)
    [ (col, score len (push col me field) me)
    | col <- [0 .. fieldColumns field - 1]
    ]


--------------------------------------------------------------------------------
main :: IO ()
main = go (cycle players) $ emptyField 7 9
  where
    players :: [(Player, Int -> Field -> IO Int)]
    players =
        [ (X, \_ _ -> readLn)
        , (O, \len field -> return $ ai len field O)
        ]

    go []             _     = return ()
    go ((p, pf) : ps) field = do
        putStr $ show field
        case winner 4 field of
            Just w  -> putStrLn $ "Player " ++ show w ++ " wins!"
            Nothing -> do
                putStrLn $ "Player " ++ show p ++ " takes a turn!"
                n <- pf 4 field
                go ps $ push n p field