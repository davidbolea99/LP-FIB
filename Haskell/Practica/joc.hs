import Data.List
import Data.Char

import Data.Map (Map)
import qualified Data.Map as Map

----------------------- Representacion del mundo ------------------------

data Player = X | O
    deriving (Bounded, Enum, Eq, Ord, Show)

data Board = Board
    {
        heigth  :: Int,
        width   :: Int,
        tops    :: [Int],
        cells   :: Map (Int, Int) Player
    }

instance Show Board where
    show board@(Board _ width _ _) = header ++ border_up ++ separated_board ++ border_down
        where
            header  = "\n     " ++ (intercalate "   " $ string2list $ take width ['A'..]) ++ "\n"
            separated_board = intercalate border_in $ map colSeparator (board2string board)

            border_up   =  " " ++ rowSeparator ++ "| "
            border_in   = " |" ++ rowSeparator ++ "| "
            border_down = " |" ++ rowSeparator

            rowSeparator = "\n   +" ++ (intercalate "+" $ take width $ repeat "---") ++ "+\n   "
            colSeparator = intercalate " | " . map (\x -> [x])

type Cell = Maybe Player

showCell :: Cell -> Char
showCell Nothing  = ' '
showCell (Just X) = 'X'
showCell (Just O) = 'O'


data AI = RANDOM | GREEDY | SMART

---------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do

    putStrLn $ take 3 $ repeat '\n'
    putStrLn "######################################################"
    putStrLn "################### CUATRO EN RAYA ###################"
    putStrLn "######################################################"

    (n,m)   <- read_size
    ai      <- read_ai
    result  <- loop (empty_board n m) ai move_list

    end_game result

---------------------------------------------------------------------------------------------------------------

-- Bucle principal del juego. Este recibe como entrada el tablero actual, el nivel de
-- la cpu, y un booleano que indica si le toca al usuario (True) o a la cpu (False)
loop :: Board -> AI -> [(Player, AI -> Board -> IO String)] -> IO Bool
loop board ai ((X,move):moves) = do
    
    putStrLn $ show board
    putStrLn "Turno del jugador X:\n"

    user_move <- move ai board
    let newBoard = put_piece X (string2int user_move) board

    let end = check_win X newBoard
    
    if end then do
        putStrLn $ show newBoard
        return True
    else do 
        loop newBoard ai moves

---------------------------------------------------------------------------------------------------------------

-- Para jugar en modo2 jugadores (debugging)
loop board ai ((O,move):moves) = do
    
    putStrLn $ show board
    putStrLn "Turno del jugador O:\n"

    user_move <- move ai board
    let newBoard = put_piece O (string2int user_move) board

    let end = check_win O newBoard
    
    if end then do
        putStrLn $ show newBoard
        return True
    else do 
        loop newBoard ai moves

---------------------------------------------------------------------------------------------------------------

string2int :: String -> Int
string2int c = (ord (head c)) - (ord 'A')

---------------------------------------------------------------------------------------------------------------

data Direction = H | V | BU | UB
    deriving Eq

check_win :: Player -> Board -> Bool
check_win player board = check_hor || check_vert || check_diag_BU || check_diag_UB
    where
        check_hor       = (max_length player H  board) >= 4
        check_vert      = (max_length player V  board) >= 4
        check_diag_BU   = (max_length player BU board) >= 4
        check_diag_UB   = (max_length player UB board) >= 4

max_length :: Player -> Direction -> Board -> Int
max_length p dir board@(Board heigth width tops _)
    | dir == H  = maximum [ max_length_bools [ get (i,j) board == Just p | j <- [0..(heigth-1)] ] | i <- [0..(width-1) ] ]
    | dir == V  = maximum [ max_length_bools [ get (i,j) board == Just p | i <- [0..(width-1) ] ] | j <- [0..(heigth-1)] ]
    | dir == BU = maximum [ max_length_bools [ get (i,j) board == Just p | i <- [0..(width-1) ], j <- [0..(heigth-1)], i+j == n ] | n <-[3..(width+heigth-5)] ]
    | dir == UB = maximum [ max_length_bools [ get (i,j) board == Just p | i <- [0..(width-1) ], j <- [0..(heigth-1)], i-j == n ] | n <-[-(width-4)..(heigth-4)] ]
        where
            max_length_bools :: [Bool] -> Int
            max_length_bools list = max_length_bools' 0 0 list
                where
                    max_length_bools' :: Int -> Int -> [Bool] -> Int
                    max_length_bools' _ max [] = max
                    max_length_bools' accum max (x:xs)
                        | new_accum > max     = max_length_bools' new_accum new_accum xs 
                        | otherwise               = max_length_bools' new_accum max xs
                        where
                            new_accum
                                | x         = accum + 1
                                | otherwise = 0
        

max_length _ _ _ = 0

---------------------------------------------------------------------------------------------------------------

put_piece :: Player -> Int -> Board -> Board
put_piece p col board@(Board h w tops cells)
    | out_of_bounds = board
    | otherwise     = (Board h w new_tops new_cells)
        where
            out_of_bounds   = not $ 0 <= row && row < h && 0 <= col && col < w
            row             = tops !! col
            new_cells       = Map.insert (row,col) p cells
            new_tops        = replaceNth col (row - 1) tops

---------------------------------------------------------------------------------------------------------------

-- Definimos move_list como una lista infinita de movimientos alternados entre el jugador y la IA.
-- Cada movimiento es una tupla formada por el jugador que mueve y una funcion que retorna el
-- entero correspondiente con la posicion en la que se va a insertar la ficha en el tablero.
-- Cuando es turno del jugador, la funcion es la lectura de la entrada, y cuando es turno de la
-- cpu, la funcion es una llamada a la funcion ai_move pasando como parametro el tablero y el tipo de IA.
move_list :: [(Player, AI -> Board -> IO String)]
--move_list =  cycle [ (X, \_ _ -> getLine), (O, \ai board -> return $ ai_move ai board)]
move_list =  cycle [ (X, \_ _ -> getLine), (O, \_ _ -> getLine)]

---------------------------------------------------------------------------------------------------------------

ai_move :: AI -> Board -> Int
ai_move ai board = 0

---------------------------------------------------------------------------------------------------------------

-- Funcion que trata la situacion de final del juego, donde el usuario puede decidir
-- si quiere jugar de nuevo o no.
end_game :: Bool -> IO ()
end_game result = do

    if result then do putStrLn "FELICIDADES! HAS GANADO!"
    else do putStrLn "GAME OVER :("

    putStrLn "\nQuieres volver a jugar? (si/no)"
    repetir <- getLine

    if repetir == "si" then
        main
    else if repetir == "no" then do
        putStrLn "\nHASTA PRONTO!!"
        return ()
    else do
        end_game result

---------------------------------------------------------------------------------------------------------------

-- Lectura del tamano del tablero
read_size:: IO (Int,Int)
read_size = do
    putStrLn "\n- Introduce el numero de filas del tablero:"
    input_n <- getLine

    putStrLn "\n- Introduce el numero de columnas del tablero:"
    input_m <- getLine

    let n = (read input_n :: Int)
    let m = (read input_m :: Int)

    if n < 1 || m < 1 || (n < 4 && m < 4) then do
        putStrLn "\n *** Entrada no valida. Los numeros deben ser positivos, y almenos uno de ellos, igual o superior a 4 ***"
        read_size
    else
        return (n,m)

---------------------------------------------------------------------------------------------------------------

-- Seleccion de la dificultad del oponente
read_ai:: IO AI
read_ai = do

    putStrLn "\n- Escoge la dificultad del oponente:\n"
    putStrLn "\t [1] - Random"
    putStrLn "\t [2] - Greedy"
    putStrLn "\t [3] - Smart"
    putStrLn "\nIntroduce el numero correspondiente:"

    input <- getLine

    if input == "1" then do
        putStrLn "Has escogido al oponente Random.\n"
        return RANDOM 
    else if input == "2" then do
        putStrLn "Has escogido al oponente Greedy.\n"
        return GREEDY
    else if input == "3" then do
        putStrLn "Has escogido al oponente Smart.\n"
        return SMART
    else do
        putStrLn "Entrada no valida. Introduzca '1', '2' o '3'."
        read_ai

---------------------------------------------------------------------------------------------------------------

-- Funcion que retorna un Board de tamano N x N vacio (lleno de espacios)
empty_board :: Int -> Int -> Board
empty_board n m = Board n m tops Map.empty
    where
        tops = take m $ iterate id (n-1)

-- Convierte un String en una lista de Strings correspondientes a cada uno de los caracteres.
string2list :: String -> [String]
string2list = map (\x -> [x])

---------------------------------------------------------------------------------------------------------------

-- Funcion que convierte el Map que representa el tablero en una lista de strings con los chars
-- asociados a cada celda. Los elementos estan ordenados del mismo modo que acostrumbramos a leer
-- una matriz en NxM: Primero el elemento [0,0], [0,1], [0,2], etc.

board2string :: Board -> [String]
board2string board@(Board heigth width _ _) = [row_elements row | row <- [0 .. heigth - 1]]
        where
            row_elements row = [cell row col | col <- [0 .. width - 1]]
            cell row col = showCell ((get (row,col)) board)

---------------------------------------------------------------------------------------------------------------

-- Funcion que encapsula aquella funcion que dada un tablero, retorna el elemento [i,j]. Utiliza
-- la funcion lookup de la clase Map. Aplica la busqueda sobre el Map asociaciado al Board (3r 
-- elemento de su struct)
get :: (Int,Int) -> (Board -> Maybe Player)
get coord = Map.lookup coord . cells

---------------------------------------------------------------------------------------------------------------

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs