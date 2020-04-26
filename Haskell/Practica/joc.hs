import System.Random
import Data.List
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map


{-|
El tipo de dato Player representa los dos tipos de fichas que se pueden introducir en el tablero.
En caso de jugar contra la CPU, el usuario llevara la X y la CPU el O.
-}
data Player = X | O
    deriving (Bounded, Enum, Eq, Ord, Show)

---------------------------------------------------------------------------------------------------------------
{-|
Tipo de razonamiento asociado al segundo jugador (CPU). En caso de jugar en modo de dos jugadores,
entonces se selecciona NONE como sistema de razonamiento, ya que este no se va a usar.
-}
data AI = RANDOM | GREEDY | SMART | NONE

---------------------------------------------------------------------------------------------------------------
{-|
La estructura de datos Board representa el tablero sobre el que se juega. Cada elemento de la
estructura representa:
    - heigth:   Altura del tablero (numero de filas)
    - width:    Anchura del tablero (numero de columnas)
    - tops:     Lista que guarda, para cada columna, la altura en la que se insertara la proxima ficha.
    - cells:    Diccionario clave-valor que representa las celdas del tablero. La clave es una tupla
                con las coordenadas (i,j) donde se localiza la celta, y el valor es el Player presente,
                o bien X, o bien O. Cabe destacar que este solo contiene las celdas con un jugador presente.
-}

---------------------------------------------------------------------------------------------------------------
data Board = Board
    {
        heigth  :: Int,
        width   :: Int,
        tops    :: [Int],
        cells   :: Map (Int, Int) Player
    }

---------------------------------------------------------------------------------------------------------------
{-|
Instanciacion de la funcion show para la estructura de datos que representa el tablero.
-}
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

---------------------------------------------------------------------------------------------------------------
{-|
Funcion main del juego. Esta escribe la presentacion, recibe los datos necesarios para empezar la partida:
tamano del tablero, numero de jugadores y tipo de IA (si es necesario). Posteriormente llama a la funcion
loop que es la que se encarga de mover el juego hasta que este finaliza, retorna el ganador, y trata el final
de la partida con la funcion end_game.
-}
main :: IO ()
main = do

    putStrLn $ take 3 $ repeat '\n'
    putStrLn "######################################################"
    putStrLn "################### CUATRO EN RAYA ###################"
    putStrLn "######################################################"

    (n,m)   <- read_size
    num_pl  <- read_num_players
    
    let list_of_moves = move_list num_pl
    let initial_board = empty_board n m

    if num_pl == 2 then do
        winner <- loop initial_board NONE list_of_moves
        end_game 2 winner

    else do
        ai     <- read_ai
        winner <- loop initial_board ai list_of_moves
        end_game 1 winner

---------------------------------------------------------------------------------------------------------------
{-|
Loop principal del juego. Este recibe, a cada ronda, el tablero actual, el nivel de inteligencia de la cpu, y
una lista de funciones que encapsulan cada uno de los movimientos*. Esta funcion se va llamando a si misma
mientras no haya un ganador, solicitando movimientos a los jugadores. Una vez el juego ha terminado, retorna
el ganador de la partida.
* Se explica con mas detalle en la descripcion de la funcion "move_list".
-}
loop :: Board -> AI -> [(Player, AI -> Board -> IO Int)] -> IO (Maybe Player)
loop board ai ((p,move):moves) = do
    
    putStrLn $ show board

    putStrLn $ "    Turno del jugador " ++ show p ++ ":\n"

    player_move <- move ai board
    let newBoard = put_piece p ( player_move) board

    let end = some_line_of p 4 newBoard
    let tie = [] == possible_moves newBoard

    if end then do
        putStrLn $ show newBoard
        return $ Just p
    else if tie then do
        putStrLn $ show newBoard
        return Nothing
    else do 
        loop newBoard ai moves

---------------------------------------------------------------------------------------------------------------
{-|
Tipo de dato que encapsula las cuatro direcciones en las que se puede recorrer el tablero. Horizontal, vertical,
en diagonal bottom-up (de izquierda a derecha) y en diagonal top-bottom (de izquierda a derecha).
-}
data Direction = H | V | BU | UB
    deriving Eq

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un tablero B, un jugador P y un entero N, te dice si hay una linea de por lo menos N fichas consecutivas
del jugador P en el tablero B en cualquier direccion.
-}
some_line_of :: Player -> Int -> Board -> Bool
some_line_of player n board = check_diag_BU || check_diag_UB || check_hor || check_vert
    where
        check_hor       = (max_length player H  board) >= n
        check_vert      = (max_length player V  board) >= n
        check_diag_BU   = (max_length player BU board) >= n
        check_diag_UB   = (max_length player UB board) >= n

---------------------------------------------------------------------------------------------------------------

{-|
Funcion que retorna un entero que indica cual es la longitud de la linea de fichas consecutivas mas larga de un jugador
p, en una direccion dir, en un tablero board.
-}
max_length :: Player -> Direction -> Board -> Int
max_length p dir board@(Board heigth width tops _)
    | dir == H  = maximum [ max_length_bools [ get (i,j) board == Just p | j <- [0..(width-1) ] ] | i <- [0..(heigth-1)] ]
    | dir == V  = maximum [ max_length_bools [ get (i,j) board == Just p | i <- [0..(heigth-1)] ] | j <- [0..(width-1 )] ]
    | dir == BU = maximum [ max_length_bools [ get (i,j) board == Just p | i <- [0..(heigth-1)], j <- [0..(width-1)], i+j == n ] | n <-[0..(width+heigth-2)   ] ] --n <-[3..(width+heigth-4)] ]
    | dir == UB = maximum [ max_length_bools [ get (i,j) board == Just p | i <- [0..(heigth-1)], j <- [0..(width-1)], i-j == n ] | n <-[-(width-1)..(heigth-1)] ] --n <-[-(width-4)..(heigth-5)] ]
        where
            max_length_bools :: [Bool] -> Int
            max_length_bools list = max_length_bools' 0 list
                where
                    max_length_bools' maxL []                       = maxL
                    max_length_bools' maxL (True:True:True:True:xs) = max_length_bools' (max maxL 4) (True:True:True:xs)
                    max_length_bools' maxL (True:True:True:xs)      = max_length_bools' (max maxL 3) (True:True:xs)
                    max_length_bools' maxL (True:True:xs)           = max_length_bools' (max maxL 2) (True:xs)
                    max_length_bools' maxL (_:xs)                   = max_length_bools' maxL         xs

            -- max_length_bools list = max_length_bools' 0 0 list
            --     where
            --         max_length_bools' :: Int -> Int -> [Bool] -> Int
            --         max_length_bools' _ max [] = max
            --         max_length_bools' accum max (x:xs)
            --             | new_accum > max     = max_length_bools' new_accum new_accum xs 
            --             | otherwise           = max_length_bools' new_accum max       xs
            --             where
            --                 new_accum
            --                     | x         = accum + 1
            --                     | otherwise = 0

---------------------------------------------------------------------------------------------------------------
    
{-|
Funcion que retorna una tripleta (q2,q3,q4) donde qi es la cantidad de lineas de i fichas consecutivas de un player p, en
una todas las direcciones, en un tablero board.
-}
how_many_lines_of_each_length :: Player -> Board -> (Int,Int,Int)
how_many_lines_of_each_length p board = suma_tripletas [count_h, count_v, count_ub, count_bu]
    where
        count_h  = how_many_lines_of_each_length_in_some_dir p H  board
        count_v  = how_many_lines_of_each_length_in_some_dir p V  board
        count_ub = how_many_lines_of_each_length_in_some_dir p UB board
        count_bu = how_many_lines_of_each_length_in_some_dir p BU board

        how_many_lines_of_each_length_in_some_dir :: Player -> Direction -> Board -> (Int,Int,Int)
        how_many_lines_of_each_length_in_some_dir p dir board@(Board heigth width tops _)
            | dir == H  = suma_tripletas [ how_many_trues [ get (i,j) board == Just p | j <- [0..(width-1)  ] ] | i <- [0..(heigth-1)] ]
            | dir == V  = suma_tripletas [ how_many_trues [ get (i,j) board == Just p | i <- [0..(heigth-1) ] ] | j <- [0..(width-1) ] ]
            | dir == BU = suma_tripletas [ how_many_trues [ get (i,j) board == Just p | i <- [0..(heigth-1) ] , j <- [0..(width-1) ], i+j == n ] | n <-[0..(width+heigth-2)   ] ]
            | dir == UB = suma_tripletas [ how_many_trues [ get (i,j) board == Just p | i <- [0..(heigth-1) ] , j <- [0..(width-1) ], i-j == n ] | n <-[-(width-1)..(heigth-1)] ]
                where 
                    how_many_trues :: [Bool] -> (Int,Int,Int)
                    how_many_trues list = (q2,q3,q4)
                        where
                            q2 = list_of_counters !! 0
                            q3 = list_of_counters !! 1
                            q4 = list_of_counters !! 2
                            list_of_counters = [how_many_trues_of_length n 0 list | n <- [2..4]]

                            how_many_trues_of_length :: Int -> Int -> [Bool] -> Int
                            
                            how_many_trues_of_length 4 acc []
                                | 4 <= acc  = 1
                                | otherwise = 0
                            how_many_trues_of_length 4 acc (x:xs)
                                | x         =     how_many_trues_of_length 4 (acc+1) xs
                                | 4 <= acc  = 1 + how_many_trues_of_length 4 0       xs
                                | otherwise =     how_many_trues_of_length 4 0       xs
                            
                            how_many_trues_of_length n acc []
                                | n == acc  = 1
                                | otherwise = 0
                            how_many_trues_of_length n acc (x:xs)
                                | x         =     how_many_trues_of_length n (acc+1) xs
                                | n == acc  = 1 + how_many_trues_of_length n 0       xs
                                | otherwise =     how_many_trues_of_length n 0       xs

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un jugador p y una columna col, inserta una ficha del jugador p en la columna col. A efectos practicos,
"inserta" una nueva entrada en el diccionario del tablero
-}
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
{-|
Definimos move_list como una lista infinita de movimientos alternados entre el hunmano y la IA, o por dos humanos.
Cada movimiento es una tupla formada por el jugador que mueve y una funcion que retorna el entero correspondiente
con la posicion en la que se va a insertar la ficha en el tablero.

Cuando es turno de un humano, la funcion es la lectura de la entrada, y cuando es turno de la cpu, la funcion es
una llamada a la funcion ai_move pasando como parametro el tablero y el tipo de IA.
-}
move_list :: Int -> [(Player, AI -> Board -> IO Int)]
move_list num_pl
    | num_pl == 1   =  cycle [ (O, \ai board -> ai_move ai board), (X, \_ _ -> inputColumn) ]
    | num_pl == 2   =  cycle [ (O, \_ _ -> inputColumn),           (X, \_ _ -> inputColumn) ]
    where
        inputColumn :: IO Int
        inputColumn = do
            line <- getLine
            return $ string2int line
            where
                string2int :: String -> Int
                string2int c = (ord (head c)) - (ord 'A')

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna una tripleta (q2,q3,q4) donde q_i es la cantidad de lineas potenciales de llegar a 4 de i
fichas consecutivas de un player p, en un tablero board.
-}
heuristic :: Player -> Board  -> Int
heuristic p board
    | some_line_of p 4 board              = 10000000
    | some_line_of (oponent p) 4 board    = -10000000
    | otherwise                     = q4 * value4 + q3 * value3 + q2 * value2
    where
        (q2,q3,q4)             = how_many_potential_lines_of_each_length p board
        (value2,value3,value4) = (5,100,10000)

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna una tripleta (q2,q3,q4) donde q_i es la cantidad de lineas potenciales de llegar a 4 de i
fichas consecutivas de un player p, en un tablero board.
-}
how_many_potential_lines_of_each_length :: Player -> Board -> (Int,Int,Int)
how_many_potential_lines_of_each_length p board = suma_tripletas [count_h, count_v, count_ub, count_bu]
    where
        count_h  = how_many_potential_lines_of_each_length_in_some_dir p H  board
        count_v  = how_many_potential_lines_of_each_length_in_some_dir p V  board
        count_ub = how_many_potential_lines_of_each_length_in_some_dir p UB board
        count_bu = how_many_potential_lines_of_each_length_in_some_dir p BU board

        how_many_potential_lines_of_each_length_in_some_dir :: Player -> Direction -> Board -> (Int,Int,Int)
        how_many_potential_lines_of_each_length_in_some_dir p dir board@(Board heigth width tops _)
            | dir == H  = suma_tripletas [ contar_lineas [ translate (get (i,j) board) | j <- [0..(width-1) ] ] | i <- [0..(heigth-1)] ]
            | dir == V  = suma_tripletas [ contar_lineas [ translate (get (i,j) board) | i <- [0..(heigth-1)] ] | j <- [0..(width-1) ] ]
            | dir == BU = suma_tripletas [ contar_lineas [ translate (get (i,j) board) | i <- [0..(heigth-1)], j <- [0..(width-1)], i+j == n ] | n <-[0..(width+heigth-2)   ] ]
            | dir == UB = suma_tripletas [ contar_lineas [ translate (get (i,j) board) | i <- [0..(heigth-1)], j <- [0..(width-1)], i-j == n ] | n <-[-(width-1)..(heigth-1)] ]
                where 
                    translate (Just player)
                        | player == p   =  1   -- coincide jugador
                        | otherwise     = -1   -- jugador opuesto
                    translate _         =  0   -- hueco

                    contar_lineas :: [Int] -> (Int,Int,Int)
                    contar_lineas list = contar_lineas' (0,0,0) list
                        where
                            contar_lineas' :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)

                            -- Los 1 son fichas del jugador que inspeccionamos y los -1 del oponente. Los 0 son casillas vacias.
                            -- Patrones de lineas y posibles lineas a generar
                            contar_lineas' (q2,q3,q4) (1:1:1:1:xs)  = contar_lineas' (q2     ,q3     ,q4 + 1000000) (1:1:1:xs)

                            contar_lineas' (q2,q3,q4) (0:1:1:1:xs)  = contar_lineas' (q2     ,q3 + 5 ,q4    ) (1:1:1:xs)
                            contar_lineas' (q2,q3,q4) (1:0:1:1:xs)  = contar_lineas' (q2     ,q3 + 1 ,q4    ) (0:1:1:xs)
                            contar_lineas' (q2,q3,q4) (1:1:0:1:xs)  = contar_lineas' (q2     ,q3 + 1 ,q4    ) (1:0:1:xs)
                            contar_lineas' (q2,q3,q4) (1:1:1:0:xs)  = contar_lineas' (q2     ,q3 + 5 ,q4    ) (1:1:0:xs)
                            
                            contar_lineas' (q2,q3,q4) (0:0:1:1:xs)  = contar_lineas' (q2 + 5 ,q3     ,q4    ) (0:1:1:xs)
                            contar_lineas' (q2,q3,q4) (0:1:0:1:xs)  = contar_lineas' (q2 + 1 ,q3     ,q4    ) (1:0:1:xs)
                            contar_lineas' (q2,q3,q4) (0:1:1:0:xs)  = contar_lineas' (q2 + 5 ,q3     ,q4    ) (1:1:0:xs)
                            contar_lineas' (q2,q3,q4) (1:0:0:1:xs)  = contar_lineas' (q2 + 1 ,q3     ,q4    ) (0:0:1:xs)
                            contar_lineas' (q2,q3,q4) (1:0:1:0:xs)  = contar_lineas' (q2 + 1 ,q3     ,q4    ) (0:1:0:xs)
                            contar_lineas' (q2,q3,q4) (1:1:0:0:xs)  = contar_lineas' (q2 + 5 ,q3     ,q4    ) (1:0:0:xs)

                            -- Cuando encuentra fichas del contrincante (no hace match con niguna de las anteriores),
                            -- ignora las siguientes casillas hasta que consiga hacer match, o se acabe la linea
                            contar_lineas' contadores (_:xs)        = contar_lineas' contadores xs

                            -- Si no queda nada que inspeccionar, se retorna el contador resultante
                            contar_lineas' contadores []            = contadores

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna una posicion en la que tirar ficha para aplicar la estrategia del 7.
-}
build_a_seven :: Player -> Board -> [Int]
build_a_seven p board@(Board heigth width tops _)
    | there_is_already_a_seven  = cols_to_push
    | otherwise                 = next_moves
    where
        -- If a seven already exists
        there_is_already_a_seven = could $ cols_to_push
        cols_to_push = [j+3 | i <- [3..(heigth-1)], j <- [0..(width-4)], get (i,j) board == Just p, space_to_win (i,j), there_is_a_seven (i,j)]

        -- Otherwise, lets create a seven
        next_moves = foldl (++) [] (map (\(x,y) -> y) distance_sorted_sevens)

        -- filtering functions
        distance_sorted_sevens = sort $ [ (distance_to coords, list_of_necessary_moves coords) | coords <- starting_cells]
        starting_cells = [(i,j) | i <- [3..(heigth-1)], j <- [0..(width-4)], get (i,j) board == Just p, space_to_win (i,j), is_valid_for_seven (i,j)]
        
        -- check functions
        there_is_a_seven cell = and $ map its_me (seven_cells cell)
            where its_me = (\cell -> get cell board == (Just p))

        is_valid_for_seven cell = and $ map not_blocked (seven_cells cell)
            where not_blocked = (\cell -> get cell board /= (Just (oponent p)))

        distance_to cell = sum $ map mine (seven_cells cell)
            where mine = (\(i,j) -> if get (i,j) board == (Just p) then 0 else (tops!!j)-i+1)

        list_of_necessary_moves (i,j) = col1 ++ col2 ++ col3
            where
                col1 = if remaining_c1    then [j  ]    else []
                col2 = if remaining_c2    then [j+1]    else []
                col3 = if remaining_c3_v1 then [j, j+3] else (if remaining_c3_v2 then [j+2] else [])

                remaining_c1    = (not $ get (i-2,j  ) board == Just p) && not_empty (get (i-1,j  ) board)
                remaining_c2    = (not $ get (i-2,j+1) board == Just p) && not_empty (get (i  ,j+1) board)
                remaining_c3_v1 = (not $ get (i-2,j+2) board == Just p) && is_empty  (get (i-1,j+2) board) && (is_empty $ get (i-3,j) board)
                remaining_c3_v2 = (not $ get (i-2,j+2) board == Just p) && not_empty (get (i-1,j+2) board)

        -- seven constant cells
        seven_cells (i,j) = [(i,j), (i-1,j+1), (i-2,j), (i-2,j+1), (i-2,j+2)]
        
        space_to_win :: (Int,Int) -> Bool
        space_to_win (i,j) = is_empty (get (i-2,j+3) board)

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna una posicion en la que tirar ficha para aplicar la estrategia del 7 invertido.
-}        
build_a_seven_inverted :: Player -> Board -> [Int]
build_a_seven_inverted p board@(Board heigth width tops _)
    | there_is_already_a_seven  = cols_to_push
    | otherwise                 = next_moves
    where
        -- If a seven already exists
        there_is_already_a_seven = could $ cols_to_push
        cols_to_push = [j-3 | i <- [3..(heigth-1)], j <- [3..(width-1)], get (i,j) board == Just p, space_to_win (i,j), there_is_a_seven (i,j)]

        -- Otherwise, lets create a seven
        next_moves = foldl (++) [] (map (\(x,y) -> y) distance_sorted_sevens)

        -- filtering functions
        distance_sorted_sevens = sort $ [ (distance_to coords, list_of_necessary_moves coords) | coords <- starting_cells]
        starting_cells = [(i,j) | i <- [3..(heigth-1)], j <- [3..(width-1)], get (i,j) board == Just p, space_to_win (i,j), is_valid_for_seven (i,j)]
        
        -- check functions
        there_is_a_seven cell = and $ map its_me (seven_cells cell)
            where its_me = (\cell -> get cell board == (Just p))

        is_valid_for_seven cell = and $ map not_blocked (seven_cells cell)
            where not_blocked = (\cell -> get cell board /= (Just (oponent p)))

        distance_to cell = sum $ map mine (seven_cells cell)
            where mine = (\(i,j) -> if get (i,j) board == (Just p) then 0 else (tops!!j)-i+1)

        list_of_necessary_moves (i,j) = col1 ++ col2 ++ col3
            where
                col1 = if remaining_c1    then [j  ]    else []
                col2 = if remaining_c2    then [j-1]    else []
                col3 = if remaining_c3_v1 then [j, j-3] else (if remaining_c3_v2 then [j-2] else [])

                remaining_c1    = (not $ get (i-2,j  ) board == Just p) && not_empty (get (i-1,j  ) board)
                remaining_c2    = (not $ get (i-2,j-1) board == Just p) && not_empty (get (i  ,j-1) board)
                remaining_c3_v1 = (not $ get (i-2,j-2) board == Just p) && is_empty  (get (i-1,j-2) board) && (is_empty $ get (i-3,j) board)
                remaining_c3_v2 = (not $ get (i-2,j-2) board == Just p) && not_empty (get (i-1,j-2) board)

        -- seven constant cells
        seven_cells (i,j) = [(i,j), (i-1,j-1), (i-2,j), (i-2,j-1), (i-2,j-2)]
        
        space_to_win :: (Int,Int) -> Bool
        space_to_win (i,j) = is_empty (get (i-2,j-3) board)
        
        is_empty :: Maybe Player -> Bool
        is_empty m = not $ not_empty m

        not_empty :: Maybe Player -> Bool
        not_empty (Just _) = True
        not_empty _ = False

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna la columna que hay que bloquear si hay peligro de que el oponente genere una estructura ganadora
que consiste en generar una linea de dos fichas, acompanada de 3 huecos, de forma que en la siguiente tirada, no puedas
bloquear ambos lados:  --XX- | -XX-- ==> -XXX- (situacion no bloqueable)
-}  
block_2side_free :: Player -> Board -> [Int]
block_2side_free p board@(Board heigth width tops _) = [j | i <- [0..(heigth-1)], j <- [0..(width-1)], tops!!j == i, check_2side_free (i,j)]
        where
            check_2side_free (i,j) = first_empty && (h_check || bu_check || ub_check)
                where
                    first_empty = is_empty (get (i,j) board)
                    h_check = valid_pos && next2_of_p && empty1 && empty2
                        where
                            valid_pos  = (j+3) < width
                            next2_of_p = (j+2) < width && (get (i,j+1) board) == Just p && (get (i,j+2) board) == Just p
                            empty1     = is_empty (get (i  ,j+3) board)
                            empty2     = ((j-1) >= 0 && (is_empty (get (i,j-1) board))) || ((j+4) < width && (is_empty (get (i,j+4) board)))
                    bu_check = valid_pos && next2_of_p && empty1 && empty2
                        where
                            valid_pos  = (j+3) < width && (i-3) >= 0
                            next2_of_p = (get (i-1,j+1) board) == Just p && (get (i-2,j+2) board) == Just p
                            empty1     = is_empty (get (i-3,j+3) board)
                            empty2     = ((i+1) < heigth && (j-1) >= 0 && (is_empty (get (i+1,j-1) board))) || ((i-4) >= 0 && (j+4) < width && (is_empty (get (i-4,j+4) board)))
                    ub_check = valid_pos && next2_of_p && empty1 && empty2
                        where
                            valid_pos  = (j+3) < width && (i+3) < heigth
                            next2_of_p = (get (i+1,j+1) board) == Just p && (get (i+2,j+2) board) == Just p
                            empty1     = is_empty (get (i+3,j+3) board)
                            empty2     = ((i+1) < heigth && (j-1) >= 0 && (is_empty (get (i+1,j-1) board))) || ((i+4) < heigth && (j+4) < width && (is_empty (get (i+4,j+4) board)))

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un Maybe Player retorna True si este es Nothing y Falso en caso contrario.
-} 
is_empty :: Maybe Player -> Bool
is_empty m = not $ not_empty m

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un Maybe Player retorna True si hay algun jugador y Falso si es Nothing.
-} 
not_empty :: Maybe Player -> Bool
not_empty (Just _) = True
not_empty _ = False

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un tipo de razonamiento, y el tablero actual, retorna la posicion donde quiere colocar la
siguiente ficha.
-}
ai_move :: AI -> Board -> IO Int
ai_move RANDOM board@(Board heigth width _ _) = randOfList valid_nums
    where
        valid_nums = [move | move <- [0..(width-1)], valid_move board move ]
        -- | randOfList retorna un elemento aleatorio de una lista no vacia.
        randOfList :: [Int] -> IO Int
        randOfList list = do
            random <- randomIO :: IO Int
            let index = mod random (length list)
            let result = list !! index
            return result
ai_move GREEDY board@(Board heigth width tops _)
    | could cpu_win        = return $ head $ cpu_win        -- Si la CPU puede ganar, pone ficha ahi
    | could user_win       = return $ head $ user_win       -- Si el usario puede ganar, la CPU bloquea su victoria
    | could cpu_line_of_3  = return $ head $ cpu_line_of_3  -- Si la CPU puede hacer una linea de 3 fichas, pone ficha ahi
    | could cpu_line_of_2  = return $ head $ cpu_line_of_2  -- Si la CPU puede hacer una linea de 2 fichas, pone ficha ahi
    | otherwise            = ai_move RANDOM board           -- Si no se da ninguna de las situaciones anteriores, pone ficha en un lugar random.
   
    where
        -- Lista de movimientos con los que el usuario gana, son aquellos en los que si el pone ficha, se genera una linea de 4.
        user_win  = [ move | move <- [0..width-1], valid_move board move, some_line_of X 4 (put_piece X move board) ]

        -- Lista de movimiento que puede hacer la CPU y cuantas lineas de cada distancia obtiene en cada caso
        cpu_moves = [ ((how_many_lines_of_each_length O (put_piece O move board)),move) | move <- [0..width-1], valid_move board move ]
        
        (t2,t3,t4) = how_many_lines_of_each_length O board

        cpu_win       = [ move | ((c2,c3,c4),move) <- cpu_moves, c4 > 0 ]
        cpu_line_of_3 = [ move | ((c2,c3,c4),move) <- cpu_moves, c3 > t3]
        cpu_line_of_2 = [ move | ((c2,c3,c4),move) <- cpu_moves, c2 > t2]
ai_move SMART board@(Board heigth width _ _)
    | first_2_rounds                = return $ head $ this_allowed_moves
    | could $ win_in 1              = return $ head $ win_in 1
    | could $ avoid_losing_in 1     = return $ head $ avoid_losing_in 1
    | could $ avoid_losing_in 2     = return $ head $ avoid_losing_in 2
    | could $ go_to_block_2side     = return $ head $ go_to_block_2side
    | could $ go_for_seven          = return $ head $ go_for_seven
    | could $ go_for_seven_inverted = return $ head $ go_for_seven_inverted
    | otherwise                     = return $ minimax
    where
        (past,left)         = game_round board
        first_2_rounds      = past < 2
        this_allowed_moves  = allowed_moves O board

        go_to_block_2side = block_moves \\ (block_moves \\ this_allowed_moves)
            where block_moves = (block_2side_free X board)

        go_for_seven = seven_moves \\ (seven_moves \\ this_allowed_moves)
            where seven_moves = (build_a_seven O board)
        
        go_for_seven_inverted = seven_inverted_moves \\ (seven_inverted_moves \\ this_allowed_moves)
            where seven_inverted_moves = (build_a_seven_inverted O board)

        minimax :: Int
        minimax = snd $ maximum [((heuristic_rec move),move) | move <- (allowed_moves O board)]
            where
                heuristic_rec move = choose_the_best_move' O (min left 3) (put_piece O move board) True
                choose_the_best_move' curr_p step board' maximize
                    | step == 0         = (heuristic curr_p board')
                    | win               =  10000000
                    | lose              = -10000000
                    | maximize          = if could list then maximum list else -10000000
                    | otherwise         = if could list then minimum list else  10000000
                    where
                        win         = some_line_of curr_p 4 board'
                        lose        = some_line_of (oponent curr_p) 4 board'
                        list        = [ value move | move <- (allowed_moves curr_p board') ]                            
                        value move  = choose_the_best_move' (oponent curr_p) (step - 1) (put_piece curr_p move board') (not maximize)

        win_in :: Int -> [Int]
        win_in n = win_in_board (2*n - 1) O O board

        avoid_losing_in :: Int -> [Int]
        avoid_losing_in n = win_in_board (2*n - 1) X X board

        win_in_board :: Int -> Player -> Player -> Board -> [Int]
        win_in_board n curr_p winner board'
            | n == 1    = [ move | move <- (allowed_moves curr_p board'), winner_win move, not $ oponent_win move ]
            | otherwise = [ move | move <- (allowed_moves curr_p board'), condition move]
            where
                condition move      = (not $ oponent_win move) && (could $ win_in_board (n-1) (oponent curr_p) winner (newBoard move))
                winner_win move     = some_line_of winner           4 (newBoard move)
                oponent_win move    = some_line_of (oponent winner) 4 (newBoard move)
                newBoard move       = put_piece curr_p move board'
           
---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un tablero board, y un jugador p, te retorna las posiciones donde p puede tirar ficha sin provocar
su propia derrota.
-}
allowed_moves :: Player -> Board -> [Int]
allowed_moves p board' = (possible_moves board') \\ (prohibited_moves p board')
    where
        prohibited_moves :: Player -> Board -> [Int]
        prohibited_moves p b = [ p_moves | p_moves <- (possible_moves b), could $
                                        [ opo_moves | opo_moves <- (possible_moves (newBoard p_moves)), some_line_of opo 4 (put_piece opo opo_moves (newBoard p_moves)) ]
                            ]
            where
                newBoard p_moves = put_piece p p_moves b
                opo = oponent p

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna los movimientos posibles a realizar, ordenados por cercania al centro de un tablero concreto.
-}
possible_moves :: Board -> [Int]
possible_moves board@(Board _ width _ _) = filter (valid_move board) (central_cols width)

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna cierto si la lista pasada como parametro tiene algun elemento
-}
could :: [a] -> Bool
could [] = False
could _  = True

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que trata la situacion de final del juego, donde el usuario puede decidir
si quiere jugar de nuevo o no.
-}
end_game :: Int -> Maybe Player -> IO ()
end_game _ (Nothing) = do -- Empate
    putStrLn "    EMPATE !!!"
    putStrLn "\n    Quieres volver a jugar? (si/no)"
    repetir <- getLine

    if repetir == "si" || repetir == "SI" then
        main
    else if repetir == "no" || repetir == "NO" then do
        putStrLn "\n    HASTA PRONTO!!"
        return ()
    else do
        end_game 1 (Nothing)
end_game 1 (Just p) = do -- Version Humano VS. CPU
    if p == X then do putStrLn "    FELICIDADES! HAS GANADO!"
    else do putStrLn "    GAME OVER :("

    putStrLn "\n    Quieres volver a jugar? (si/no)"
    repetir <- getLine

    if repetir == "si" || repetir == "SI" then
        main
    else if repetir == "no" || repetir == "NO" then do
        putStrLn "\n    HASTA PRONTO!!"
        return ()
    else do
        end_game 1 (Just p)
end_game 2 (Just p) = do -- Version Humano VS. Humano

    putStrLn $ "    FELICIDADES " ++ show p ++ "! HAS GANADO!"
    putStrLn "\n    Quereis volver a jugar? (si/no)"

    repetir <- getLine

    if repetir == "si" || repetir == "SI" then
        main
    else if repetir == "no" || repetir == "NO" then do
        putStrLn "\n    HASTA PRONTO!!"
        return ()
    else do
        end_game 2 (Just p)

---------------------------------------------------------------------------------------------------------------
{-|
Lectura del tamano del tablero
-}
read_size :: IO (Int,Int)
read_size = do

    putStrLn "\n- Con que tablero quieres jugar?\n"
    putStrLn "\t [1] - Tablero clasico (6x7)."
    putStrLn "\t [2] - Tablero personalizado"
    putStrLn "\nIntroduce el numero correspondiente:"
    
    input_q <- getLine
    let q = (read input_q :: Int)

    if q == 1 then do
        return (6,7)
    
    else if q == 2 then do
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

    else do
        putStrLn "\n *** Entrada no valida. Debes introducir 1 o 2. ***"
        read_size

---------------------------------------------------------------------------------------------------------------
{-|
Funcion lee cual es el numero de jugadores deseado.
-}
read_num_players :: IO Int
read_num_players = do
    
    putStrLn "\n- Introduce el numero de jugadores: (1/2)"
    input <- getLine

    let n = (read input :: Int)

    if n < 1 || n > 2 then do
        putStrLn "\n *** Entrada no valida. Debe introducir 1 o 2. ***"
        read_num_players
    else
        return n

---------------------------------------------------------------------------------------------------------------
{-|
Funcion lee cual es la dificultad de la cpu deseada. Esta recibe el numero de jugadores ya que, en caso de estar
jugando 2 jugadores, retornara NONE.
-}
read_ai :: IO AI
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
{-|
Funcion me retorna la lista de columnas por orden de cercania al centro.
-}
central_cols :: Int -> [Int]
central_cols width
    | (mod width 2) == 0    = central_cols'
    | otherwise             = [div (width-1) 2] ++ central_cols'
    where
        central_cols' = tuples2list $ reverse $ [(x,y) | x <- [0..(centre-1)], y <- [centre..last], x+y == last]
        centre = (div width 2)
        last = width - 1

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna un Board de tamano N x N vacio (lleno de espacios)
-}
empty_board :: Int -> Int -> Board
empty_board n m = Board n m tops Map.empty
    where
        tops = take m $ iterate id (n-1)

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un tablero y un movimiento (columna en la que tirar ficha), retorna true si el movimiento es
valido, es decir, si la columna no esta llena. Retorna falso si la columna esta llena.
-}
valid_move :: Board -> Int -> Bool
valid_move (Board _ _ tops _) col = not $ tops!!col < 0

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un tablero, retorna una tupla con la cantidad de movimientos realizados, y la cantidad de
movimientos restantes.
-}
game_round :: Board -> (Int,Int)
game_round (Board heigth width tops _) = (past,left)
    where
        left  = width + foldl (+) 0 tops
        past  = total - left
        total = heigth * width

---------------------------------------------------------------------------------------------------------------
{-|
Convierte un String en una lista de Strings correspondientes a cada uno de los caracteres.
-}
string2list :: String -> [String]
string2list = map (\x -> [x])

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que convierte el Map que representa el tablero en una lista de strings con los chars
asociados a cada celda. Los elementos estan ordenados del mismo modo que acostrumbramos a leer
una matriz en NxM: Primero el elemento [0,0], [0,1], [0,2], etc.
-}
board2string :: Board -> [String]
board2string board@(Board heigth width _ _) = [row_elements row | row <- [0 .. heigth - 1]]
        where
            row_elements row = [cell (row,col) | col <- [0 .. width - 1]]
            cell pos = showCell $ (get pos) board
            
            showCell :: Maybe Player -> Char
            showCell Nothing  = ' '
            showCell (Just p) = head $ show p

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que encapsula aquella funcion que dada un tablero, retorna el elemento [i,j]. Utiliza
la funcion lookup de la clase Map. Aplica la busqueda sobre el Map asociaciado al Board (4o 
elemento de su struct)
-}
get :: (Int,Int) -> (Board -> Maybe Player)
get coord = Map.lookup coord . cells

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que reemplaza el elemento iesimo de una lista de una lista.
-}
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que retorna la suma de una lista de tripletas en forma de tripleta.
El primer elemento es la suma de todos los primeros elementos de las tuplas de la lista y de forma analoga para
las otras dos posiciones.
-}
suma_tripletas :: [(Int,Int,Int)] -> (Int,Int,Int)
suma_tripletas list = foldl (suma_tripleta) (0,0,0) list
    where
        suma_tripleta :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
        suma_tripleta (a1,b1,c1) (a2,b2,c2) = (a1+a2,b1+b2,c1+c2)

---------------------------------------------------------------------------------------------------------------
{-|
Funcion pasa de una lista de tuplas, a una lista que contiene los mismos elementos, en el mismo orden, pero sin
estar envueltos en tuplas.
-}
tuples2list :: [(a,a)] -> [a]
tuples2list [] = []
tuples2list ((x, y) : xs) = x : y : (tuples2list xs)

---------------------------------------------------------------------------------------------------------------
{-|
Funcion que dado un jugador, te retorna su oponente.
-}
oponent :: Player -> Player
oponent X = O
oponent O = X