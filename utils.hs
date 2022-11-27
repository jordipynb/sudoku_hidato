module Utils where
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------- Definicion del Tablero -----------------------------------------
data Tablero = Tablero {
    matriz :: [[Int]],
    minPos :: (Int,Int),
    maxPos :: (Int,Int)
} | Null deriving (Show, Eq)


----------------------------------- Pintar el Tablero en Consola -------------------------------------
printTablero Null = putStrLn "Vacio"
printTablero tablero = putStrLn (showTablero tablero)


--------------------------------- Retorna el Tablero en un String ------------------------------------
showTablero :: Tablero -> String
showTablero tablero = showMatriz listaDeListas 0 0 (length listaDeListas) (length listasNum) espaciado
    where
        listaDeListas = matriz tablero
        listasNum = head listaDeListas
        espaciado = length (show ((length listaDeListas)*(length listasNum)))


------------------------------- Retorna la matriz en forma de String -----------------------------------
showMatriz :: [[Int]] -> Int -> Int-> Int-> Int -> Int -> String
showMatriz listaDeListas iniFila iniCol finFila finCol espaciado
    | iniFila == finFila = ""
    | iniCol == finCol = "\n" ++ showMatriz listaDeListas (iniFila + 1) 0 finFila (length listasNum) espaciado
    | otherwise = espacio ++ elem ++ showMatriz listaDeListas iniFila (iniCol + 1) finFila finCol espaciado
        where
            listasNum = listaDeListas !! (iniFila + 1)
            el =  listaDeListas !! iniFila !! iniCol
            elem = if el == -1 then " " else show el
            espacio =  concat [" " | _ <- [0..(espaciado - length elem)]]


--------------------- Retorna True/False si la posicion esta en rango en la matriz ----------------------
enRango :: (Int, Int) -> [[Int]] -> Bool
enRango (i, j) matriz =
    i >= 0 && i < filaN && j >= 0 && j < colN
    where
        filaN = length matriz
        colN = length (matriz !! i)


---------------------------------- Reemplaza un elemento en la matriz -----------------------------------
reemplaza :: Eq a => (Int, Int) -> a -> [[a]] -> [[a]]
reemplaza (i,j) valor listaDeListas = a1 ++ (listaNumNueva:b1)
    where
        -- divide la lista de listas en dos listas a partir de la posicion i referente a la fila
        (a1, listaNum:b1) = splitAt i listaDeListas
        -- divide la lista en dos listas a partir de la posicion j referente a la columna
        (a2, _:b2) = splitAt j listaNum
        listaNumNueva = a2 ++ (valor:b2)


------------------------------ Calcula la cantidad de ceros en la matriz --------------------------------
cerosCantidad :: [[Int]] -> Int
cerosCantidad matriz = sum ([ 1 | x <- concat matriz, x==0])

--- Si todas las casillas alrededor son distintas de 0 entonces estan ocupadas y no se puede continuar ---
noPuedoContinuar :: (Int, Int) -> ([Int], [Int]) -> [[Int]] -> Bool
noPuedoContinuar (i, j) (dx, dy) listaDeListas =
        0 `notElem` [listaDeListas!!(i+ni)!!(j+nj) | ni <- dx, nj <- dy, enRango (i+ni,j+nj) listaDeListas]


------------------------ Calcula la cantidad de los ceros conectados en la matriz --------------------------
cerosConectados :: [[Int]] -> Int -> Int -> Int
cerosConectados listaDeListas i j
    | listaDeListas!!i!!j /= 0 = 0
    | otherwise = encuentraCeros (Set.fromList [(i, j)])
    where
        -- Se itera sobre el conjunto de los ceros hasta que no haya ningun cambio
        encuentraCeros :: Set (Int, Int) -> Int
        encuentraCeros cerosConjunto
            -- Si no hubo ningun cambio se retorna la cantidad de ceros
            | cerosConjunto == cerosVecinosFinal = (Set.size cerosVecinosFinal)
            -- En otro caso, se itera sobre el nuevo conjunto con las posiciones de los ceros hasta que no haya ningun cambio
            | otherwise = encuentraCeros cerosVecinosFinal
            where
                cerosVecinosFinal = vecinos cerosConjunto
                -- Retorna todos los vecinos de la lista de ceros que tambien sean ceros
                vecinos :: Set (Int, Int) -> Set (Int, Int)
                vecinos cerosConjunto
                    | Set.size cerosConjunto == 0 = Set.empty
                    | otherwise = Set.union cerosVecinos (vecinos popCerosConjuntos)
                    where
                        -- Arrays direccionales
                        dx = [0, 0, 0,  1, 1,  1, -1, -1, -1]
                        dy = [0, 1, -1, 0, 1, -1,  1,  0, -1]
                        (x, y) = Set.findMin cerosConjunto
                        popCerosConjuntos = Set.deleteMin cerosConjunto
                        cerosVecinos = Set.fromList [(x+dx!!i,y+dy!!i) | i <- [0..(length dx)-1], enRango (x+dx!!i, y+dy!!i) listaDeListas, listaDeListas!!(x+dx!!i)!!(y+dy!!i)==0]


------------------------------------------ Se desconecta ---------------------------------------------------
seDesconecta :: [[Int]] -> Int -> Int -> Bool
seDesconecta listaDeListas x y
    | length cerosConjunto <= 1 = False
    | enRango (arriba, y) listaDeListas && enRango (x, derecha) listaDeListas && enRango (arriba, derecha) listaDeListas && intercepto arriba derecha = True
    | enRango (arriba, y) listaDeListas && enRango (x, izquierda) listaDeListas && enRango (arriba, izquierda) listaDeListas && intercepto arriba izquierda = True
    | enRango (abajo, y) listaDeListas && enRango (x, izquierda) listaDeListas && enRango (abajo, izquierda) listaDeListas && intercepto abajo izquierda = True
    | enRango (abajo, y) listaDeListas && enRango (x, derecha) listaDeListas && enRango (abajo, derecha) listaDeListas && intercepto abajo derecha = True
    | enRango (x,derecha) listaDeListas && enRango (x,izquierda) listaDeListas && separaColumna = True
    | enRango (abajo,y) listaDeListas && enRango (arriba,y) listaDeListas && separaFila = True
    | otherwise = False
    where
        arriba = x - 1
        abajo = x + 1
        izquierda = y - 1
        derecha = y + 1
        intercepto ni nj = listaDeListas!!x!!nj /= 0 && listaDeListas!!ni!!y /= 0 && listaDeListas!!ni!!nj == 0
        separaColumna = ((not (enRango (arriba,y) listaDeListas)) || listaDeListas!!arriba!!y /= 0)  &&
                     ((not (enRango (abajo,y) listaDeListas)) || listaDeListas!!abajo!!y /= 0) &&
                     ((enRango (arriba,izquierda) listaDeListas && listaDeListas!!arriba!!izquierda == 0) ||
                      (enRango (abajo,izquierda) listaDeListas && listaDeListas!!abajo!!izquierda == 0) || listaDeListas!!x!!izquierda == 0) &&
                     ((enRango (arriba,derecha) listaDeListas && listaDeListas!!arriba!!derecha == 0) ||
                      (enRango (abajo,derecha) listaDeListas && listaDeListas!!abajo!!derecha == 0) || listaDeListas!!x!!derecha == 0)
        separaFila = ((not (enRango (x,izquierda) listaDeListas)) || listaDeListas!!x!!izquierda /= 0) &&
                     ((not (enRango (x,derecha) listaDeListas)) ||  listaDeListas!!x!!derecha /= 0) &&
                    ((enRango (arriba,izquierda) listaDeListas  && listaDeListas!!arriba!!izquierda == 0 ) ||
                     (enRango (arriba,derecha) listaDeListas && listaDeListas!!arriba!!derecha == 0) || listaDeListas!!arriba!!y == 0) &&
                    ((enRango (abajo, izquierda) listaDeListas && listaDeListas!!abajo!!izquierda == 0 ) ||
                     (enRango (abajo, derecha) listaDeListas && listaDeListas!!abajo!!derecha == 0) || listaDeListas!!abajo!!y == 0)
        -- Arrays direccionales
        dx = [0, 0, 0,  1, 1,  1, -1, -1, -1]
        dy = [0, 1, -1, 0, 1, -1,  1,  0, -1]
        cerosConjunto = filter (\x -> x==0) [listaDeListas!!(x+dx!!i)!!(y+dy!!i) | i <- [0..length dx-1], enRango (x+dx!!i,y+dy!!i) listaDeListas]


------------------------------- function to find cells in the board between min and max -------------------------------
posValorNoNeg :: [[Int]] -> [(Int,Int)]
posValorNoNeg listaDeListas = 
    [(x,y) | (x, filas) <- enumerador listaDeListas, (y,valor)<- enumerador filas, valor /= (-1) ]        
    where
        enumerador = zip [0..]


------------------------------------ Hace cero algunas casillas del Tablero ------------------------------------------
eliminaElementos :: Tablero -> [(Int,(Int,Int))] -> Int -> Tablero
eliminaElementos tablero listaRandom cant
    | cant == 0 = tablero
    | otherwise = eliminaElementos nuevoTablero resto (cant-1)
    where
        (cabeza:resto) = listaRandom
        (indice,pos) = cabeza
        listaDeListas = reemplaza pos 0 (matriz tablero)
        nuevoTablero = Tablero listaDeListas (minPos tablero) (maxPos tablero)