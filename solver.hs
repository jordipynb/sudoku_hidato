module Solver where
import Data.List
import Data.Maybe
import Utils

------------------------------------------ MAIN ----------------------------------------------------
soluciona :: Utils.Tablero -> IO ()
soluciona tablero =
    printTablero nuevoTablero
    where
        -- Se empieza a llenar el tablero por donde esta el menor elemento
        (i,j) = (minPos tablero)
        valor = (matriz tablero)!!i!!j
        -- se busca la cantidad de casillas a completar
        ceros = cerosCantidad (matriz tablero)
        -- se llama a la funcion recursiva para resolver el tablero
        nuevoTablero = solucionaRecursivo tablero valor (i,j) ceros


--------------------------- Funcion Recursiva para solucionar el Tablero -----------------------------
solucionaRecursivo :: Tablero -> Int -> (Int, Int) -> Int -> Tablero
solucionaRecursivo tablero valor (i, j) ceros
    -- Si solo falta poner el ultimo numero entonces ya termino
    | ceros == 0 = nuevoTablero
    -- Si todas las casillas alrededor del numero estan ocupadas entonces no puedo poner un numero nuevo y retorno
    | noPuedoContinuar (i, j) (dx, dy) (matriz tablero) = Null
    -- Si el siguiente numero es negativo entonces es porque se invalido el tablero
    | nuevoValor < 0 = Null
    -- En otro caso busco la solucion del tablero
    | otherwise = siguienteTablero
    where
        -- Arrays direccionales
        dx = [0, 0,  1, 1,  1, -1, -1, -1]
        dy = [1, -1, 0, 1, -1,  1,  0, -1]
        -- Creo el nuevo tablero poniendo el numero en tablero anterior
        listaDeListas = reemplaza (i, j) valor (matriz tablero)
        nuevoTablero = Tablero listaDeListas (minPos tablero) (maxPos tablero)
        -- Calculo el siguiente  numero que voy a poner
        -- Si valor + 1 no esta en el tablero retorno ese, si no el siguiente numero que no esté
        nuevoValor = numSig (valor+1) nuevoTablero dx dy
        -- Encuentro la posicion del numero anterior
        -- Si me desplacé en nuevoValor tengo que encontrar otro punto de partida 
        -- xq el numero anterior no es el que acabo de poner
        (ni, nj) = posElemento listaDeListas (nuevoValor-1)
        -- Busco el siguiente board llamando recursivamente con la matriz direccional
        siguienteTablero = direccionalRecursivo 0

        direccionalRecursivo :: Int -> Tablero
        direccionalRecursivo indice
            -- Ya probé todas las direcciones
            | indice == length dx = Null
            -- Si no esta en rango o el elemento en la nueva posicion esta ocupado entonces no puedo poner el numero
            | not (enRango (iNueva, jNueva) listaDeListas) || listaDeListas!!iNueva!!jNueva /= 0 = direccionalRecursivo (indice + 1)
            -- Cuando encuentre la primera solucion valida retorno
            | solucion /= Null = solucion
            -- Si no he encontrado solucion pruebo con otra direccion
            | otherwise = direccionalRecursivo (indice + 1)
            where
                -- Calculo la nueva posicion
                (iNueva, jNueva) = (ni + dx!!indice, nj + dy!!indice)
                -- Llamo para buscar si poniendo el siguiente numero en la nueva posicion es solucion
                solucion = solucionaRecursivo nuevoTablero nuevoValor (iNueva, jNueva) (ceros-1)


------------------------ Retorna el siguiente valor a poner en el tablero -----------------------
numSig :: Int -> Tablero -> [Int] -> [Int] -> Int
numSig valor tablero dx dy
    -- si el numero no está en el tablero entonces el es el siguiente numero
    | all (\x -> valor `notElem` x) listaDeListas = valor
    -- si en cualquiera de sus alrededores esta el numero anterior entonces 
    -- el numero está bien posicionado, seguir buscando el numero
    | esValido = numSig (valor + 1) tablero dx dy  
    -- en otro caso se invalida el tablero porque el numero anterior esta muy lejos
    | otherwise = -1
    where
        listaDeListas = matriz tablero
        (x, y) = posElemento listaDeListas valor
        esValido = any (\x -> x == valor - 1) 
            [listaDeListas!!(x+xi)!!(y+yj) | xi <- dx, yj <- dy, enRango (x+xi, y+yj) listaDeListas]


--------------------------- Encuentra la posicion de un elemento  ---------------------------
posElemento :: [[Int]] -> Int -> (Int, Int)
posElemento listaDeListas elemnt = (x, y)
    where
        x = encuetraFila listaDeListas elemnt 0
        y = fromJust $ elemIndex elemnt (listaDeListas!!x)

------------------------- Retorna la fila de un elemento en la matriz -------------------------
encuetraFila :: [[Int]] -> Int -> Int -> Int 
encuetraFila listaDeListas elemnt index
    | index == length listaDeListas = -1
    | elemnt `elem` (listaDeListas !! index) = index
    | otherwise = encuetraFila listaDeListas elemnt (index + 1)