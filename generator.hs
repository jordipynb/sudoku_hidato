module Generator where
import Data.List
import Data.Maybe
import System.Random
import Utils

-------------------------------------- Generar un Tablero Cascada ---------------------------------------
cascadaTab :: Int -> Int -> Tablero
cascadaTab n m =  
    Tablero {matriz = [[
        if 
            esFactible x y m n (m `div` 2) (n `div` 2) == True
        then (-1) 
        else (0) 
        | x <- [1..m]] | y <- [1..n]], 
        minPos = (0,0), maxPos = (n-1,m-1)}

esFactible :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
esFactible x y m n mVal nVal
    | ((x>(m-1))||(y>(n-1)))&&((x>y+1)||(y>x+1)) = True
    | mVal == 1 && nVal == 1 = False
    | (((x>(m-mVal))||(y>(n-nVal)))&&((x>y+mVal)||(y>x+nVal))) || esFactible x y m n mNuevo nNuevo = True
    | otherwise = False
    where
        mNuevo = max 1 (mVal-1)
        nNuevo = max 1 (nVal-1)

----------------------------------- Generar un Tablero Estandar ----------------------------------------
estandarTab :: Int -> Int -> Tablero
estandarTab n m = Tablero {matriz = [[0 | _ <- [1..m]] | _ <- [1..n]], minPos = (0,0), maxPos = (n-1,m-1)}


-------------------------------------- Generar un Tablero Cruz ------------------------------------------
cruzTab :: Int -> Int -> Tablero
cruzTab n m = Tablero {matriz = [[ if ((x==y)||(x==(n+1-y))) then (-1) else (0)| x <- [1..m]] | y <- [1..n]], minPos = (0,0), maxPos = (n-1,m-1)}


------------------------------- Generar un Tablero Triangular Inferior ----------------------------------
triangularInfTab :: Int -> Int -> Tablero
triangularInfTab n m = Tablero {matriz = [[ if (x>y) then (-1) else (0)| x <- [1..m]] | y <- [1..n]], minPos = (0,0), maxPos = (n-1,m-1)}


------------------------------- Generar un Tablero Triangular Superior ----------------------------------
triangularSupTab :: Int -> Int -> Tablero
triangularSupTab n m = Tablero {matriz = [[ if (x<y) then (-1) else (0)| x <- [1..m]] | y <- [1..n]], minPos = (0,0), maxPos = (n-1,m-1)}


-------------------------------------------- MAIN -------------------------------------------------------
genera n m = do
    semilla <- newStdGen

    randomForma <- randomRIO (0,4) 
    let formas = [(cruzTab n m),(estandarTab n m),(cascadaTab n m),(triangularInfTab n m),(triangularSupTab n m)]
    let selecc = formas!!randomForma
    
    let tableroEscogido = (selecc)

    minVal <- randomRIO (1,9) 
    
    -- la posicion inicial para el valor
    let listaPos = posValorNoNeg (matriz tableroEscogido)
    randindice <- randomRIO (0,(length (listaPos) `div` 2)) -- para que el rango en la semilla no sea tan mala
    let iniIndice = listaPos!!(randindice)

    -------rellena el tablero
    let tablero = rellena tableroEscogido iniIndice minVal

    ---- falta lo de remplazar con 0 los valores de las casilla q decidas quitar
    -------posiciones ocupadas
    let listaPosOcup1 = posValorNoNeg (matriz tablero)
    let listaPosOcup2 = elimina listaPosOcup1 (minPos tablero)
    let listaPosOcupadas = elimina listaPosOcup2 (maxPos tablero)
    let n = length listaPosOcupadas

    let lista = map(\x -> (mod x n)) (listaRandom n semilla)
    let listaEliminar = sort (zip (lista) listaPosOcupadas)

    let tableroGenerado = eliminaElementos tablero listaEliminar ((n * 2) `div` 3)

    return (tableroGenerado)

---------------------------------------- lista random de enteros ----------------------------------------
listaRandom :: Int -> StdGen -> [Int]
listaRandom n = take n . unfoldr (Just . random)

elimina :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
elimina (cabeza:resto) valor = 
    if x1 == x2 && y1 == y2 then resto else cabeza:(elimina resto valor)
    where
        (x1,y1) = cabeza
        (x2,y2) = valor

----------------------------------------- Rellenar el tablero -------------------------------------------
rellena :: Tablero -> (Int,Int) -> Int -> Tablero
rellena tablero pos valor = nuevoTablero
    where
        maximo = cerosCantidad (matriz tablero) + (valor - 1) -- el ultimo numero
        -- se llama a la funcion recursiva para resolver el tablero
        nuevoTablero = rellenaRecursivo tablero valor pos maximo
        
        rellenaRecursivo :: Tablero -> Int -> (Int, Int) -> Int -> Tablero
        rellenaRecursivo tablero valor (i, j) maximo
            -- Si solo falta poner el ultimo numero entonces ya termino
            | valor == maximo = nuevoTablero
            -- Si todas las casillas alrededor del numero estan ocupadas entonces no puedo poner un numero nuevo y retorno
            | noPuedoContinuar (i, j) (dx, dy) (matriz tablero) = Null
            -- En otro caso busco la solucion del tablero
            | otherwise = siguienteTablero
            where
                -- Arrays direccionales
                dx = [0, 0,  1, 1,  1, -1, -1, -1]
                dy = [1, -1, 0, 1, -1,  1,  0, -1]
                -- Creo el nuevo tablero poniendo el valor en tablero anterior
                listaDeListas = reemplaza (i, j) valor (matriz tablero)
                nuevoTablero = Tablero listaDeListas pos (i,j)
                -- Siguiente numero que a poner
                nuevoValor = valor + 1
                -- Busco el siguiente tablero llamando recursivamente con la matriz direccional
                siguienteTablero = direccionalRecursivo 0

                direccionalRecursivo :: Int -> Tablero
                direccionalRecursivo indice
                    -- Ya probé todas las direcciones
                    | indice == length dx = Null
                    -- Si no esta en rango o el elemento en la nueva posicion esta ocupado entonces no puedo poner el numero
                    | not (enRango (iNueva, jNueva) listaDeListas) || listaDeListas!!iNueva!!jNueva /= 0 = direccionalRecursivo (indice + 1)
                    -- Si el numero de ceros conectados es menor que los ceros que quedan entonces no es una solucion valida
                    | seDesconecta (matriz nuevoTablero) i j && cerosConectados (matriz nuevoTablero) iNueva jNueva < maximo - valor = direccionalRecursivo (indice + 1)
                    -- Cuando encuentre la primera solucion valida retorno
                    | solucion /= Null = solucion
                    -- Si no he encontrado solucion pruebo con otra direccion
                    | otherwise = direccionalRecursivo (indice + 1)
                    where
                        -- Calculo la nueva posicion
                        (iNueva, jNueva) = (i + dx!!indice, j + dy!!indice)
                        -- Llamo para buscar si poniendo el siguiente numero en la nueva posicion es solucion
                        solucion = rellenaRecursivo nuevoTablero nuevoValor (iNueva, jNueva) maximo

