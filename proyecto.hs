--Lista De Alto Orden creado a través de cadenas puras y compuestas
listaISBN :: String -> [Int]
listaISBN = convertirLista . reemplazarX . cadenaALista
  where
    cadenaALista :: String -> [String]
    cadenaALista = map (\x -> [x])

    convertirLista :: [String] -> [Int]
    convertirLista = listaCadenasAEnteros

    reemplazarX :: [String] -> [String]
    reemplazarX lista = init lista ++ [if last lista == "X" then "10" else last lista]

    listaCadenasAEnteros :: [String] -> [Int]
    listaCadenasAEnteros = map read

--Funciones Puras
multiplicarPorDiez :: [Int] -> [Int]
multiplicarPorDiez = zipWith (*) [10,9..1]

verificarResiduoCero :: Int -> Bool
verificarResiduoCero x = x `mod` 11 == 0

--Funcion Compuesta de listaISBN(Una funcion de alto orden)
listGG :: [Int]
listGG = listaISBN "067973371X"
--Funciones compuestas 
listaResiduo ::[Int] 
listaResiduo = multiplicarPorDiez listGG

suma:: Int 
suma = sum listaResiduo

resultado :: Bool
resultado = verificarResiduoCero suma

