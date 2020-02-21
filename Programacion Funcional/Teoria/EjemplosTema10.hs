
module EjemplosTema10 where

import Data.Char   -- para poder usar la funcion toUpper

-- Programa que lee n caracteres del teclado:

leer :: Int -> IO String
leer 0 = return []
leer n =  getChar >>= q 
             where q c = leer (n-1) >>= r
                               where r cs = return (c:cs)

-- Programa equivalente a leer (con notación do):

leer' :: Int -> IO String
leer' 0 = return []
leer' n  =  do  
               c  <- getChar
               cs <- leer' (n-1)
               return (c:cs)

-- Y otro programa tambien equivalente (con notación do)

leer'' :: Int -> IO String
leer'' 0 = return []
leer'' n  =  do  
               cs <- leer'' (n-1)
               c  <- getChar
               return (cs++[c])

----------------------------------------------------------------------------------------------

-- Vamos a simular la función predefinida getLine con leerLinea
-- para leer una línea del teclado (hasta el return):

leerLinea :: IO String                           -- predefinido: getLine
leerLinea  =  getChar  >>= q 
                    where  
                    q c = if c == '\n' 
                             then return ""
                             else  leerLinea >>= r
                                     where r cs = return (c:cs)


-- Programa equivalente a leerLinea (con notación do):

leerLinea' :: IO String
leerLinea'  =  do   
                    c  <- getChar
                    if c == '\n' 
                    then return ""
                    else   do  
                              cs <- leerLinea'
                              return (c:cs)
-----------------------------------------------------------------------------------------

-- Programas que leen un carácter y lo imprime en mayúscula 

prog0 = getChar >>= f
             where  f x = putChar (toUpper x)

prog1 = getChar >>= f
            where  f x = putChar (toUpper x) >> putChar '\n'

prog1' = do
             x <- getChar
             putChar (toUpper x) 
             putChar '\n'

---------------------------------------------------------------------------------------------

-- Programa que lee un entero

leerEnt :: IO Int
leerEnt =  do
               e <- getLine
               return (read e)

-- Programa que lee una lista de enteros (escrita como lista):

leerLisEnt :: IO [Int]
leerLisEnt = do   
                   lin <- getLine
                   return (read lin)

-- Programa que lee una lista de enteros, aplica la función parámetro e imprime la lista resultante:

mapLisEnt :: Show a => (Int -> a) -> IO( )
mapLisEnt f  = do  
                      lis <- leerLisEnt
                      print (map f  lis)

-- Programa que lee enteros (uno en cada línea) y los devuelve en una lista:

leerLisEnt2:: IO [Int]
leerLisEnt2 =  do 
                       lis <- getLine
                       if lis == "" then return []
                       else do
                              resto <- leerLisEnt2
                              return ( (read lis) : resto )

-- Programa que lee enteros (uno en cada línea) e imprime la suma de todos ellos:

leeYsuma :: IO ( )
leeYsuma = do
                    lisEnt <- leerLisEnt2
                    print (sum lisEnt)

------------------------------------------------------------------------------------------------------------------------

-- Programas que tratan ficheros. Cambiar el path !!!


prog2 = do
             s <- readFile "C:/Users/Marisa/Desktop/entrada.txt"
             writeFile "C:/Users/Marisa/Desktop/salida.txt" (map toUpper s) 
             putStrLn "salida.txt es entrada.txt en mayusculas"

prog3 = do 
             s <- readFile "C:/Users/Marisa/Desktop/entrada.txt"
             appendFile "C:/Users/Marisa/Desktop/salida.txt"
                             ( "\n\n\t\t -- En mayusculas queda: --\n\n" ++ (map toUpper s))
             putStrLn "annadido a salida.txt"
