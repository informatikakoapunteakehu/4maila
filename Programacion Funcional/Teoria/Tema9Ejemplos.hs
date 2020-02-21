-----------------------------------------------------------------------------------------------------
--  Programacion Funcional                curso 2017/18

-- Ejemplos de programas eficientes (Tema 9) 
-- Recuerda :set +s para que el sistema muestre recursos consumidos
--------------------------------------------------------------------------------------------------------

-- Inversa de una lista   -----------------------------------------------------

inversa [] = [] 
inversa (x:s) = inversa s ++ [x] 

inversaE s = invConc [] s
invConc t [] = t 
invConc t (x:s) = invConc (x:t) s


-- N-simo número de Fibonacci (n>=0) -------------------------------

fib 0 = 0 
fib 1 = 1 
fib n = fib (n-1) + fib (n-2)


fibE n = fst (fibDos n)
fibDos 0 = (0,1) 
fibDos n = (b, a+b) where (a,b) = fibDos (n-1)


-- Mínimo de una lista, usando la ordenación por inserción ----

minimoLista :: Ord a => [a] -> a
minimoLista  = head . ordIns

ordIns :: Ord a => [a] -> [a]
ordIns = foldr ins []
               where
               ins x [] = [x]
               ins x (y:s) = if x<=y then (x:y:s) else (y: ins x s)



-- Problema de las 8 reinas ------------------------------------------------------

soluciones = reinas 8
reinas 0 = [[]]
reinas m = [ p++[n] | p <- reinas (m-1), n <- [1..8] , seguro p n]

seguro p n = and [not (mata (i,j) (k,n)) | (i,j) <- zip [1..h] p]
                    where h = length p ; k = h+1

mata (i,j) (k,n) = (j == n) || (i+j == k+n) || (i-j == k-n)

-- soluciones             -- da las 92 soluciones
-- head soluciones     -- da la primera solución (en menos tiempo)


-- Números de Hamming --------------------------------------------------------------

hamming = 1: f (hamming)
                  where 
                  f s = merge3 (por 2 s) (por 3 s) (por 5 s)
                  por n = map (n*)

merge3 as bs cs = merge as (merge bs cs)

merge (x:s) (y:t) 
         | x==y    = x: merge s t
         | x<y      = x: merge s (y:t)
         | x>y      = y: merge (x:s) t

--------------------------------------------------------------------------------------------------------

pares = [2,4..]
pares' = 2:map (2+) pares'


sumas = [sumaHasta i | i <- [1..]]
  where sumaHasta i = sum [1..i]


sumas' = zipWith (+) [1..] (0:sumas')
sumas'' = 1:(zipWith (+) [2..] sumas'')

fib' = 0 : 1 : zipWith (+) fib' (tail fib')

factoriales' = zipWith (*) [1..] (1:sumas')