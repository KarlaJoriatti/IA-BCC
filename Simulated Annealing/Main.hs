{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
import System.IO
import Control.Monad
import Data.Char
import Text.Read
import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import System.Random


main = do
        (numVars, numClauses, clauses) <- fileRead "uf250-01.cnf"
        now <- getCurrentTime
        timezone <- getCurrentTimeZone
        let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now -- gerar seed aleatoria
        variaveis <- randomRsIO (0 :: Integer,1 :: Integer) numVars ((truncate second)*(truncate(second*10000)))
        let valoracao = valorarClausulas variaveis clauses
            --count = countClauses valoracao
        (solution, cl) <- sA variaveis valoracao variaveis valoracao 0 250000 1.0 numVars clauses
        print ("Numero de Variaveis: "++ show(numVars))
        print ("Numero de Clausulas: "++ show(numClauses))
        print (variaveis)
        print(solution)
        print(cl)

-- sA = simulated annealing
-- bS = best solution
-- cB = best solution's countClauses
-- aS = actual solution
-- cA = actual solution's countClauses
-- aI = actual iteration
-- mI = max Iteration
-- t = temperature
-- cl = clauses
sA bS 0 aS cA aI mI t numV cl = return (bS, 0)
sA bS cB aS cA 250000 mI t numV cl = return (bS, cB)
sA bS cB aS cA aI mI t numV cl = do
                                   viz <- criarVizinho numV aS 
                                   let val = valorarClausulas viz cl
                                       delta = cA - val
                                       i1 = chooseBest bS cB viz val
                                       tAtualizada = temperatura aI mI
                                   i2 <- chooseSolution aS viz delta t
                                   appendFile "temp.txt" (show tAtualizada ++ "\n")
                                   print("T = " ++ show tAtualizada)
                                   case i1 of 
                                     0 -> case i2 of
                                            0 -> appendFile "convergencia.txt" (show cA ++ "\n") >> print("cA =" ++ show cA) >> print("cB =" ++ show cB) >> sA bS cB aS cA (aI+1) mI tAtualizada numV cl >>= (return)
                                            1 -> appendFile "convergencia.txt" (show val ++ "\n") >> print("cA =" ++ show val) >> print("cB =" ++ show cB) >> sA bS cB viz val (aI+1) mI tAtualizada numV cl >>= (return)
                                     1 -> case i2 of
                                            0 -> appendFile "convergencia.txt" (show cA ++ "\n") >> print("cA =" ++ show cA) >> print("cB =" ++ show val) >> sA viz val aS cA (aI+1) mI tAtualizada numV cl >>= (return)
                                            1 -> appendFile "convergencia.txt" (show val ++ "\n") >> print("cA =" ++ show val) >> print("cB =" ++ show val) >> sA viz val viz val (aI+1) mI tAtualizada numV cl >>= (return) 
                                   
-- 1 = mudou
-- 0 = não mudou                                          
chooseBest bS cB viz cV = if cV < cB then 1 else 0       

re x delta t aS viz = if x < t then return 1 else return 0       

chooseSolution aS viz delta t = if delta > 0 then return 1 else chooseSolution' aS viz delta t
chooseSolution' aS viz delta t = do 
                                   now <- getCurrentTime
                                   timezone <- getCurrentTimeZone
                                   let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
                                       g = mkStdGen ((truncate second)*(truncate(second*10000)))
                                       (a, g') = randomR (0::Float, 1 :: Float) g
                                   --print("X = " ++ show a)
                                   re' <- re a delta t aS viz
                                   return re'


fileRead path = do 
                  g <- readFile path 
                  let l = lines g 
                      xs = removeC l 
                      (numVars : numClauses : []) = vars (head xs)
                      clauses = map reverse (map clausesToInt (map words (tail xs)))
                  return (numVars, numClauses, clauses)

removeC [] = []
removeC ((y:ys):xs) | y == '%' = []
                    | y == 'c' = removeC xs
                    | otherwise = (y:ys): removeC xs

varClause [] = []
varClause (x:xs) | isNumber x = x:xs
                 | otherwise = varClause xs

vars xs = map stringToInt (words (varClause xs))

clausesToInt [] = []
clausesToInt (x:xs) = stringToInt x: clausesToInt xs

stringToInt :: String -> Int
stringToInt s = read s

evaluation' [] (posicao, valor) = False
evaluation' (x:xs) (posicao, valor) | x < 0 = if posicao == abs(x) && valor == 0 then True
                                              else if posicao == abs(x) then False
                                                   else evaluation' xs (posicao, valor)
                                    | otherwise = if posicao == x && valor == 1 then True
                                                  else if posicao == x then False
                                                       else evaluation' xs (posicao, valor)

-- valorar uma unica clausula
evaluation xs l@(x1:x2:x3:x4:[]) | evaluation' (tail l) (abs(x2), xs !! (abs(x2)-1)) || evaluation' (tail l) (abs(x3), xs !! (abs(x3)-1)) || evaluation' (tail l) (abs(x4), xs !! (abs(x4)-1)) = 0
                                 | otherwise = 1

-- valorar todas as clausulas
valorarClausulas vars clauses = foldr (+) 0 (map (evaluation vars) clauses)


-- primeira geracao de variaveis random
randomRsIO :: (Random a) => (a, a) -> Int -> Int -> IO [a]
randomRsIO range numV seed = do
                               let g = mkStdGen seed  
                               return (take numV (randomRs range g))

flip' pos (x:xs) | pos == 1 = if x == 0 then 1:xs else 0:xs
                 | otherwise = x: flip' (pos-1) xs
flipin [] ls = ls
flipin (x:xs) ls = flipin xs (flip' x ls)

toInt :: Float -> Int
toInt = round

criarVizinho numV ls = do
                         now <- getCurrentTime
                         timezone <- getCurrentTimeZone
                         let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
                             g = mkStdGen ((truncate second)*(truncate(second*10000)))
                             (a, g') = randomR (1::Integer, truncate (sqrt(fromIntegral numV :: Float))) g
                         pos <- randomRsIO (1, numV) (fromInteger a) ((truncate second)*(truncate(second*10000)))
                         let vizinho = flipin pos ls
                         return vizinho


temperatura :: Float -> Float -> Float
temperatura iter n = (1 - iter/n)^5

