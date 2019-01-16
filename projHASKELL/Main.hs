module Main
    where

import           Data.Text.Unsafe
import           System.Exit
import           Text.Read

import           Determinante
import           Matriz_inversa
import           Utilidades


getInt :: IO Int
getInt = do
  entrada <- getLine
  case (readMaybe entrada :: Maybe Int) of
    Just x -> if x > 2 then return x else putStrLn "Dimensao deve ser maior que 2" >> getInt
    Nothing -> putStrLn "Isso não é um inteiro" >> getInt


getElemento :: Int -> Int -> String -> IO Float
getElemento linha coluna nome_matriz = do
    let saida = "Pegando elemento da linha " ++ show linha ++ " e da coluna " ++ show coluna ++ ", da matriz " ++ show nome_matriz
    putStrLn saida
    entrada <- getLine
    case (readMaybe entrada :: Maybe Float) of
        Just x -> return x
        Nothing -> putStrLn "Isso não é um numero" >> getElemento linha coluna nome_matriz


pegar_matriz_entrada_a :: Int -> [[Float]]
pegar_matriz_entrada_a n = [[ inlinePerformIO (getElemento (linha) (coluna) "a") | coluna <- [1.. n]] | linha <- [1.. n]]


pegar_matriz_entrada_c :: Int -> [[Float]]
pegar_matriz_entrada_c n = [[ inlinePerformIO (getElemento (linha) 1 "c") ] | linha <- [1.. n]]


main = do
    putStrLn ""
    putStrLn "Primeiro peguemos a dimensão (inteiro sem sinal):"
    n <- getInt
    let saida = "Então vai ser " ++ (show n) ++ " dimensões, logo vão ser " ++ show (n+1) ++ " vetores"  ++ ", sendo " ++ (show n) ++ " para a matriz A e mais 1 para a matriz C de solução"
    putStrLn saida
    let saida = "O calculo é feito da seguinte forma: A*X = C, onde X é a matriz coluna contendo o ponto da interseção dos " ++ (show n) ++ " planos, a qual vai ser a saida deste programa"
    putStrLn saida
    let entrada_a =  pegar_matriz_entrada_a n
    print entrada_a
    print "E representado internamente da forma acima"
    putStrLn "A seguinte matriz:"
    printar_matriz entrada_a
    putStrLn "C e uma matriz coluna"
    let entrada_c = pegar_matriz_entrada_c n
    print entrada_c
    putStrLn "Logo a matriz coluna C:"
    printar_matriz entrada_c
    let det = determinante entrada_a
    print det
    if det == 0 then do exitFailure else do putStrLn "Determinante diferente de zero, ok"
    let inv = matriz_inversa entrada_a
    let ponto = multiplicaMatrizes inv entrada_c
    print "Ponto comum a todos os planos: "
    printar_matriz ponto
