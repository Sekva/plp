module Matriz_inversa
    where


import           Determinante
import           Utilidades


-- Calculo de matriz adjunta direto criando uma matriz e preenchendo enquanto transpõe
matriz_adjunta :: Num a => [[a]] -> [[a]]
matriz_adjunta [] = []
matriz_adjunta matriz =
    -- Apenas o determinante dá a matriz menor, multiplicando por (-1)^(i+j) temos a variação de cofatores
    -- (tabuleiro de xadrez onde o primeiro elemento é (-1)^(1+1) == 1).
    -- Para cada coluna da matriz original é uma nova linha com os elementos calculados,
    -- logo é a matriz transposta da matriz de cofatores, portanto a matriz adjunta
  [
    [ (-1)^(i+j) * determinante (remover_nesima_linha i (remover_nesima_coluna j matriz))
    | i <- [0.. -1+(length matriz)]
    ]
  | j <- [0.. -1+(length matriz)]
  ]


-- Multiplicando cada elemento da matriz por um escalar qualquer
matriz_por_escalar escalar matriz = [[ escalar * pegar_nesimo i (pegar_nesimo j matriz) | i <- [0.. -1+(length matriz)]] | j <- [0.. -1+(length matriz)]]


-- Matriz inversa da forma 1/det(A) * adj(A)
matriz_inversa matriz = matriz_por_escalar (1/(determinante matriz)) (matriz_adjunta matriz)
