module Determinante
    where

import           Utilidades

-- Funcao 'principal'
determinante :: Num a => [[a]] -> a
--Chama a parte que calcula o determinante
determinante matriz = calcula matriz

    --Caso a matriz seja uma matriz 2x2, calcula o determinante
    where
        calcula [[x1, x2], [y1, y2]] = x1 * y2 - x2 * y1
        -- Primeiramente, é necessário que esse método utiliza o teorema de Laplace.
        --Dado um vetor de vetor, x é o primeiro elemento, e xs os demais
        -- Define em tamanho o tamanho do vetor x
        -- Soma o produto da determinante das submatrizes (matrizes 2x2) e o fator multiplicante (que nesse caso é um número da premira linha da matriz [fica mais fácil se tentar imaginar], que é o nosso x)
        -- Esse map com a função lambda faz a separação das submatrizes.
        -- Válido notar que o 'n' na funcao map está representando o índice da coluna a ser removida. Dessa forma, será gerado um vetor de tamanho 'l' com os demais resultados da determinante de uma matriz 2x2 (que é o caso escrido a cima [calcular])
        -- Dessa forma, é fácil de entender como ficará os seguintes processos
        calcula (x:xs) = let tamanho = length x in alternaESoma $ zipWith(*) x $ map(\n -> determinante $ removerColuna n xs) [0..tamanho]
        -- Coisas válidas de serem notadas:
        -- A função zipWith nada mais faz do que uma ação aplicada nos elementos de dois vetores. No nosso caso, queremos multiplicar dois números, que nesse caso é o numero do elemento escolhido na qual exclui a linha a linha e a coluna por o determinante da sub-matriz gerada.
        -- O '$' nada mais é do que uma forma de "dizer" que dalí para o final da linha é parametro para o método anteposto. Por exemplo: [ soma $ a b ]
        -- No método map foi utilizado a função lambda, no entando, poderia ser feito com mais um where, no entanto, acredito que ficaria mais complicaod de entender: { map calculaDeterminante' [0..l] where calculaDeterminante' p = determinante $ removerColuna p xs }


        --Outro caso base, onde o determinante é nulo
        calcula [] = 0
