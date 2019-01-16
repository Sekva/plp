
-- Fará o processo final (uma funcao separada tornará mais fácil de entender o código)
alternaESoma :: Num a => [a] -> a

-- Seguinto o método de Laplace, na qual é escolhido uma determinada linha e coluna para ser removida e só assim manipular a submatriz gerada
removerColuna :: Int -> [[a]] -> [[a]]


-- Funcao 'principal'
determinante :: Num a => [[a]] -> a
--Chama a parte que calcula o determinante
determinante matriz = calcula matriz

	--Caso a matriz seja uma matriz 2x2, calcula o determinante
	where calcula [[x1, x2], [y1, y2]] = x1 * y2 - x2 * y1
		-- Primeiramente, é necessário que esse método utiliza o teorema de Laplace.
		--Dado um vetor de vetor, x é o primeiro elemento, e xs os demais
		-- Define em tamanho o tamanho do vetor x
		-- Soma o produto da determinante das submatrizes (matrizes 2x2) e o fator multiplicante (que nesse caso é um número da premira linha da matriz [fica mais fácil se tentar imaginar], que é o nosso x)
		-- Esse map com a função lambda faz a separação das submatrizes.
		-- Válido notar que o 'n' na funcao map está representando o índice da coluna a ser removida. Dessa forma, será gerado um vetor de tamanho 'l' com os demais resultados da determinante de uma matriz 2x2 (que é o caso escrido a cima [calcular])
		-- Dessa forma, é fácil de entender como ficará os seguintes processos
		calcula (x:xs) = let tamanho = length x in alternaESoma $ zipWith(*) x $ map(\n -> determinane $ removerColuna n xs) [0..l]


		--Outro caso base, onde a matriz é nula
		calcula [] = 0
