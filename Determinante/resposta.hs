
-- Fará o processo final (uma funcão separada tornará mais fácil de entender)
alternaESoma :: Num a => [a] -> a
alternaESoma [] = 0
alternaESoma vetor = somar vetor 0 0
	-- Primeiramente, lembre-se do método de Laplace. Lembre-se que precisamos alternar os sinais das somas ao obtermos os valores dos produtos das determinantes das submatrizes por o elemento escolhido.
	-- É simples de entender lendo como esse código funciona
	-- Utiliza-se um indice para somar os valores gerados pelo produto. Na qual, dado que o indice seja par, o valor é somado com o resultado, caso seja par, é subtraído.
	where
		somar (x:xs) indice resultado = if even indice then somar xs (indice + 1) (resultado + x) else somar xs (indice + 1) (resultado - x)
			-- Independente do indice, caso o vetor seja vazio, o resultado será o mesmo.
		somar [] _ resultado = resultado



-- Seguinto o método de Laplace, na qual é escolhido uma determinada linha e coluna para ser removida e só assim manipular a submatriz gerada
removerColuna :: Int -> [[a]] -> [[a]]
-- Para cada linha da matriz, será removido n-ésimo elemento.
-- Ficou bem simplificado essa forma (saber usar a função map ajuda bastante)
removerColuna indice = map(removerIndice indice)

removerIndice :: Int -> [a] -> [a]
-- Remove primeira coluna
removerIndice 0 (x:xs) = xs
-- Dado que 'xs' é toda a matriz menos a primeira coluna, utiliza-se (n-1) ao chamar novamente removerIndice
-- Essa recursão será feita até que entre no caso em que o íncide a ser removido seja igual a 0
removerIndice n (x:xs) = x : removerIndice (n-1) xs
-- Caso genérico, onde o indice não importa
removerIndice _ [] = []


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
