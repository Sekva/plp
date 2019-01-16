module Utilidades
    where


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
removerIndice _ []     = []


multiplicaMatrizes:: Num a => [[a]] -> [[a]] -> [[a]]  --Função para multiplicar a matriz inversa de A com o vetor de resultados
multiplicaMatrizes xs ys = multiplicarAdicionar (*) (+) xs ys       --Chamada da função que de fato faz as operações

{- Basicamente a função abaixo vai passando vetor por vetor (linha por linha) para as outras funções, pra que cada linha
da nova matriz possa ser calculada, foi utilizado lambda
-}
multiplicarAdicionar::(a -> b -> c) -> (c -> c -> c) -> [[a]] -> [[b]] -> [[c]]     -- Funçãozinha que vai fazendo elemento por elemento
multiplicarAdicionar f g xs ys = map (\us -> juntandoElemntosVetor (\u vs -> map (f u) vs) (zipWith g) us ys) xs

{-
  A função abaixo verifica possiveis erros e depois manda pra outra função calcular os elementos da nova matriz
-}
juntandoElemntosVetor::(a -> b -> c) -> (c -> c -> c) -> [a] -> [b] -> c
juntandoElemntosVetor _ _ [] _          = error "Lista inicial vazia"
juntandoElemntosVetor _ _ _ []          = error "Segunda lista vazia"
juntandoElemntosVetor f g (x:xs) (y:ys) = juntandoElemntos f g (f x y) xs ys

{-
  No fim de tudo é aqui que tudo acontece, cada elemento vai sendo calculado de acordo com as linhas e as colunas das matrizes iniciais
  (lembrando que um elemnto x = XA1.XC1 + YA1.XC2 + ZA1 . XC3 + ...)
-}

juntandoElemntos::(a -> b -> c) -> (d -> c -> d) -> d -> [a] -> [b]  -> d
juntandoElemntos _ _ u [] _          = u
juntandoElemntos _ _ u _ []          = u
juntandoElemntos f g u  (x:xs)  (y:ys) = juntandoElemntos f g  (g u $ f x y) xs ys


-- Pega o (n+1)º elemento de vetor
pegar_nesimo :: Int -> [a] -> a
pegar_nesimo n vetor = vetor!!n


-- Um print de matriz bem simples
-- Se receber um tail vazio, printa ok
printar_matriz [] = print "ok"
printar_matriz matriz = do
    -- Printa a primeira linha
    print (head matriz)
    -- Chama recursivamente para todas as outras linhas
    printar_matriz (tail matriz)


-- Remove a (n+1)ª linha da matriz
remover_nesima_linha :: Int -> [a] -> [a]
remover_nesima_linha n matriz = do
    -- Pega os elementos até o nº excluido ele
    let pt1 = take n matriz
    -- Pega os elementos a partir do nº, excluindo ele
    let pt2 = snd (splitAt (n+1) matriz)
    -- Concatena tudo, ou seja, uma lista sem o nº elemento
    pt1 ++ pt2


-- Mini "gambiarra" pra montar uma matriz a partir de uma lista
inserir :: [a] -> [a] -> [[a]]
inserir v1 v2 = [v1,v2]


-- Remove a (n+1)ª coluna da matriz
-- Refatorar isso aqui né, ta pegando pt de toda forma, mas por agora nao vou mexer
remover_nesima_coluna n matriz = do
    if (length matriz) == 1
        then do
            -- Se o tamanho da matriz for 1, tem apenas uma linha, então basta remover a coluna (head usado
            -- porque ainda é [[a]])
            let pt = inserir [] (remover_nesima_linha n (head matriz))
            -- Limpando o [] de [[], [a]]
            let pt1 = remover_nesima_linha 0 pt
            pt1;
        else do
            -- Se for maior que 1, executo a mesma coisa e chamo recursivamente para o resto exceto por esta lista
            let pt = inserir [] (remover_nesima_linha n (head matriz))
            let pt1 = remover_nesima_linha 0 pt
            pt1 ++ remover_nesima_coluna n (tail matriz);


-- Pega a matriz resultante da eliminação da linha e coluna passadas
pegar_submatriz :: Int -> Int -> [[Float]] -> [[Float]]
pegar_submatriz linha coluna matriz = do
    remover_nesima_linha (linha - 1) (remover_nesima_coluna (coluna - 1) matriz)


-- Substitui o valor de um elemento da lista por outro
trocar_elemento_lista  :: Int -> a -> [a] -> [a]
trocar_elemento_lista _ _ [] = []
-- Se pos == 0, já foi percorrido recursivamente todos os elementos da lista
-- e é concatenado o que vem antes com o valor e com o complemento, substituindo o valor da posição
trocar_elemento_lista pos valor (pt1:comp)
    | pos == 0 = valor:comp
    | otherwise = pt1:trocar_elemento_lista (pos-1) valor comp


-- Substitui o valor de um elemento da matriz por outro
mudar_elemento :: Float -> Int -> Int -> [[Float]] -> [[Float]]
mudar_elemento novoValor linha coluna matriz = do
    let index_linha = linha - 1
    let index_coluna = coluna - 1
    let linha = matriz!!index_linha
    -- Pegando a linha, que é apenas uma lista, basta trocar o valor usando trocar_elemento_lista
    let linha_mudada = trocar_elemento_lista index_coluna novoValor linha
    -- Tendo a linha mudada, basta trocar a linha original pela mudada usando trocar_elemento_lista
    trocar_elemento_lista index_linha linha_mudada matriz


-- Função simples de transpor matriz usando submatrizes transpostas
matriz_transposta :: [[a]] -> [[a]]
matriz_transposta ([]:_) = []
matriz_transposta matriz = do
    (map head matriz) : matriz_transposta (map tail matriz)
