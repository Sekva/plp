-- Codigo de terceiros --------------------------------
-------------------------------------------------------

sPermutations :: [a] -> [([a], Int)]
sPermutations = flip zip (cycle [1, -1]) . foldl aux [[]]
  where
    aux items x = do
      (f, item) <- zip (cycle [reverse, id]) items
      f (insertEv x item)
    insertEv x [] = [[x]]
    insertEv x l@(y:ys) = (x : l) : ((y :) <$>) (insertEv x ys)

elemPos :: [[a]] -> Int -> Int -> a
elemPos ms i j = (ms !! i) !! j

prod
  :: Num a
  => ([[a]] -> Int -> Int -> a) -> [[a]] -> [Int] -> a
prod f ms = product . zipWith (f ms) [0 ..]

sDeterminant
  :: Num a
  => ([[a]] -> Int -> Int -> a) -> [[a]] -> [([Int], Int)] -> a
sDeterminant f ms = sum . fmap (\(is, s) -> fromIntegral s * prod f ms is)

determinant
  :: Num a
  => [[a]] -> a
determinant ms =
  sDeterminant elemPos ms . sPermutations $ [0 .. pred . length $ ms]

permanent
  :: Num a
  => [[a]] -> a
permanent ms =
  sum . fmap (prod elemPos ms . fst) . sPermutations $ [0 .. pred . length $ ms]

------------------------------------------------------
------------------------------------------------------

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

------------------------------------------------------
------------------------------------------------------


-- Matriz de teste
m = [[3, 5, 1],[2, -1, 0],[-1, 3, 1]]
--m = [[1, 2, 3],[4, 5, 6],[7, 8, 9]]


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


-- Calculo de matriz adjunta direto criando uma matriz e preenchendo enquanto transpõe
matriz_adjunta :: Num a => [[a]] -> [[a]]
matriz_adjunta [] = []
matriz_adjunta matriz =
    -- Apenas o determinante dá a matriz menor, multiplicando por (-1)^(i+j) temos a variação de cofatores
    -- (tabuleiro de xadrez onde o primeiro elemento é (-1)^(1+1) == 1).
    -- Para cada coluna da matriz original é uma nova linha com os elementos calculados,
    -- logo é a matriz transposta da matriz de cofatores, portanto a matriz adjunta
  [
    [ (-1)^(i+j) * determinant (remover_nesima_linha i (remover_nesima_coluna j matriz))
    | i <- [0.. -1+(length matriz)]
    ]
  | j <- [0.. -1+(length matriz)]
  ]


-- Multiplicando cada elemento da matriz por um escalar qualquer
matriz_por_escalar escalar matriz = [[ escalar * pegar_nesimo i (pegar_nesimo j matriz) | i <- [0.. -1+(length matriz)]] | j <- [0.. -1+(length matriz)]]


-- Matriz inversa da forma 1/det(A) * adj(A)
matriz_inversa matriz = matriz_por_escalar (1/(determinant m)) (matriz_adjunta m)

-- Testes
main = do
    print "Iniciando..."
    print $ determinant m
    printar_matriz m
    printar_matriz (multiplicaMatrizes m (matriz_inversa m))
