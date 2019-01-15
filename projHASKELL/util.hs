m = [[1, 2, 3],[4, 5, 6],[7, 8, 9]]

pegar_nesimo :: Int -> [a] -> a
pegar_nesimo n vetor = vetor!!n

printar_matriz [] = print "ok"
printar_matriz matriz = do
    print (head matriz)
    printar_matriz (tail matriz)

remover_nesima_linha :: Int -> [a] -> [a]
remover_nesima_linha n matriz = do
    let pt1 = take n matriz
    let pt2 = snd (splitAt (n+1) matriz)
    pt1 ++ pt2

inserir :: [a] -> [a] -> [[a]]
inserir v1 v2 = [v1,v2]


-- Refatorar isso aqui né
remover_nesima_coluna n matriz = do
    if (length matriz) == 1
        then do
            let pt = inserir [] (remover_nesima_linha n (head matriz))
            let pt1 = remover_nesima_linha 0 pt
            pt1;
        else do
            let pt = inserir [] (remover_nesima_linha n (head matriz))
            let pt1 = remover_nesima_linha 0 pt
            pt1 ++ remover_nesima_coluna n (tail matriz);


pegar_submatriz :: Int -> Int -> [[Int]] -> [[Int]]
pegar_submatriz linha coluna matriz = do
    remover_nesima_linha (linha - 1) (remover_nesima_coluna (coluna - 1) matriz)

trocar_elemento_lista  :: Int -> a -> [a] -> [a]
trocar_elemento_lista _ _ [] = []
trocar_elemento_lista pos valor (pt1:comp)
    | pos == 0 = valor:comp
    | otherwise = pt1:trocar_elemento_lista (pos-1) valor comp


mudar_elemento :: Int -> Int -> Int -> [[Int]] -> [[Int]]
mudar_elemento novoValor linha coluna matriz = do
    let index_linha = linha - 1
    let index_coluna = coluna - 1
    let linha = matriz!!index_linha
    let linha_mudada = trocar_elemento_lista index_coluna novoValor linha
    trocar_elemento_lista index_linha linha_mudada matriz


main = do
    print "Iniciando..."
    print (mudar_elemento 12 2 2 m)
    print $ pegar_nesimo 2 m
    --printar_matriz (mudar_elemento 12 2 2 m)
