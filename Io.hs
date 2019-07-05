module Io
(
lerCasos,
imprimeResultados,
lerDescricao,
lerBase,
lerCaso,
imprimeArvore,
imprimeResultado
) where

import System.IO

formataEntrada file = map words $ lines file

lerDescricao = do hDescricao <- readFile "descricao.txt"
                  let descricao =  formataEntrada hDescricao
                  return descricao

lerBase = do hBase <-readFile "base.txt"
             let base = formataEntrada hBase
             return base

lerCaso = do hCaso <- readFile "caso.txt"
             let caso = words hCaso
             return caso

lerCasos = do hCasos <- readFile "caso.txt"
              let casos = formataEntrada hCasos
              return casos

imprimeResultado resultado = writeFile "result.txt" resultado

imprimeResultados [] formatada = writeFile "result.txt" formatada
imprimeResultados (x:xs) formatada = imprimeResultados xs (formatada++(x++"\n"))

imprimeArvore arvore = writeFile "arvore.txt" arvore