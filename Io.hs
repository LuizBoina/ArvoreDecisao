module Io
(
lerDescricao,
lerBase,
lerCaso,
-- imprimeArvore,
-- imprimeResult
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