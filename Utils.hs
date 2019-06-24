module Utils(
formataCaracteriscas,
formataExemplos,
temMesmaClassificacao,
-- criaArvoreDecisao,
-- formataArvore,
-- computaResultado
) where

import Data.List (sort)

data Exemplo = T [String] String deriving (Show)
type Exemplos = [Exemplo]

data Caracterisca = Nominal String [String] | Numeral String [String] deriving (Show)
type Caracteriscas = [Caracterisca]

data Arvore = Nil | No Caracterisca [Arvore] deriving (Show)

criaRaiz caracteriscas = No caracteriscas [Nil]

formataExemplos :: [[String]] -> Exemplos
formataExemplos [] = []
formataExemplos (xs:xss) = [T (init xs) (last xs)] ++ formataExemplos xss

formataCaracteriscas :: [[String]] -> Caracteriscas
formataCaracteriscas [] = []
formataCaracteriscas (xs:xss)
                    | length xs == 1 = [Numeral (head xs) []] ++ formataCaracteriscas xss
                    | otherwise = [Nominal (head xs) (tail xs)] ++ formataCaracteriscas xss

{- criaArvoreDecisao [] _ maisComum = maisComum
criaArvoreDecisao exemplos [] _ = maioria exemplos
criaArvoreDecisao exemplos caracteristicas maisComum
                        | temMesmaClassificacao exemplos = classificacao (head exemplos)
                        | otherwise = do let melhor = melhorTeste caracteriscas exemplos
                                         let arvore = criaRaiz melhor
 -}

temMesmaClassificacao :: Exemplos -> Bool
temMesmaClassificacao ((T caracteristicas classificacao):exemplos) = and [classificacao == _classificacao | (T _caracteristicas _classificacao) <- exemplos]


classificacao (T caracteriscas classificacao) = classificacao

pegarClassesExemplos [] = []
pegarClassesExemplos ((T caracteristicas classificacao):exemplos) = [classificacao] ++ pegarClassesExemplos exemplos

ordenaPorClasse exemplos = (sort $ pegarClassesExemplos exemplos)

dividirPorClasseExemplos [] = []
dividirPorClasseExemplos exemplos = (takeWhile (==(head exemplos)) exemplos) ++  dropWhile (==(head exemplos)) exemplos

percentagemClasse classes = [(fromIntegral $ length mesmaClasse)/n | mesmaClasse <- classes]
                        where n = fromIntegral $ product $ map length classes

maioria exemplos = classeMajoritaria (ordenaPorClasse exemplos) []
                   where classeMajoritaria [] majoritaria = head majoritaria
                         classeMajoritaria classes majoritaria
                           | length classeAtual > length majoritaria = classeMajoritaria restante classeAtual
                           | otherwise = classeMajoritaria restante majoritaria
                           where classeAtual = takeWhile (==(head classes)) classes
                                 restante = dropWhile (==(head classes)) classes

-- melhorTeste caracteriscas exemplos = do let entropia = entropiaBase (percentagemClasse $ dividirPorClasseExemplos $ ordenarPorClasse exemplos)

entropiaBase percentages = sum [-percentagem*(log percentagem) | percentagem <- percentages]