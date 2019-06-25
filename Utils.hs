module Utils(
formataCaracteristicas,
formataExemplos,
listaCaracteristicas,
--temMesmaClassificacao,
-- criaArvoreDecisao,
-- formataArvore,
-- computaResultado
) where

import Data.List (sort)

data Exemplo = T [(String, String)] String deriving (Show)
type Exemplos = [Exemplo]

data Caracteristica = Nominal String [String] | Numeral String [String] deriving (Show)
type Caracteristicas = [Caracteristica]

data Arvore = Nil | No Caracteristica [Arvore] deriving (Show)

criaRaiz caracteristicas = No caracteristicas [Nil]

formataExemplos []  _ = []
formataExemplos (xs:xss) caract = [T (zip (caract) (init xs)) (last xs)] ++ (formataExemplos xss caract)

listaCaracteristicas [] = []
listaCaracteristicas ((Numeral caract valores):caracteristicas) = [caract] ++ listaCaracteristicas caracteristicas
listaCaracteristicas ((Nominal caract valores):caracteristicas) = [caract] ++ listaCaracteristicas caracteristicas

formataCaracteristicas :: [[String]] -> Caracteristicas
formataCaracteristicas [] = []
formataCaracteristicas (xs:xss)
                    | length xs == 1 = [Numeral (head xs) []] ++ formataCaracteristicas xss
                    | otherwise = [Nominal (head xs) (tail xs)] ++ formataCaracteristicas xss

{- criaArvoreDecisao [] _ maisComum = maisComum
criaArvoreDecisao exemplos [] _ = maioria exemplos
criaArvoreDecisao exemplos caracteristicas maisComum
                        | temMesmaClassificacao exemplos = classificacao (head exemplos)
                        | otherwise = do let melhor = melhorTeste caracteristicas exemplos
                                         let arvore = criaRaiz melhor
 -}

temMesmaClassificacao :: Exemplos -> Bool
temMesmaClassificacao ((T caracteristicas classificacao):exemplos) = and [classificacao == _classificacao | (T _caracteristicas _classificacao) <- exemplos]


classificacao (T caracteristicas classificacao) = classificacao

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

-- melhorTeste caracteristicas exemplos = do let ig = calculaIG exemplos caracteristicas

valor (T caracteristicas classificacao) (Nominal caracteristica valores) = head [snd caract | caract <- caracteristicas, (fst caract) == caracteristica]

calculaEntropia percentages = sum [-percentagem*(log percentagem) | percentagem <- percentages]

{- calculaIG exemplo caracteristica = entropiaExemplos - somatorioCaracteristica
                            where entropiaExemplos = calculaEntropia (percentagemClasse $ dividirPorClasseExemplos $ ordenarPorClasse exemplos) -}