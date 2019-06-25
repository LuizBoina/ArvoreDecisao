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

formataExemplos :: [[String]] -> [String] -> Exemplos
formataExemplos []  _ = []
formataExemplos (xs:xss) caract = [T (zip (caract) (init xs)) (last xs)] ++ (formataExemplos xss caract)

listaCaracteristicas :: Caracteristicas -> [String]
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

--recebe uma base de exemplos e retorna as classificacoes
pegarClassesExemplos :: Exemplos -> [String]
pegarClassesExemplos [] = []
pegarClassesExemplos ((T caracteristicas classificacao):exemplos) = [classificacao] ++ pegarClassesExemplos exemplos

--agrupa classificacao por tipo
ordenaPorClasse :: Exemplos -> [String]
ordenaPorClasse exemplos = (sort $ pegarClassesExemplos exemplos)

--dividi grupos de classificacoes em listas
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

-- recebe um exemplo e o nome de uma caracteristica e retorna o valor dessa caracteristica no exemplos
valor :: Exemplo -> String -> String
valor (T caracteristicas classificacao) caracteristica = head [snd caract | caract <- caracteristicas, (fst caract) == caracteristica]

calculaEntropia percentages = sum [-percentagem*(log percentagem) | percentagem <- percentages]

calculaIG exemplos caracteristica = entropiaExemplos - somatorioCaracteristica
                                   where entropiaExemplos = calculaEntropia (percentagemClasse $ dividirPorClasseExemplos $ ordenarPorClasse exemplos)

somatorioCaracteristica (ex:exs) (Nominal caracteristica valores) = valor ex 