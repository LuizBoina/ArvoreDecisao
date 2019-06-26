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
formataExemplos (xs:xss) caract = (T (zip (caract) (init xs)) (last xs)):(formataExemplos xss caract)

listaCaracteristicas :: Caracteristicas -> [String]
listaCaracteristicas [] = []
listaCaracteristicas ((Numeral caract valores):caracteristicas) = caract:(listaCaracteristicas caracteristicas)
listaCaracteristicas ((Nominal caract valores):caracteristicas) = caract:(listaCaracteristicas caracteristicas)

formataCaracteristicas :: [[String]] -> Caracteristicas
formataCaracteristicas [] = []
formataCaracteristicas (xs:xss)
                    | length xs == 1 = (Numeral (head xs) []):(formataCaracteristicas xss)
                    | otherwise = (Nominal (head xs) (tail xs)):(formataCaracteristicas xss)

{- criaArvoreDecisao [] _ maisComum = maisComum
criaArvoreDecisao exemplos [] _ = maioria exemplos
criaArvoreDecisao exemplos caracteristicas maisComum
                        | temMesmaClassificacao exemplos = classificacaoEx (head exemplos)
                        | otherwise = do let melhor = melhorTeste caracteristicas exemplos
                                         let arvore = criaRaiz melhor
 -}

classificacaoEx (T caract classificacao) = classificacao

temMesmaClassificacao :: Exemplos -> Bool
temMesmaClassificacao ((T caracteristicas classificacao):exemplos) = and [classificacao == _classificacao | (T _caracteristicas _classificacao) <- exemplos]

--agrupa classificacao por tipo
ordenaClasseEx :: Exemplos -> [String]
ordenaClasseEx exemplos = sort [classi | (T caract classi)<-exemplos]

--dividi grupos de classificacoes em listas
dividirPorClasseExemplos [] = []
dividirPorClasseExemplos exemplos = (takeWhile (==(head exemplos)) exemplos) ++  dropWhile (==(head exemplos)) exemplos

percentagemClasse classes = [(fromIntegral $ length mesmaClasse)/n | mesmaClasse <- classes]
                            where n = fromIntegral $ product $ map length classes

maioria exemplos = classeMajoritaria (ordenaClasseEx exemplos) []
                   where classeMajoritaria [] majoritaria = head majoritaria
                         classeMajoritaria classes majoritaria
                           | length classeAtual > length majoritaria = classeMajoritaria restante classeAtual
                           | otherwise = classeMajoritaria restante majoritaria
                           where classeAtual = takeWhile (==(head classes)) classes
                                 restante = dropWhile (==(head classes)) classes

-- melhorTeste caracteristicas exemplos = do let ig = calculaIG exemplos caracteristicas

pegaValores (Numeral caract valores) = sort valores
pegaValores (Nominal caract valores) = sort valores
pegaNomeCaract (Numeral caract valores) = caract
pegaNomeCaract (Nominal caract valores) = caract

calculaEntropia percentages = sum [-percentagem*(log percentagem) | percentagem <- percentages]

{- calculaIGR exemplos caracteristica = entropiaExemplos - (somatorioIG (pegaClassiCaract exemplos (pegaNomeCaract caracteristica)) valores)/(fromIntegral $ length exemplos)
                                     where entropiaExemplos = calculaEntropia (percentagemClasse (dividirPorClasseExemplos (ordenaClasseEx exemplos)))
                                           valores = calculaFreq (pegaValoresEx exemplos (pegaNomeCaract caracteristica)) (pegaValores caracteristica) -}
somatorioIG _ [] = 0
somatorioIG exemplos (valor:valores) = (snd valor)*(calculaEntropia (percentagemClasse (pegaClassiValor exemplos (fst valor))))/(fromIntegral $ length exemplos) + (somatorioIG exemplos valores)

pegaClassiValor exemplos valor = dividirPorClasseExemplos $ sort [snd exemplo | exemplo <- exemplos, (fst exemplo) == valor]

pegaValoresEx exemplos caract = sort [snd _caract | (T _caracts classi) <- exemplos, _caract <- _caracts, (fst _caract) == caract]

pegaClassiCaract exemplos caracteristica = [(snd _caractEx, classi) | (T caractEx classi) <-exemplos, _caractEx <- caractEx, (fst _caractEx) == caracteristica]

--considerando que ambos estao ordenados e que tem a mesma ordem
calculaFreq _ [] = []
calculaFreq valoresEx (valor:valores)
                      | length valoresEx == 0 || head valoresEx /= valor = (valor, 0):(calculaFreq valoresEx valores)
                      | otherwise = (valor ,(length (takeWhile (== valor) valoresEx))):(calculaFreq (dropWhile (== valor) valoresEx) valores)