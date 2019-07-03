module Utils(
formataCaracteristicas,
formataExemplos,
listaCaracteristicas,
discretizaNumeral,
maioria,
criaArvoreDecisao,
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
pegaRaiz (No caract _) = caract

criaArvore :: Caracteristica -> [Arvore] -> Arvore
criaArvore raiz [] = No raiz [Nil]
criaArvore raiz arv = No raiz (arv)

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
                    | otherwise = (Nominal (head xs) (sort $ tail xs)):(formataCaracteristicas xss)

criaArvoreDecisao :: Exemplos -> Caracteristicas -> Caracteristica -> Arvore
criaArvoreDecisao [] _ maisComum = criaRaiz maisComum
criaArvoreDecisao exemplos [] _ = criaRaiz (maioria exemplos)
criaArvoreDecisao exemplos caracteristicas maisComum
                        | temMesmaClassificacao exemplos = criaRaiz (pegaClassiEx (head exemplos))
                        | otherwise = criaArvore melhor (criaSubArvores melhor exemplos caracteristicas maisComum)
                                      where melhor = melhorTeste exemplos caracteristicas (-999) (head caracteristicas)

criaSubArvores :: Caracteristica -> Exemplos -> Caracteristicas -> Caracteristica -> [Arvore]
criaSubArvores (Numeral caract []) _ _ _ = []
criaSubArvores (Numeral caract (vi:vis)) exemplos caracts maisComum = (criaArvoreDecisao exemplosVi (removeCaractDaLista caracts caract) maisComum):(criaSubArvores (Numeral caract vis) exemplosSemVi caracts maisComum)
                                                                      where exemplosVi = [exemplo | exemplo<-exemplos, (_pegaClassiCaract exemplo caract) <= vi]
                                                                            exemplosSemVi = [exemplo | exemplo<-exemplos, (_pegaClassiCaract exemplo caract) > vi]
criaSubArvores (Nominal caract []) _ _ _= []
criaSubArvores (Nominal caract (vi:vis)) exemplos caracts maisComum = (criaArvoreDecisao exemplosVi (removeCaractDaLista caracts caract) maisComum):(criaSubArvores (Nominal caract vis) exemplosSemVi caracts maisComum)
                                                                      where exemplosVi = [exemplo | exemplo<-exemplos, (_pegaClassiCaract exemplo caract) == vi]
                                                                            exemplosSemVi = [exemplo | exemplo<-exemplos, (_pegaClassiCaract exemplo caract) /= vi]

readFloat :: String -> Double
readFloat = read

removeCaractDaLista :: Caracteristicas -> String -> Caracteristicas
removeCaractDaLista caracts caract = [_caract | _caract <- caracts, (pegaNomeCaract _caract) /= caract]

temMesmaClassificacao :: Exemplos -> Bool
temMesmaClassificacao ((T caracteristicas classificacao):exemplos) = and [classificacao == _classificacao | (T _caracteristicas _classificacao) <- exemplos]

--agrupa classificacao por tipo
ordenaClasseEx :: Exemplos -> [String]
ordenaClasseEx exemplos = sort [classi | (T caract classi)<-exemplos]

--dividi grupos de classificacoes em listas
dividirPorClasseExemplos [] = []
dividirPorClasseExemplos exemplos = (takeWhile (==(head exemplos)) exemplos) ++  dropWhile (==(head exemplos)) exemplos

percentagemClasse classes = [(fromIntegral $ length mesmaClasse)/n | mesmaClasse <- classes]
                            where n = fromIntegral $ sum $ map length classes

maioria :: Exemplos -> Caracteristica
maioria exemplos = classeMajoritaria (ordenaClasseEx exemplos) []
                   where classeMajoritaria [] majoritaria = (Nominal (head majoritaria) [])
                         classeMajoritaria classes majoritaria
                           | length classeAtual > length majoritaria = classeMajoritaria restante classeAtual
                           | otherwise = classeMajoritaria restante majoritaria
                           where classeAtual = takeWhile (==(head classes)) classes
                                 restante = dropWhile (==(head classes)) classes

pegaClassiEx (T caract classificacao) = (Nominal classificacao [])
pegaNomeCaract (Numeral caract valores) = caract
pegaNomeCaract (Nominal caract valores) = caract

calculaEntropia :: [Double] -> Double
calculaEntropia [] = 0
calculaEntropia (percentagem:percentages)
                        | percentagem == 0 = calculaEntropia percentages
                        | otherwise = -percentagem*(logBase 2 percentagem) + calculaEntropia percentages

melhorTeste :: Exemplos -> Caracteristicas -> Double -> Caracteristica -> Caracteristica
melhorTeste _ [] _ caractMaiorIGR = caractMaiorIGR
melhorTeste exemplos (caracteristica:caracteristicas) maiorIGR caractMaiorIGR
                                                      | maiorIGR < atualIGR = melhorTeste exemplos caracteristicas atualIGR caracteristica
                                                      | otherwise = melhorTeste exemplos caracteristicas maiorIGR caractMaiorIGR
                                                      where atualIGR
                                                                  | somaIV == 0 = 0
                                                                  | otherwise = (entropiaExemplos - somaIG)/somaIV
                                                            entropiaExemplos = calculaEntropia (percentagemClasse (dividirPorClasseExemplos (ordenaClasseEx exemplos)))
                                                            somaIG = ((somatorioIG (pegaClassiCaract exemplos (pegaNomeCaract caracteristica)) valorFreq)/(fromIntegral $ length exemplos))
                                                            valorFreq = calculaFreq (pegaValoresEx exemplos (pegaNomeCaract caracteristica)) caracteristica
                                                            somaIV = -((somatorioIV (pegaClassiCaract exemplos (pegaNomeCaract caracteristica)) valorFreq)/(fromIntegral $ length exemplos))

somatorioIV :: [(String, String)] -> [(String, Int)] -> Double
somatorioIV _ [] = 0
somatorioIV exemplos (valorFreq:valores) 
                    | snd valorFreq == 0 = somatorioIV exemplos valores
                    | otherwise = (fromIntegral $ snd valorFreq)*(logBase 2 ((fromIntegral $ snd valorFreq)/(fromIntegral $ length exemplos))) + (somatorioIV exemplos valores)

--entrada: lista de tupla contendo (valor, classe) e uma lista de tupla contendo (valor, frequencia)
--saida: somatorio IG de todos valores da dada caracteristica
somatorioIG :: [(String, String)] -> [(String, Int)] -> Double
somatorioIG _ [] = 0
somatorioIG exemplos (valorFreq:valores) = (fromIntegral $ snd valorFreq)*(calculaEntropia (percentagemClasse (pegaClassiValor exemplos (fst valorFreq)))) + (somatorioIG exemplos valores)

--entrada: uma lista de tupla de (valor, classe) da base e um valor
--saida: lista de todas as classificacoes para o valor na base ordenado e semparado em sublistas
pegaClassiValor :: [(String, String)] -> String -> [String]
pegaClassiValor exemplos valor = dividirPorClasseExemplos $ sort [snd exemplo | exemplo <- exemplos, (fst exemplo) == valor]

--entrada: lista de exemplos e nome de uma caracteristica
--saida: valores ordenados dessa caracteristica na base de exemplos
pegaValoresEx :: Exemplos -> String -> [String]
pegaValoresEx exemplos caract = sort [snd _caract | (T _caracts classi) <- exemplos, _caract <- _caracts, (fst _caract) == caract]

--entrada: lista de exemplos e uma dada caracteristica
--saida: lista de tupla contendo o valor da caracteristica e a classificacao que o exemplo recebeu
pegaClassiCaract :: Exemplos -> String -> [(String, String)]
pegaClassiCaract exemplos caracteristica = [(snd _caractEx, classi) | (T caractEx classi) <-exemplos, _caractEx <- caractEx, (fst _caractEx) == caracteristica]
_pegaClassiCaract (T caractEx classi) caract = head [snd _caractEx | _caractEx <- caractEx, (fst _caractEx) == caract]

--entrada: valores ordenados da base em relacao a uma dada caracteristica e caracteristica
--saida: lista de tupla contendo o valor da caracteristica e a frequencia que aparece na base de exemplos
calculaFreq :: [String] -> Caracteristica -> [(String, Int)]
calculaFreq _ (Nominal caract []) = []
calculaFreq valoresEx (Nominal caract (valor:valores))
                      | length valoresEx == 0 || head valoresEx /= valor = (valor, 0):(calculaFreq valoresEx (Nominal caract valores))
                      | otherwise = (valor ,(length (takeWhile (== valor) valoresEx))):(calculaFreq (dropWhile (== valor) valoresEx) (Nominal caract valores))
calculaFreq _ (Numeral caract []) = []
calculaFreq valoresEx (Numeral caract (valor:valores))
                      | length valoresEx == 0 || head valoresEx > valor = (valor, 0):(calculaFreq valoresEx (Numeral caract valores))
                      | otherwise = (valor ,(length (takeWhile (<= valor) valoresEx))):(calculaFreq (dropWhile (<= valor) valoresEx) (Numeral caract valores))
          
discretizaNumeral :: Caracteristicas -> Exemplos -> Caracteristicas
discretizaNumeral [] _ = []
discretizaNumeral ((Nominal caract valores):caracts) exemplos = (Nominal caract valores):(discretizaNumeral caracts exemplos)
discretizaNumeral (((Numeral caract [])):caracts) exemplos = (Numeral caract (discretizaValor++["9999"])):(discretizaNumeral caracts exemplos)
                                                             where discretizaValor = calculaMedias (sort (pegaClassiCaract exemplos caract))
                                                                   calculaMedias [ex] = []
                                                                   calculaMedias (ex:exs)
                                                                                   | snd ex == snd (head exs) = calculaMedias exs
                                                                                   | otherwise = (show (((readFloat (fst ex))+(readFloat (fst (head exs))))/2.0)):(calculaMedias exs)