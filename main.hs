import Io
import Utils

main = do descricao <- lerDescricao
          base <- lerBase
          let exemplos = formataExemplos base
          putStrLn (show $  temMesmaClassificacao exemplos)
          {- let arvoreDecisao = criaArvoreDecisao (formataExemplos base) (formataCaracteriscas descricao)
          imprimeArvore (formataArvore arvoreDecisao)
          caso <- lerCaso
          let resultado = computaResultado arvoreDecisao caso
          imprimeResult resultado -}
