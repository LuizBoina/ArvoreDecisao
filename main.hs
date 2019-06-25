import Io
import Utils

main = do _descricao <- lerDescricao
          let descricao = formataCaracteristicas _descricao --init descricao = caracteristicas/ last descricao = classe
          base <- lerBase
          let exemplos = formataExemplos base (listaCaracteristicas (init descricao))
          putStrLn (show exemplos)
          {- let arvoreDecisao = criaArvoreDecisao (formataExemplos base) (formataCaracteriscas descricao)
          imprimeArvore (formataArvore arvoreDecisao)
          caso <- lerCaso
          let resultado = computaResultado arvoreDecisao caso
          imprimeResult resultado -}
