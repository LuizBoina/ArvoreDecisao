import Io
import Utils

main = do descricao <- lerDescricao
          let _descricao = formataCaracteristicas descricao --init descricao = caracteristicas/ last descricao = classe
          base <- lerBase
          let exemplos = formataExemplos base (listaCaracteristicas (init _descricao))
          let arvore =  criaArvoreDecisao exemplos (init _descricao) (maioria exemplos)
        --   imprimeArvore (formataArvore arvore)
          caso <- lerCaso
          let resultado = computaResultado arvore (formataCaso caso (listaCaracteristicas (init _descricao)))
          imprimeResultado resultado
        -- No (Numeral "Umidade" ["78.5","9999"]) [No (Nominal "Va" []) [Nil],No (Nominal "NaoVa" []) [Nil]]