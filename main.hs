import Io
import Utils

main = do descricao <- lerDescricao
          let _descricao = formataCaracteristicas descricao --init descricao = caracteristicas/ last descricao = classe
          base <- lerBase
          let exemplos = formataExemplos base (listaCaracteristicas (init _descricao))
          let __descricao = discretizaNumeral (init _descricao) exemplos
          let arvore =  criaArvoreDecisao exemplos __descricao (maioria exemplos)
        --   imprimeArvore (formataArvore arvore)
          hCasos <- lerCasos
          let casos = (formataCasos hCasos (listaCaracteristicas (init _descricao)))
          imprimeResultados (resultados casos arvore) []
          where resultados [] _= []
                resultados (x:xs) arv = (computaResultado arv x):(resultados xs arv)
          -- No (Numeral "Umidade" ["78.5","9999"]) [No (Nominal "Va" []) [Nil],No (Nominal "NaoVa" []) [Nil]]