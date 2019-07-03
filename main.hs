import Io
import Utils

main = do descricao <- lerDescricao
          let _descricao = formataCaracteristicas descricao --init descricao = caracteristicas/ last descricao = classe
          base <- lerBase
          let exemplos = formataExemplos base (listaCaracteristicas (init _descricao))
          let __descricao = discretizaNumeral (init _descricao) exemplos
          putStrLn $ show $ criaArvoreDecisao exemplos __descricao (maioria exemplos)   -- let arvoreDecisao = criaArvoreDecisao exemplos __descricao (maioria exemplos)
          {-imprimeArvore (formataArvore arvoreDecisao)
          caso <- lerCaso
          let resultado = computaResultado arvoreDecisao caso
          imprimeResult resultado -}
        --   melhorTeste [T [("Aparencia","Sol"),("Temperatura","25"),("Umidade","72"),("Vento","Sim")] "Va",T [("Aparencia","Sol"),("Temperatura","28"),("Umidade","91"),("Vento","Sim")] "NaoVa",T [("Aparencia","Sol"),("Temperatura","22"),("Umidade","70"),("Vento","Nao")] "Va",T [("Aparencia","Sol"),("Temperatura","23"),("Umidade","95"),("Vento","Nao")] "NaoVa",T [("Aparencia","Sol"),("Temperatura","30"),("Umidade","85"),("Vento","Nao")] "NaoVa"] [Nominal "Aparencia" ["Sol","Chuva","Nublado"],Numeral "Temperatura" ["22.5","24.0","26.5"],Numeral "Umidade" ["78.5"],Nominal "Vento" ["Sim","Nao"]] (-9999) (Nominal "Vento" ["Sim","Nao"])