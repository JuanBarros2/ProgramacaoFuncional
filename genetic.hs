import System.Random

data Individuo valor = Individuo valor deriving (Eq, Show)

inicializaPopulacao 0 = []
inicializaPopulacao tam = [(Individuo value)| value <- take tam $ randomRs (0,tam) (mkStdGen 3) :: [Int] ]

geraScore funcao lista = map funcao lista

criaExemploListaNumerica tam = [1..tam]