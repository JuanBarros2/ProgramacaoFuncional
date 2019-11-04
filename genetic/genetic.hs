import System.Random
import Data.List.Split

data Individuo valor = Individuo valor deriving (Eq, Show)
threshold = 10
inicializaPopulacao 0 = []
inicializaPopulacao tam = [(Individuo value)| value <- take tam $ randomRs (0,255) (mkStdGen 3) :: [Int] ]

intToIndividuo [] = []
intToIndividuo (x:xs)= [(Individuo x)] ++ (intToIndividuo xs)

geraScore funcao esperado encontrado = map funcao (zip encontrado esperado)

randomNum seed = randomR (0, 255) (mkStdGen seed) 

geraMutacao scores threshold seed = map (\x -> if ((abs x) <= threshold) then x else fst(randomNum x))  scores
criaExemploListaNumerica tam = [1..tam]

funcaoScore (Individuo a, Individuo b) = a - b

algRun esperado = geraScore funcaoScore (intToIndividuo esperado) (inicializaPopulacao (length esperado))
main = do
    contents <- readFile "photo.txt"  
    print (algRun (map (\x -> read x::Int) (splitOn "," contents)))