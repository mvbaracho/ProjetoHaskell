module FuncAux
    ( add
    , verif
    , filtro
    , filtro2
    , filtro3
    , filtroLogUser
    , filtroLogCpf
    , vrfNum
    , validaCpf
    , vrfNome
    , cpfSecaoAtual
    , equalsIgn
    ) where

import Data.List.Split
import Data.Char

-- Responsavel por escrever no arquivo de acordo com a entrada
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

-- Verifica se uma String é elemento da lista de cadastro
verif :: [String] -> String -> Bool
verif listCtr logUser = logUser `elem` listCtr

-- Retorna apenas o login dos usuarios da lista de cadastro
filtro :: [String] -> [String]
filtro [] = []
filtro (a:as) = (head (splitOn " " (a))):filtro as

filtro2 :: [String] -> [String]
filtro2 [] = []
filtro2 (a:as) = (head (drop 1 (splitOn " " (a)))):filtro2 as

filtro3 :: String -> String -> Bool
filtro3 [] b = False
filtro3 a b | (head(splitOn "," (a))) == b = True
            | otherwise = False

filtroLogUser :: [String] -> [String]
filtroLogUser [] = []
filtroLogUser (a:as) = (drop (find ' ' a) a):(filtroLogUser as)

filtroLogCpf :: [String] -> [String]
filtroLogCpf [] = []
filtroLogCpf (a:as) = (cpf ++ " " ++ senha):(filtroLogCpf as)
                    where
                        cpf = head $ splitOn " " (a)
                        senha = head $ drop 2 (splitOn " " (a))

find :: Eq t => t -> [t] -> Int
find a [] = 0
find a (b:bs)
           |a == b = 1 + (find a [])
           |otherwise = 1 + find a bs

vrfNum::String -> Bool
vrfNum input | False `elem` (map (isDigit) input) = False
             | otherwise = True

validaCpf :: String -> Bool
validaCpf cpf | (length cpf == 11) && (vrfNum cpf) = True
              | otherwise = False

vrfNome :: String -> Bool
vrfNome [a] = isAlpha a
vrfNome (a:as) |a == ' ' = vrfNome as
               |isAlpha a = vrfNome as
               |otherwise = False

cpfSecaoAtual :: [String] -> String -> String
cpfSecaoAtual [] _ = error "PROBLEMA EM cpfSecaoAtual ENTRADA VAZIA"
cpfSecaoAtual (a:as) logInput | logInput == cpf = cpf
                              | user == logInput = cpf
                              | otherwise = cpfSecaoAtual as logInput
                            where
                                cpf = head $ splitOn " " (a)
                                user = head $ drop 1 (splitOn " " (a))

equalsIgn :: [Char] -> [Char] -> Bool
equalsIgn a b = equalsIgnAux (equalsIgnUpper a) (equalsIgnUpper b)

equalsIgnUpper :: [Char] -> [Char]
equalsIgnUpper [] = []
equalsIgnUpper (a:as)
                    | (ord a >= 97 && ord a <= 122) = chr ((ord a)-32):equalsIgnUpper as
                    | otherwise = a:equalsIgnUpper as

equalsIgnAux :: [Char] -> [Char] -> Bool
equalsIgnAux [] [] = True
equalsIgnAux a b | (length a /= length b) = False
equalsIgnAux (a:as) (b:bs)
                         | (a == b) = equalsIgnAux as bs
                         | otherwise = False
