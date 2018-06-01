module FuncAux
    ( add
    , verif
    , filtro
    , filtro2
    , filtroLogUser
    , filtroLogCpf
    ) where

import Data.List.Split

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
