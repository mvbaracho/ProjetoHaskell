module FuncAux
    ( add
    , verif
    , filtro
    , filtro2
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
