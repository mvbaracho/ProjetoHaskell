module Main
    ( main
    ) where

import System.IO
import System.Directory
import Data.List
import Data.List.Split
import FuncAux
import LoginCadastro

main = do
    clean
    putStrLn "Bem vindo à Agenda do Estudante: \n\n 1.Login \n 2.Cadastro \n 3.Sair"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    menuCtrl esc

menuCtrl esc| esc == "1" = do
                login
                main
            | esc == "2" = do
                cadastro
                clean
                main
            |esc == "3" = do
                return ()
            | otherwise = do
                main
