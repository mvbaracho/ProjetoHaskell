module Main
    ( main
    ) where

import System.IO
import System.Directory
import Data.List
import Data.List.Split
import FuncAux
import Controlador

main = do
    clean
    putStrLn "Bem vindo à Agenda do Estudante: \n\n 1.Login \n 2.Cadastro \n 3.Sair"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    case esc of
        "1" -> do
            control "1"
            main
        "2" -> do
            control "2"
            main
        "3" -> control "3"
        otherwise -> main
