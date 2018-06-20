module MainAluno
    ( mainAluno
    ) where

import System.IO
import System.Directory
import FuncAux
import Controlador

mainAluno = do
    clean
    putStrLn "Bem vindo à Agenda do Estudante: \n\n 1.Login \n 2.Cadastro \n 3.Voltar"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    case esc of
        "1" -> do
            control "1"
            mainAluno
        "2" -> do
            control "2"
            mainAluno
        "3" -> control "4"
        otherwise -> mainAluno
