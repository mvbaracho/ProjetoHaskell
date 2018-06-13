module Controlador
    ( control
    ) where

import LoginCadastro
import System.Exit

control :: String -> IO ()
control input = do
    case input of
        "1" -> login
        "2" -> cadastro
        "3" -> exitWith (ExitFailure 2)
        otherwise -> return ()
