module Controlador
    ( control
    ) where

import LoginCadastro
import System.Exit
import {-# SOURCE #-} TelaInicial (main)

control :: String -> IO ()
control input = do
    case input of
        "1" -> login
        "2" -> cadastro
        "3" -> exitWith (ExitFailure 2)
        "4" -> main
        otherwise -> return ()
