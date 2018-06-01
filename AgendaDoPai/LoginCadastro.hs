module LoginCadastro
    ( login
    , agendaAluno
    , cadastro
    ) where

import System.IO
import System.Directory
import Data.List
import Data.List.Split
import FuncAux
import Agenda

login = do
    putStrLn "\nTELA DE LOGIN!!"
    putStrLn "Login: "
    log <- getLine
    putStrLn "Senha: "
    senha <- getLine
    let logAluno = (log ++ " " ++ senha)
    handle <- openFile "cadastro.txt" ReadMode
    contents <- hGetContents handle
    let listAlunos = lines contents
    let ctrl1 = verif (filtroLogUser listAlunos) logAluno -- add exception
    let ctrl2 = verif (filtroLogCpf listAlunos) logAluno
    if (ctrl1 || ctrl2)
        then do
            agendaAluno
            else do   --exception
                putStrLn "Login ou senha invalida!"

cadastro = do
    putStrLn "\nTELA DE CADASTRO"
    putStrLn "Nome Completo: " -- exception para nome com numero
    nome <- getLine
    putStrLn "Idade: " -- exception para letra
    idade <- getLine
    putStrLn "Cpf: " -- exception para letra
    cpf <- getLine
    putStrLn "Curso: "
    curso <- getLine
    putStrLn "Nome de Usuario: "
    usuario <- getLine
    putStrLn "Senha: "
    senha <- getLine -- add confirme sua senha e exception no conforme
    if (not ((vrfNum idade) && (validaCpf cpf) && (vrfNome nome)))
        then do
            putStrLn "Nome, Idade ou Cpf em formato invalido, CADASTRO CANCELADO"
            else do
                let ctrAluno = (cpf ++ " " ++ usuario ++ " " ++ senha)
                let ifsAluno = (nome ++ "," ++ cpf ++ "," ++ idade ++ "," ++ curso)
                handle <- openFile "cadastro.txt" ReadMode
                contents <- hGetContents handle
                let listAlunos = lines contents
                let ctrl1 = verif (filtro listAlunos) cpf -- add exception
                let ctrl2 = verif (filtro2 listAlunos) usuario -- add exception
                if (ctrl1 || ctrl2)
                    then do
                        putStrLn "Usuario ja existe"
                        else do
                            add ["cadastro.txt", ctrAluno] -- lembrar de fazer as exception
                            add ["infoAlunos.txt", ifsAluno]
