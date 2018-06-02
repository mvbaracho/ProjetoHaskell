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
    putStrLn "\nTELA DE LOGIN"
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
    if ((not $ vrfNome nome) || null nome)
        then do
            putStrLn "Nome invalido, retornando para a tela de cadastro!"
            cadastro
        else do
            putStrLn "Idade: " -- exception para letra
            idade <- getLine
            if ((not $ vrfNum idade) || null idade)
                then do
                    putStrLn "Idade invalida, retornando para a tela de cadastro!"
                    cadastro
                else do
                    putStrLn "Cpf: " -- exception para letra
                    cpf <- getLine
                    if ((not $ validaCpf cpf) || null cpf)
                        then do
                            putStrLn "Cpf invalido, retornando para a tela de cadastro!"
                            cadastro
                        else do
                            putStrLn "Curso: "
                            curso <- getLine
                            if ((not $ vrfNome curso) || null curso)
                                then do
                                    putStrLn "Curso invalido, retornando para a tela de cadastro!"
                                    cadastro
                                else do
                                    putStrLn "Instituicao de ensino: "
                                    inst <- getLine
                                    if ((not $ vrfNome inst) || null inst)
                                        then do
                                            putStrLn "Instituicao de ensino invalida, retornando para a tela de cadastro!"
                                            cadastro
                                        else do
                                            putStrLn "Nome de Usuario: "
                                            usuario <- getLine
                                            putStrLn "Senha: "
                                            senha <- getLine -- add confirme sua senha e exception no conforme
                                            if (null usuario || null senha)
                                                then do
                                                    putStrLn "Nome de usuario ou senha nao foi preenchida,\nvoltando para a tela de cadastro!"
                                                    cadastro
                                                    else do
                                                        let ctrAluno = (cpf ++ " " ++ usuario ++ " " ++ senha)
                                                        let ifsAluno = (nome ++ "," ++ cpf ++ "," ++ idade ++ "," ++ curso ++ "," ++ inst)
                                                        handle <- openFile "cadastro.txt" ReadMode
                                                        contents <- hGetContents handle
                                                        let listAlunos = lines contents
                                                        let ctrl1 = verif (filtro listAlunos) cpf -- add exception
                                                        let ctrl2 = verif (filtro2 listAlunos) usuario -- add exception
                                                        if (ctrl1 || ctrl2)
                                                            then do
                                                                putStrLn "Nome de usuario ou cpf ja esta cadastrado!"
                                                                else do
                                                                    add ["cadastro.txt", ctrAluno] -- lembrar de fazer as exception
                                                                    add ["infoAlunos.txt", ifsAluno]
