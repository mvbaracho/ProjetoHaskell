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
    putStrLn "TELA DE LOGIN"
    putStrLn "\nNome de usuario ou Cpf: "
    log <- getLine
    putStrLn "\nSenha: "
    senha <- getLine
    let logAluno = (log ++ " " ++ senha)
    handle <- openFile "cadastro.txt" ReadMode
    contents <- hGetContents handle
    let listAlunos = lines contents
    let ctrl1 = verif (filtroLogUser listAlunos) logAluno -- add exception
    let ctrl2 = verif (filtroLogCpf listAlunos) logAluno
    if (ctrl1 || ctrl2)
        then do
            writeFile "secaoAtual.txt" log
            agendaAluno
            else do   --exception
                putStrLn "\nLogin ou senha invalida!"

cadastro = do
    putStrLn "TELA DE CADASTRO"
    putStrLn "\nNome Completo: " -- exception para nome com numero
    nome <- getLine
    if ((not $ vrfNome nome) || null nome)
        then do
            putStrLn "\nNome invalido, retornando para a tela de cadastro!"
            cadastro
        else do
            putStrLn "\nIdade: " -- exception para letra
            idade <- getLine
            if ((not $ vrfNum idade) || null idade)
                then do
                    putStrLn "\nIdade invalida, retornando para a tela de cadastro!"
                    cadastro
                else do
                    putStrLn "\nCpf: " -- exception para letra
                    cpf <- getLine
                    if ((not $ validaCpf cpf) || null cpf)
                        then do
                            putStrLn "\nCpf invalido, retornando para a tela de cadastro!"
                            cadastro
                        else do
                            putStrLn "\nCurso: "
                            curso <- getLine
                            if ((not $ vrfNome curso) || null curso)
                                then do
                                    putStrLn "\nCurso invalido, retornando para a tela de cadastro!"
                                    cadastro
                                else do
                                    putStrLn "\nInstituicao de ensino: "
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
                                                                    putStrLn "CADASTRO CONCLUIDO!"
