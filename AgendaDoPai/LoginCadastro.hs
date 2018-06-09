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
    clean
    putStrLn "TELA DE LOGIN"
    putStrLn "\nNome de usuário ou Cpf: "
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
                putStrLn "\nLogin e/ou senha inválida!"

cadastro = do
    clean
    putStrLn "TELA DE CADASTRO"
    putStrLn "\nNome Completo: " -- exception para nome com numero
    nome <- getLine
    if ((not $ vrfNome nome) || null nome)
        then do
            putStrLn "\nNome inválido, retornando para a tela de cadastro!"
            putStrLn "Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            cadastro
        else do
            putStrLn "\nIdade: " -- exception para letra
            idade <- getLine
            if ((not $ vrfNum idade) || null idade)
                then do
                    putStrLn "\nIdade inválida, retornando para a tela de cadastro!"
                    putStrLn "Pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    cadastro
                else do
                    putStrLn "\nCpf: " -- exception para letra
                    cpf <- getLine
                    if ((not $ validaCpf cpf) || null cpf)
                        then do
                            putStrLn "\nCpf inválido, retornando para a tela de cadastro!"
                            putStrLn "Pressione qualquer tecla para continuar!"
                            teclatemporaria <- getLine
                            cadastro
                        else do
                            putStrLn "\nCurso: "
                            curso <- getLine
                            if ((not $ vrfNome curso) || null curso)
                                then do
                                    putStrLn "\nCurso inválido, retornando para a tela de cadastro!"
                                    putStrLn "Pressione qualquer tecla para continuar!"
                                    teclatemporaria <- getLine
                                    cadastro
                                else do
                                    putStrLn "\nInstituição de ensino: "
                                    inst <- getLine
                                    if ((not $ vrfNome inst) || null inst)
                                        then do
                                            putStrLn "Instituição de ensino inválida, retornando para a tela de cadastro!"
                                            putStrLn "Pressione qualquer tecla para continuar!"
                                            teclatemporaria <- getLine
                                            cadastro
                                        else do
                                            putStrLn "\nNome de Usuário: "
                                            usuario <- getLine
                                            putStrLn "\nSenha: "
                                            senha <- getLine -- add confirme sua senha e exception no conforme
                                            if (null usuario || null senha)
                                                then do
                                                    putStrLn "Nome de usuário ou senha não foi preenchida,\nvoltando para a tela de cadastro!"
                                                    putStrLn "Pressione qualquer tecla para continuar!"
                                                    teclatemporaria <- getLine
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
                                                                putStrLn "Nome de usuário e/ou cpf já está cadastrado!"
                                                                putStrLn "Pressione qualquer tecla para continuar!"
                                                                teclatemporaria <- getLine
                                                                cadastro
                                                                else do
                                                                    putStrLn "CADASTRO CONCLUÍDO!"
                                                                    putStrLn "Pressione qualquer tecla para continuar!"
                                                                    teclatemporaria <- getLine
                                                                    add ["cadastro.txt", ctrAluno] -- lembrar de fazer as exception
                                                                    add ["infoAlunos.txt", ifsAluno]
