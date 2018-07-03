module TelaInicial
    ( main
    ) where

import System.IO
import System.Directory
import System.FilePath
import Controlador
import FuncAux
import {-# SOURCE #-} MainAluno (mainAluno)
import Data.List.Split

main = do
    clean
    putStrLn "Bem vindo, para continuar, selecione uma opção!"
    putStrLn "1. Aluno"
    putStrLn "2. Administrador"
    putStrLn "3. Sair"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    case esc of
        "1" -> mainAluno
        "2" -> admin
        "3" -> control "3"
        otherwise -> main

admin = do
    clean
    putStrLn "TELA DE LOGIN"
    putStrLn "\nLogin: "
    log <- getLine
    putStrLn "\nSenha: "
    senha <- getLine
    let logAdmin = (log ++ " " ++ senha)
    let logPass = ("Admin Admin")
    if (logAdmin == logPass)
        then do
            menuAdmin
        else do   --exception
            putStrLn "\nLogin e/ou senha inválida!"
            putStrLn "Pressione ENTER para continuar!"
            teclatemporaria <- getLine
            main

menuAdmin = do
    clean

    putStrLn "Selecione uma opção:"
    putStrLn "1. Pesquisar por um aluno"
    putStrLn "2. Listar alunos por matéria"
    putStrLn "3. Listar alunos por instituição de ensino"
    putStrLn "4. Listar todos alunos"
    putStrLn "5. Logout"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    case esc of
        "1" -> do
            clean
            putStrLn "Digite o nome do aluno:"
            nome <- getLine
            handle <- openFile "Dados\\infoAlunos.txt" ReadMode
            contents <- hGetContents handle
            let listAlnComp = lines contents
            let resposta = getAlunoInfos listAlnComp nome
            clean
            putStrLn resposta

            putStrLn "\nPressione qualquer tecla para continuar!"
            temp <-getLine
            menuAdmin
        "2" -> do
            clean
            handle <- openFile "Dados\\infoMaterias.txt" ReadMode
            contents <- hGetContents handle
            let lista = lines contents
            let matLista = listaMaterias $ filtro4 $ lines contents
            let matNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ matLista

            putStrLn "Lista de matérias"
            putStr $ unlines matNumeradas
            putStrLn "\nDigite sua opção:"
            numberString <- getLine
            if ((not $ vrfNum numberString) || (read numberString) < 0 || (read numberString) > (length matLista) - 1 || null numberString)
                then do
                    putStrLn "Opção inválida, retornando para o menu!\nPressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    menuAdmin
                else do
                    clean
                    let number = read numberString
                        matNumber = (matLista !! number)
                    let listaCpfs = getCpfsMatCmns lista matNumber
                    hClose handle

                    handle1 <- openFile "Dados\\infoAlunos.txt" ReadMode
                    contents1 <- hGetContents handle1
                    let listaInfAlunos = lines contents1

                    putStrLn "Lista de alunos cadastrados nessa matéria:\n"
                    putStr $ unlines $ listaInfosMat listaInfAlunos listaCpfs

                    putStrLn "Pressione ENTER para continuar!"
                    temp <-getLine
                    menuAdmin
        "3" -> do
            clean
            handle <- openFile "Dados\\infoAlunos.txt" ReadMode
            contents <- hGetContents handle
            let alunoInf = lines contents
            let instuicao = ordena $ removeRep [(equalsIgnUpper x) | x <- (listaInst alunoInf)]
            let instNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ instuicao
            putStrLn "Lista de instituições"
            putStr $ unlines instNumeradas
            putStrLn "\nDigite sua opção:"
            numberString <- getLine
            if ((not $ vrfNum numberString) || (read numberString) < 0 || (read numberString) > (length instuicao) - 1  || null numberString)
                then do
                    putStrLn "Opção inválida, retornando para o menu!\nPressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    menuAdmin
                else do
                    clean
                    let number = read numberString
                        instNumber = (instuicao !! number)
                    let listaAlunos = listaInfosInst alunoInf instNumber
                    putStrLn "Lista de alunos cadastrados nessa instituição de ensino:\n"
                    putStr $ unlines $ listaAlunos
                    hClose handle
                    putStrLn "Pressione ENTER para continuar!"
                    temp <-getLine
                    menuAdmin
        "4" -> do
            clean
            handle <- openFile "Dados\\infoAlunos.txt" ReadMode
            contents <- hGetContents handle
            let alunoInf = lines contents
            let listaCompleta = ordena $ listaAlunosCompl alunoInf
            let numeroAlunos ="Atualmente estão cadastrados " ++ (show $ length listaCompleta) ++ " alunos."
            putStrLn numeroAlunos
            putStrLn "\nLista completa de alunos cadastrados:\n"
            putStr $ unlines listaCompleta
            hClose handle
            putStrLn "Pressione ENTER para continuar!"
            temp <-getLine
            menuAdmin
        "5" -> do
            putStrLn "\nLogout concluído, retornando para a tela inicial!"
            putStrLn "Pressione ENTER para continuar!"
            teclatemporaria <- getLine
            main
        otherwise -> menuAdmin

getAlunoInfos :: [String] -> String -> String
getAlunoInfos [] _ = "Mensagem do sistema:\n\nAluno não cadastrado, verifique se o nome foi digitado corretamente!"
getAlunoInfos (a:as) input | equalsIgn nome input = "Informações do aluno:\n\nNome: " ++ nome ++ "\nCpf: " ++ cpf ++ "\nIdade: " ++ idade ++ "\nCurso: " ++ curso ++ "\nInstituição de ensino: " ++ instt
                           | otherwise = getAlunoInfos as input
                        where
                            nome = head $ splitOn "," (a)
                            cpf = head $ drop 1 (splitOn "," (a))
                            idade = head $ drop 2 (splitOn "," (a))
                            curso = head $ drop 3 (splitOn "," (a))
                            instt = head $ drop 4 (splitOn "," a)

listaAlunosCompl :: [String] -> [String]
listaAlunosCompl [] = []
listaAlunosCompl (a:as) = [("> Nome: " ++ nome ++ " Cpf: " ++ cpf ++ "\n")] ++ (listaAlunosCompl as)
                        where
                            nome = head $ splitOn "," (a)
                            cpf = head $ drop 1 (splitOn "," (a))

listaInfosInst :: [String] -> String -> [String]
listaInfosInst [] _ = []
listaInfosInst (a:as) input | equalsIgn instt input = [("> Nome: " ++ nome ++ " Cpf: " ++ cpf ++ "\n")] ++ (listaInfosInst as input)
                            | otherwise = listaInfosInst as input
                        where
                            nome = head $ splitOn "," (a)
                            cpf = head $ drop 1 (splitOn "," (a))
                            instt = head $ drop 4 (splitOn "," a)

listaInfosMat :: [String] -> [String] -> [String]
listaInfosMat [] _ = []
listaInfosMat (a:as) input | cpf `elem` input = [("> Nome: " ++ nome ++ " Cpf: " ++ cpf ++ "\n")] ++ (listaInfosMat as input)
                        | otherwise = listaInfosMat as input
                        where
                            nome = head $ splitOn "," (a)
                            cpf = head $ drop 1 (splitOn "," (a))

getCpfsMatCmns :: [String] -> String -> [String]
getCpfsMatCmns [] _ = []
getCpfsMatCmns (a:as) input | equalsIgn mat input = [cpf] ++ (getCpfsMatCmns as input)
                            | otherwise = getCpfsMatCmns as input
                            where
                                mat = head $ drop 1 (splitOn "," (a))
                                cpf = head $ splitOn "," (a)

listaInst :: [String] -> [String]
listaInst [] = []
listaInst (a:as) = (drop 4 (splitOn "," a)) ++ (listaInst as)

listaMaterias :: [String] -> [String]
listaMaterias input = ordena $ removeRep [(equalsIgnUpper x) | x <- input]
