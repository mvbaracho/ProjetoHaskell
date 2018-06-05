module Agenda
    (agendaAluno
    ) where

import System.IO
import System.Directory
import Data.List
import Data.List.Split
import FuncAux
import {-# SOURCE #-} Main (main)
import System.IO
import System.IO.Error
import System.Process


clean = system "cls"

agendaAluno = do -- fazer
    putStrLn "\nBem vindo a sua Agenda do Estudante!\nO que voce deseja fazer?"
    putStrLn "1. Inserir disciplina"
    putStrLn "2. Remover disciplina"
    putStrLn "3. Gerenciar notas"
    putStrLn "4. Inserir ou remover atividade no calendario academico"
    putStrLn "5. Ver suas informacoes"
    putStrLn "6. Logout"
    esc <- getLine
    opcAgenda esc

opcAgenda esc| esc == "1" = do
                    clean
                    putStrLn "\nInserir materias"
                    insMtr
                    agendaAluno
             | esc == "2" = do
                    clean
                    putStrLn "\nRemover de materias"
                    rmvMat
                    main
             | esc == "3" = do
                    putStrLn "calendario"
                    --insCldr
             | esc == "4" = do
                    putStrLn "informacoes"
                    --verInfos
             | esc == "5" = do
                    main
             | esc == "6" = do
                    main
             | otherwise = do
                    agendaAluno

insMtr = do
    handle1 <- openFile "secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)
    putStrLn "\nInforme a materia:"
    materia <- getLine
    if null materia
        then do
            putStrLn "Voce precisa digitar o nome da materia!"
            insMtr
        else do
            add ["infoMaterias.txt", (cpfAtual ++ "," ++ materia)]
            putStrLn "Adicionar outra materia?\n1. SIM\n2. NAO"
            esc <-getLine
            opcaoMat esc

opcaoMat esc| esc == "1" = insMtr
         | esc == "2" = do
             putStrLn "Cadastro de materia concluido!\nRetornando ao menu principal!"
             agendaAluno
         | otherwise = do
             putStrLn "Opcao invalida, retornando para o menu!"
             agendaAluno

rmvMat = do
    handle1 <- openFile "secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)

    handle <- openFile "infoMaterias.txt" ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let matLista = lines contents
    let matListaUser = [elemLista |elemLista <- matLista, filtro3 elemLista cpfAtual]
        matNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] matListaUser
    putStrLn "Essas são as suas matérias:"
    putStr $ unlines matNumeradas
    putStrLn "Qual delas você deseja remover?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (matLista !! number) matLista
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "infoMaterias.txt"
    renameFile tempName "infoMaterias.txt"








--cadastro de periodo (periodo atual, materias com professor, notas com ate 3 entradas)
--adicionar e remover data de prova (e assuntos) ~~ tem parecido no livro
--notas (mostrar materia, professor, notas e media)
--calendario (mostrar data das provas e assuntos)
