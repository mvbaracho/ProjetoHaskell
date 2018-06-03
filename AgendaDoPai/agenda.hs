module Agenda
    (agendaAluno
    ) where

import System.IO
import System.Directory
import Data.List
import Data.List.Split
import FuncAux
import {-# SOURCE #-} Main (main)

agendaAluno = do -- fazer
    handle1 <- openFile "secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)
    putStrLn "\nBem vindo a sua Agenda do Estudante!\nO que voce deseja fazer?"
    putStrLn "1. Inserir disciplinas"
    putStrLn "2. Inserir notas"
    putStrLn "3. Inserir atividade no calendario academico"
    putStrLn "4. Ver suas informacoes"
    putStrLn "5. Logout"
    esc <- getLine
    opcAgenda esc

opcAgenda esc| esc == "1" = do
                    insMtr
                    agendaAluno
             | esc == "2" = do
                    --insNota
                    main
             | esc == "3" = do
                    putStrLn "calendario"
                    --insCldr
             | esc == "4" = do
                    putStrLn "informacoes"
                    --verInfos
             | esc == "5" = do
                    main
             | otherwise = do
                    agendaAluno

insMtr = do
    putStrLn "\nCadastro de materias"
    putStrLn "\nInforme a materia:"
    materia <- getLine
    -- add arq
    putStrLn "Adicionar outra materia?\n1. SIM\n2. NAO"
    esc <-getLine
    opcao esc

opcao esc| esc == "1" = insMtr
         | esc == "2" = do
             putStrLn "Cadastro de materias concluido!"
             agendaAluno
         | otherwise = do
             putStrLn "Opcao invalida, voltando para o menu!"
             agendaAluno







--cadastro de periodo (periodo atual, materias com professor, notas com ate 3 entradas)
--adicionar e remover data de prova (e assuntos) ~~ tem parecido no livro
--notas (mostrar materia, professor, notas e media)
--calendario (mostrar data das provas e assuntos)
