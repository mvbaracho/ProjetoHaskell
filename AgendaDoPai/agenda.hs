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


agendaAluno = do -- fazer
    clean
    putStrLn "Bem vindo a sua Agenda do Estudante!\n\n O que voce deseja fazer?\n"
    putStrLn "1. Inserir disciplina"
    putStrLn "2. Remover disciplina"
    putStrLn "3. Gerenciar notas"
    putStrLn "4. Gerenciar atividades no calendário acadêmico"
    putStrLn "5. Ver suas informações"
    putStrLn "6. Logout"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    case esc of
        "1" -> do
            insMtr
            agendaAluno
        "2" -> do
            rmvMat
            agendaAluno
        "3" -> do
            grcNota
            agendaAluno
        "4" -> do
            clean
            putStrLn "falta implementar"
            temp <- getLine
        "5" -> do
            verInf
            agendaAluno
        "6" -> main
        otherwise -> agendaAluno

insMtr = do
    clean
    handle1 <- openFile "secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)

    putStrLn "CADASTRO DE MATÉRIA"
    putStrLn "Informe a matéria:"
    materia <- getLine
    if null materia
        then do
            putStrLn "Você precisa digitar o nome da matéria!"
            putStrLn "Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            insMtr
        else do
            let inputMateria = cpfAtual ++ "," ++ materia
            handle <- openFile "infoMaterias.txt" ReadMode
            contents <- hGetContents handle
            let materias = lines contents
            if (inputMateria `elem'` materias)
                then do
                    putStrLn "Você já adicionou essa matéria!"
                    putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
                    teclatemporaria <- getLine
                    hClose handle
                    agendaAluno
                else do
                    let inputNotas = cpfAtual ++ "," ++ materia ++ "," ++ "[n1]" ++ "," ++ "[n2]" ++ "," ++ "[nf]"
                    add ["infoMaterias.txt", inputMateria]
                    add ["infoNotas.txt", inputNotas]
                    putStrLn "\nAdicionar outra matéria?\n1. SIM\n2. NÃO"
                    putStrLn "\nDigite sua opção:"
                    esc <-getLine
                    case esc of
                        "1" -> insMtr
                        "2" -> do
                            putStrLn "\nCadastro de matérias concluído!"
                            putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
                            teclatemporaria <- getLine
                            agendaAluno
                        otherwise -> do
                            putStrLn "\nOpção inválida!"
                            putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
                            teclatemporaria <- getLine
                            agendaAluno

rmvMat = do
    clean
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
    let matListaOthers = [elemListaO |elemListaO <- matLista, not $ filtro3 elemListaO cpfAtual]
        matNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ filtro4 matListaUser
    if (length matListaUser == 0)
        then do
            putStrLn "Você não tem matérias cadastradas!"
            putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
            teclatemporaria <- getLine
            agendaAluno
        else do
            putStrLn "Essas são as suas matérias:"
            putStr $ unlines matNumeradas
            putStrLn "Qual delas você deseja remover?"
            numberString <- getLine
            if ((not $ vrfNum numberString) || (read numberString) < 0 || (read numberString) > (length matListaUser) - 1)
                then do
                    putStrLn "Opção inválida, pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    rmvMat
                else do
                    let number = read numberString
                        newTodoItems = (delete (matListaUser !! number) matListaUser) ++ matListaOthers
                    hPutStr tempHandle $ unlines newTodoItems
                    hClose handle
                    hClose tempHandle
                    removeFile "infoMaterias.txt"
                    renameFile tempName "infoMaterias.txt"

                    putStrLn "Remover outra matéria?\n1. SIM\n2. NÃO"
                    putStrLn "\nDigite sua opção:"
                    esc <-getLine
                    case esc of
                        "1" -> rmvMat
                        "2" -> do
                             putStrLn "\nRemoção de matéria concluída!"
                             putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
                             teclatemporaria <- getLine
                             agendaAluno
                        otherwise -> do
                              putStrLn "\nOpção inválida!"
                              putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
                              teclatemporaria <- getLine
                              agendaAluno

grcNota = do
    clean
    putStrLn "GERENCIAMENTO DE NOTAS"
    putStrLn "1. Inserir nota"
    putStrLn "2. Ver notas"
    putStrLn "3. Voltar ao menu inicial"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    case esc of
        "1" -> do
            insNota
            agendaAluno
        "2" -> do
            rmvMat
            agendaAluno
        "3" -> agendaAluno
        otherwise -> do
            putStrLn "Opção inválida, retornando para o Gerenciamento de Notas!"
            putStrLn "Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            grcNota

insNota = do
    clean
    handle1 <- openFile "secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)

    handle <- openFile "infoNotas.txt" ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let matLista = lines contents
    let matListaUser = [elemLista |elemLista <- matLista, filtro3 elemLista cpfAtual]
    let matListaOthers = [elemListaO |elemListaO <- matLista, not $ filtro3 elemListaO cpfAtual]
        matNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ filtro4 matListaUser

    if (length matListaUser == 0)
        then do
            putStrLn "Você não tem matérias cadastradas!"
            putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
            teclatemporaria <- getLine
            agendaAluno
        else do
            putStrLn "Essas são as suas matérias:"
            putStr $ unlines matNumeradas
            putStrLn "Qual delas você deseja inserir notas?"
            numberString <- getLine
            if ((not $ vrfNum numberString) || (read numberString) < 0 || (read numberString) > (length matListaUser) - 1)
                then do
                    putStrLn "Opção inválida, pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    insNota
                else do
                    clean
                    let number = read numberString
                        materiaPos = (matListaUser !! number)
                        newTodoItems = (delete materiaPos matListaUser) ++ matListaOthers
                    hPutStr tempHandle $ unlines newTodoItems
                    hClose handle
                    hClose tempHandle
                    removeFile "infoNotas.txt"
                    renameFile tempName "infoNotas.txt"

                    putStrLn "INSERÇÃO DE NOTAS"
                    putStrLn "\nQual nota você deseja adicionar?"
                    putStrLn "1. Primeiro Exercício Escolar"
                    putStrLn "2. Segundo Exercício Escolar"
                    putStrLn "3. Proval Final"
                    putStrLn "4. Voltar ao Gerenciamento de Notas"
                    esc <- getLine
                    case esc of
                        "1" -> do
                            clean
                            addNota1 materiaPos
                            insNota
                        "2" -> do
                            clean
                            addNota2 materiaPos
                            insNota
                        "3" -> do
                            clean
                            addFinal materiaPos
                            insNota
                        "4" -> do
                            clean
                            grcNota
                        "5" -> do
                            verInf
                            agendaAluno
                        otherwise -> do
                            putStrLn "Opção inválida, retornando para o Gerenciamento de Notas!"
                            putStrLn "Pressione qualquer tecla para continuar!"
                            teclatemporaria <- getLine
                            grcNota

addNota1 :: String -> IO ()
addNota1 input = do
    let nota1 = getNota input 1
    if (nota1 /= "[n1]")
        then do
            putStrLn "AVISO:\nVocê já tem uma nota cadastrada nessa categoria, caso não deseje altera-la, apenas tecle ENTER!"
            putStrLn "Caso contrário, digite o novo valor da nota!"
        else do
            putStrLn "Digite a nota do Primeiro Exercício Escolar:"
    n1 <- getLine
    if null n1
        then do
            putStrLn "Você não adicionou/alterou nenhuma nota, retornando para o Gerenciamento de Notas!"
            putStrLn "Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            add ["infoNotas.txt", input]
            grcNota
        else do
            if ((not $ vrfNota n1) || (read n1 :: Float) < 0 || (read n1 :: Float) > 10)
                then do
                    putStrLn "Nota no formato inválido, operação cancelada, retornando para Inserção de Notas!"
                    putStrLn "Pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    add ["infoNotas.txt", input]
                    insNota
                else do
                    let newNota1 = insN1 n1 input
                    add ["infoNotas.txt", newNota1]
                    putStrLn "Nota atualizada com sucesso!"
                    putStrLn "Deseja adicionar a nota do Segundo Exercício Escolar?\n1. SIM\n2. NÃO"
                    putStrLn "\nDigite sua opção:"
                    esc <-getLine
                    case esc of
                        "1" -> do
                            clean
                            addNota2 newNota1
                        "2" -> do
                            putStrLn "Operação encerrada, voltando para o Gerenciamento de Notas!"
                            putStrLn "Pressione qualquer tecla para continuar!"
                            teclatemporaria <- getLine
                            grcNota
                        otherwise -> do
                            putStrLn "Opção inválida, retornando para o Gerenciamento de Notas!"
                            putStrLn "Pressione qualquer tecla para continuar!"
                            teclatemporaria <- getLine
                            grcNota

addNota2 :: String -> IO ()
addNota2 input = do
    handle <- openFile "infoNotas.txt" ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let matLista = lines contents
    let nota1 = getNota input 1
    let nota2 = getNota input 2
    let newTodoItems = [x | x <- matLista, x /= input]
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "infoNotas.txt"
    renameFile tempName "infoNotas.txt"

    if (nota1 == "[n1]")
        then do
            putStrLn "Não é possível adicionar a nota do Segundo Exercício Escolar!"
            putStrLn "A nota do Primeiro Exercício Escolar não foi adicionada!"
            add ["infoNotas.txt", input]
            putStrLn "Retornando para o Gerenciamento de Notas. Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            grcNota
        else do
            if (nota2 /= "[n2]")
                then do
                    putStrLn "AVISO:\nVocê já tem uma nota cadastrada nessa categoria, caso não deseje altera-la, apenas tecle ENTER!"
                    putStrLn "Caso contrário, digite o novo valor da nota!"
                else do
                    putStrLn "Digite a nota do Segundo Exercício Escolar:"
            n2 <- getLine
            if null n2
                then do
                    putStrLn "Você não adicionou/alterou nenhuma nota, retornando para o Gerenciamento de Notas!"
                    putStrLn "Pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    add ["infoNotas.txt", input]
                    grcNota
                else do
                    if ((not $ vrfNota n2) || (read n2 :: Float) < 0 || (read n2 :: Float) > 10)
                        then do
                            putStrLn "Nota no formato inválido, operação cancelada, retornando para Inserção de Notas!"
                            putStrLn "Pressione qualquer tecla para continuar!"
                            teclatemporaria <- getLine
                            add ["infoNotas.txt", input]
                            insNota
                        else do
                            let newNota2 = insN2 n2 input
                            add ["infoNotas.txt", newNota2]
                            putStrLn "Nota atualizada com sucesso!"
                            putStrLn "Deseja adicionar a nota da Prova final?\n1. SIM\n2. NÃO"
                            putStrLn "\nDigite sua opção:"
                            esc <-getLine
                            case esc of
                                "1" -> do
                                    clean
                                    addFinal newNota2
                                "2" -> do
                                    putStrLn "Operação encerrada, voltando para o Gerenciamento de Notas!"
                                    putStrLn "Pressione qualquer tecla para continuar!"
                                    teclatemporaria <- getLine
                                    grcNota
                                otherwise -> do
                                    putStrLn "Opção inválida, retornando para o Gerenciamento de Notas!"
                                    putStrLn "Pressione qualquer tecla para continuar!"
                                    teclatemporaria <- getLine
                                    grcNota

addFinal :: String -> IO ()
addFinal input = do
    handle <- openFile "infoNotas.txt" ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let matLista = lines contents
    let notaFinal = getNota input 3
    let newTodoItems = [x | x <- matLista, x /= input]
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "infoNotas.txt"
    renameFile tempName "infoNotas.txt"
    let nota1 = getNota input 1
    let nota2 = getNota input 2

    if (nota1 == "[n1]" || nota2 == "[n2]")
        then do
            putStrLn "Não é possível adicionar uma nota final, as notas anteriores não foram adicionadas por completo!"
            add ["infoNotas.txt", input]
            putStrLn "Retornando para o Gerenciamento de Notas. Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            grcNota
        else do
            let media = ((read nota1 :: Float) + (read nota2 :: Float))/2
            if media < 3
                then do
                    putStrLn "Não é possível adicionar uma nota final, sua média está abaixo de 3 (três)!"
                    add ["infoNotas.txt", input]
                    putStrLn "Retornando para o Gerenciamento de Notas. Pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    grcNota
                else do
                    if media > 7
                        then do
                            putStrLn "Não é possível adicionar uma nota final, sua média está acima de 7 (sete)!"
                            add ["infoNotas.txt", input]
                            putStrLn "Retornando para o Gerenciamento de Notas. Pressione qualquer tecla para continuar!"
                            teclatemporaria <- getLine
                            grcNota
                        else do
                            if (notaFinal /= "[nf]")
                                then do
                                    putStrLn "AVISO:\nVocê já tem uma nota cadastrada nessa categoria, caso não deseje altera-la, apenas tecle ENTER!"
                                    putStrLn "Caso contrário, digite o novo valor da nota!"
                                else do
                                    putStrLn "Digite a nota da Prova Final:"
                                    nf <- getLine
                                    if null nf
                                        then do
                                            putStrLn "Você não adicionou/alterou nenhuma nota, retornando para o Gerenciamento de Notas!"
                                            putStrLn "Pressione qualquer tecla para continuar!"
                                            teclatemporaria <- getLine
                                            add ["infoNotas.txt", input]
                                            grcNota
                                        else do
                                            if ((not $ vrfNota nf) || (read nf :: Float) < 0 || (read nf :: Float) > 10)
                                                then do
                                                    putStrLn "Nota no formato inválido, operação cancelada, retornando para Inserção de Notas!"
                                                    putStrLn "Pressione qualquer tecla para continuar!"
                                                    teclatemporaria <- getLine
                                                    add ["infoNotas.txt", input]
                                                    insNota
                                                else do
                                                    let newNotaF = insNf nf input
                                                    add ["infoNotas.txt", newNotaF]
                                                    putStrLn "Nota atualizada com sucesso!"
                                                    putStrLn "Operação encerrada, voltando para o Gerenciamento de Notas!"
                                                    putStrLn "Pressione qualquer tecla para continuar!"
                                                    teclatemporaria <- getLine
                                                    grcNota

verInf = do
        clean
        handle1 <- openFile "secaoAtual.txt" ReadMode
        contents1 <- hGetContents handle1
        let secao = lines contents1
        handle2 <- openFile "cadastro.txt" ReadMode
        contents2 <- hGetContents handle2
        let cdstr = lines contents2
        let cpfAtual = cpfSecaoAtual cdstr (head secao)

        handle3 <- openFile "infoAlunos.txt" ReadMode
        tempdir <- getTemporaryDirectory
        (tempName, tempHandle) <- openTempFile tempdir "temp"
        contents <- hGetContents handle3
        let infLista = lines contents
        let infListaAux = [elemLista |elemLista <- infLista]
        let infListaNormalizada = splitOn "," (head infListaAux)
        let infListaInter =  zipWith (++) ["Nome: ","Cpf: ","Idade: ", "Curso: ", "Instituição: "] infListaNormalizada

        putStrLn "    Informações\n"
        putStr $ unlines infListaInter
        putStrLn "\n"

        handle <- openFile "infoMaterias.txt" ReadMode
        contents <- hGetContents handle
        let matLista = lines contents
        let matListaUser = [elemLista |elemLista <- matLista, filtro3 elemLista cpfAtual]
        let matListaOthers = [elemListaO |elemListaO <- matLista, not $ filtro3 elemListaO cpfAtual]
            matNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ filtro4 matListaUser

        putStrLn "  Materias Cadastradas\n"
        if(length matNumeradas == 0)
            then do
                putStrLn "Você não tem materias cadastradas!"
            else do
                putStr $ unlines matNumeradas

        hClose handle1
        hClose handle2
        hClose handle3

        putStrLn "\nPressione qualquer tecla para retorna a agenda!"
        teclatemporaria <- getLine
        putStrLn ""



--cadastro de periodo (periodo atual, materias com professor, notas com ate 3 entradas)
--adicionar e remover data de prova (e assuntos) ~~ tem parecido no livro
--notas (mostrar materia, professor, notas e media)
--calendario (mostrar data das provas e assuntos)
