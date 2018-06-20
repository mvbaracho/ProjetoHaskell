module Agenda
    (agendaAluno
    ) where

import System.IO
import System.Directory
import System.FilePath
import Data.List
import Data.List.Split
import FuncAux
import {-# SOURCE #-} MainAluno (mainAluno)
import System.IO
import System.IO.Error
import System.Process


agendaAluno = do -- fazer
    clean
    putStrLn "Bem vindo a sua Agenda do Estudante!\n\nO que voce deseja fazer?\n"
    putStrLn "1. Inserir disciplina"
    putStrLn "2. Remover disciplina"
    putStrLn "3. Gerenciar notas"
    putStrLn "4. Gerenciar atividades no Calendário do Aluno"
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
            calendarioAluno
            agendaAluno
        "5" -> do
            verInf
            agendaAluno
        "6" -> mainAluno
        otherwise -> agendaAluno


calendarioAluno = do
    clean
    putStrLn "\tCALENDÁRIO DO ALUNO\n"
    putStrLn "1. Inserir Atividade"
    putStrLn "2. Remover Atividade"
    putStrLn "3. Voltar"
    putStrLn "\nDigite sua opção:"
    esc <- getLine
    case esc of
        "1" -> do
            insData
            agendaAluno
        "2" -> do
            removeData
            agendaAluno
        "3" -> agendaAluno
        otherwise -> do
            clean
            putStrLn "Opção inválida, retornando para o Calendário do Aluno!"
            putStrLn "Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            calendarioAluno


insData = do
    clean
    handle1 <- openFile "Dados\\secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "Dados\\cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)

    putStrLn "\tCALENDÁRIO DO ALUNO\n"
    putStrLn "Informe a data (digite no modelo dd/mm/aaaa): "
    dataInf <- getLine
    if (not $ validaData dataInf)
        then do
            putStrLn "\nData Inválida, retornando ao calendário"
            putStrLn "Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            calendarioAluno
        else do
            putStrLn "\nDescrição da atividade: "
            desData <- getLine
            let dataAtual = cpfAtual ++ "[{(,]})" ++ dataInf ++ "[{(,]})" ++ desData
            add ["Dados\\calendario.txt", dataAtual]
            clean
            putStrLn "\tCALENDÁRIO DO ALUNO\n"
            putStrLn "\nAdicionar outra Atividade?\n1. SIM\n2. NÃO"
            putStrLn "\nDigite sua opção:"
            esc <-getLine
            case esc of
                "1" -> insData
                "2" -> do
                    clean
                    putStrLn "\tCALENDÁRIO DO ALUNO\n"
                    putStrLn "\nInserção de Atividades concluido"
                    putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
                    teclatemporaria <- getLine
                    agendaAluno
                otherwise -> do
                    clean
                    putStrLn "\tCALENDÁRIO DO ALUNO\n"
                    putStrLn "\nOpção inválida!"
                    putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
                    teclatemporaria <- getLine
                    agendaAluno


removeData = do
    clean
    handle1 <- openFile "Dados\\secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "Dados\\cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)

    handle <- openFile "Dados\\calendario.txt" ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let atiData = lines contents
    let atiAtual = [elemLista |elemLista <- atiData, filtroData elemLista cpfAtual]
    let atiAtualOthers = [elemListaO |elemListaO <- atiData, not $ filtroData elemListaO cpfAtual]
        atiNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ ordena (filtroData2 atiAtual)

    if (length atiAtual == 0)
        then do
            putStrLn "\tCALENDÁRIO DO ALUNO\n"
            putStrLn "Você não tem Atividades cadastradas no Calendário!\n"
            putStrLn "Pressione qualquer tecla para retornar ao menu principal!"
            teclatemporaria <- getLine
            agendaAluno
        else do
            putStrLn "\tCALENDÁRIO DO ALUNO\n"
            putStrLn "Essas são as suas Atividades:\n"
            putStr $ unlines atiNumeradas
            putStrLn "\nQual delas você deseja remover?"
            numberString <- getLine
            if ((not $ vrfNum numberString) || (read numberString) < 0 || (read numberString) > (length atiAtual) - 1)
                then do
                    putStrLn "Opção inválida, pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    removeData
                else do
                    let number = read numberString
                        newTodoItems = (delete (atiAtual !! number) atiAtual) ++ atiAtualOthers
                    hPutStr tempHandle $ unlines newTodoItems
                    hClose handle
                    hClose tempHandle
                    removeFile "Dados\\calendario.txt"
                    renameFile tempName "Dados\\calendario.txt"

                    putStrLn "Remover outra Atividade?\n1. SIM\n2. NÃO"
                    putStrLn "\nDigite sua opção:"
                    esc <-getLine
                    case esc of
                        "1" -> removeData
                        "2" -> do
                            putStrLn "\nRemoção de Atividades concluída!"
                            putStrLn "Pressione qualquer tecla para retornar ao Calendario do Aluno!"
                            teclatemporaria <- getLine
                            calendarioAluno
                        otherwise -> do
                            putStrLn "\nOpção inválida!"
                            putStrLn "Pressione qualquer tecla para retornar ao Calendario do Aluno!"
                            teclatemporaria <- getLine
                            calendarioAluno

insMtr = do
    clean
    handle1 <- openFile "Dados\\secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "Dados\\cadastro.txt" ReadMode
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
            handle <- openFile "Dados\\infoMaterias.txt" ReadMode
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
                    add ["Dados\\infoMaterias.txt", inputMateria]
                    add ["Dados\\infoNotas.txt", inputNotas]
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
    handle1 <- openFile "Dados\\secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "Dados\\cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)

    handle <- openFile "Dados\\infoMaterias.txt" ReadMode
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
                    removeFile "Dados\\infoMaterias.txt"
                    renameFile tempName "Dados\\infoMaterias.txt"

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
            verNotas
            agendaAluno
        "3" -> agendaAluno
        otherwise -> do
            putStrLn "Opção inválida, retornando para o Gerenciamento de Notas!"
            putStrLn "Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            grcNota

insNota = do
    clean
    handle1 <- openFile "Dados\\secaoAtual.txt" ReadMode
    contents1 <- hGetContents handle1
    let secao = lines contents1
    handle2 <- openFile "Dados\\cadastro.txt" ReadMode
    contents2 <- hGetContents handle2
    let cdstr = lines contents2
    let cpfAtual = cpfSecaoAtual cdstr (head secao)

    handle <- openFile "Dados\\infoNotas.txt" ReadMode
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
                    removeFile "Dados\\infoNotas.txt"
                    renameFile tempName "Dados\\infoNotas.txt"

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
            add ["Dados\\infoNotas.txt", input]
            grcNota
        else do
            if ((not $ vrfNota n1) || (read n1 :: Float) < 0 || (read n1 :: Float) > 10)
                then do
                    putStrLn "Nota no formato inválido, operação cancelada, retornando para Inserção de Notas!"
                    putStrLn "Pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    add ["Dados\\infoNotas.txt", input]
                    insNota
                else do
                    let newNota1 = insN1 n1 input
                    add ["Dados\\infoNotas.txt", newNota1]
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
    handle <- openFile "Dados\\infoNotas.txt" ReadMode
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
    removeFile "Dados\\infoNotas.txt"
    renameFile tempName "Dados\\infoNotas.txt"

    if (nota1 == "[n1]")
        then do
            putStrLn "Não é possível adicionar a nota do Segundo Exercício Escolar!"
            putStrLn "A nota do Primeiro Exercício Escolar não foi adicionada!"
            add ["Dados\\infoNotas.txt", input]
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
                    add ["Dados\\infoNotas.txt", input]
                    grcNota
                else do
                    if ((not $ vrfNota n2) || (read n2 :: Float) < 0 || (read n2 :: Float) > 10)
                        then do
                            putStrLn "Nota no formato inválido, operação cancelada, retornando para Inserção de Notas!"
                            putStrLn "Pressione qualquer tecla para continuar!"
                            teclatemporaria <- getLine
                            add ["Dados\\infoNotas.txt", input]
                            insNota
                        else do
                            let newNota2 = insN2 n2 input
                            add ["Dados\\infoNotas.txt", newNota2]
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
    handle <- openFile "Dados\\infoNotas.txt" ReadMode
    tempdir <- getTemporaryDirectory
    (tempName, tempHandle) <- openTempFile tempdir "temp"
    contents <- hGetContents handle
    let matLista = lines contents
    let notaFinal = getNota input 3
    let newTodoItems = [x | x <- matLista, x /= input]
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "Dados\\infoNotas.txt"
    renameFile tempName "Dados\\infoNotas.txt"
    let nota1 = getNota input 1
    let nota2 = getNota input 2

    if (nota1 == "[n1]" || nota2 == "[n2]")
        then do
            putStrLn "Não é possível adicionar uma nota final, as notas anteriores não foram adicionadas por completo!"
            add ["Dados\\infoNotas.txt", input]
            putStrLn "Retornando para o Gerenciamento de Notas. Pressione qualquer tecla para continuar!"
            teclatemporaria <- getLine
            grcNota
        else do
            let media = ((read nota1 :: Float) + (read nota2 :: Float))/2
            if media < 3
                then do
                    putStrLn "Não é possível adicionar uma nota final, sua média está abaixo de 3 (três)!"
                    add ["Dados\\infoNotas.txt", input]
                    putStrLn "Retornando para o Gerenciamento de Notas. Pressione qualquer tecla para continuar!"
                    teclatemporaria <- getLine
                    grcNota
                else do
                    if media > 7
                        then do
                            putStrLn "Não é possível adicionar uma nota final, sua média está acima de 7 (sete)!"
                            add ["Dados\\infoNotas.txt", input]
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
                                            add ["Dados\\infoNotas.txt", input]
                                            grcNota
                                        else do
                                            if ((not $ vrfNota nf) || (read nf :: Float) < 0 || (read nf :: Float) > 10)
                                                then do
                                                    putStrLn "Nota no formato inválido, operação cancelada, retornando para Inserção de Notas!"
                                                    putStrLn "Pressione qualquer tecla para continuar!"
                                                    teclatemporaria <- getLine
                                                    add ["Dados\\infoNotas.txt", input]
                                                    insNota
                                                else do
                                                    let newNotaF = insNf nf input
                                                    add ["Dados\\infoNotas.txt", newNotaF]
                                                    putStrLn "Nota atualizada com sucesso!"
                                                    putStrLn "Operação encerrada, voltando para o Gerenciamento de Notas!"
                                                    putStrLn "Pressione qualquer tecla para continuar!"
                                                    teclatemporaria <- getLine
                                                    grcNota

verInf = do
        clean
        handle1 <- openFile "Dados\\secaoAtual.txt" ReadMode
        contents1 <- hGetContents handle1
        let secao = lines contents1
        handle2 <- openFile "Dados\\cadastro.txt" ReadMode
        contents2 <- hGetContents handle2
        let cdstr = lines contents2
        let cpfAtual = cpfSecaoAtual cdstr (head secao)

        handle3 <- openFile "Dados\\infoAlunos.txt" ReadMode
        tempdir <- getTemporaryDirectory
        (tempName, tempHandle) <- openTempFile tempdir "temp"
        contents <- hGetContents handle3
        let infLista = lines contents
        let infLista1 = filtro5 (infLista) (cpfAtual)
        let infListaAux = [elemLista |elemLista <- infLista1]
        let infListaNormalizada = splitOn "," (head infListaAux)
        let infListaInter =  zipWith (++) ["Nome: ","Cpf: ","Idade: ", "Curso: ", "Instituição: "] infListaNormalizada

        putStrLn "\tINFORMAÇÕES\n"
        putStr $ unlines infListaInter
        putStrLn "\n"

        handleData <- openFile "Dados\\calendario.txt" ReadMode
        contentsData <- hGetContents handleData
        let atiData = lines contentsData
        let atiAtual = [elemLista |elemLista <- atiData, filtroData elemLista cpfAtual]
        let atiAtualOthers = [elemListaO |elemListaO <- atiData, not $ filtroData elemListaO cpfAtual]
            atiNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ ordena (filtroData2 atiAtual)

        putStrLn "\tCALENDÁRIO DO ALUNO\n"
        if(length atiNumeradas == 0)
            then do
                putStrLn "Você não tem Atividades cadastradas no Calendário!"
            else do
                putStr $ unlines atiNumeradas

        handle <- openFile "Dados\\infoMaterias.txt" ReadMode
        contents <- hGetContents handle
        let matLista = lines contents
        let matListaUser = [elemLista |elemLista <- matLista, filtro3 elemLista cpfAtual]
            matNumeradas = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ filtro4 matListaUser

        putStrLn "\n\tMATERIAS CADASTRADAS\n"
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

verNotas = do
          clean
          handle1 <- openFile "Dados\\secaoAtual.txt" ReadMode
          contents1 <- hGetContents handle1
          let secao = lines contents1
          handle2 <- openFile "Dados\\cadastro.txt" ReadMode
          contents2 <- hGetContents handle2
          let cdstr = lines contents2
          let cpfAtual = cpfSecaoAtual cdstr (head secao)
          putStrLn "    NOTAS\n"
          handle <- openFile "Dados\\infoNotas.txt" ReadMode
          contents <- hGetContents handle
          let notas = lines contents
          let notasWithUser = [elemLista |elemLista <- notas, filtro3 elemLista cpfAtual]
              notasN = zipWith (\n line -> show n ++ " - " ++ line) [0..] $ filtro4 notasWithUser
          if(length notasN == 0)
              then do
                  putStrLn "Você não tem notas cadastradas!"
              else do
                  ntsFrmtds notasWithUser

          hClose handle1
          hClose handle2

          putStrLn "\nPressione qualquer tecla para retorna a agenda!"
          teclatemporaria <- getLine
          putStrLn ""



--cadastro de periodo (periodo atual, materias com professor, notas com ate 3 entradas)
--adicionar e remover data de prova (e assuntos) ~~ tem parecido no livro
--notas (mostrar materia, professor, notas e media)
--calendario (mostrar data das provas e assuntos)
