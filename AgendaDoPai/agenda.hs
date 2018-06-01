import System.IO
import System.Directory
import Data.List
import Data.List.Split
import FuncAux

main = do
    putStrLn "Seja Bem vindo a Agenda do Estudante: \n 1.Login \n 2.Cadastro \n 3.Sair"
    esc <- getLine
    menuCtrl esc

menuCtrl esc |esc == "1" = login
             |esc == "2" = cadastro
             |esc == "3" = return ()
             |otherwise = main

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
                main
    main

agendaAluno = do -- fazer
    putStrLn "\nVOCE ESTA NA AGENDA"
    main

cadastro = do
    putStrLn "\nTELA DE CADASTRO"
    putStrLn "Nome Completo: " -- exception para nome com numeros
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
            main
            else do
                add ["cadastro.txt", ctrAluno] -- lembrar de fazer as exception
                add ["infoAlunos.txt", ifsAluno]
    main
