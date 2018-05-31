import System.IO
import System.Directory
import Data.List
import Data.List.Split

main = do
    putStrLn "Seja Bem vindo a Agenda do Estudante: \n 1.Login \n 2.Cadastro"
    esc <- getLine
    menuCtrl esc

menuCtrl esc| esc == "1" = do
                putStrLn "~~Tela de login"
                login
                main
            | esc == "2" = do
                cadastro
                main
            | otherwise = main

login = do
    putStrLn "Login: "
    log <- getLine
    putStrLn "Senha: "
    senha <- getLine
    let logAluno = (log ++ " " ++ senha)
    handle <- openFile "cadastro.txt" ReadMode
    contents <- hGetContents handle
    let listAlunos = lines contents
    let controlador = verif listAlunos logAluno -- add exception
    if controlador
        then
            agendaAluno
            else do
                --exception
                login

agendaAluno = do -- fazer
    putStrLn "Voce esta na agenda"
cadastro = do
    putStrLn "Nome: "
    nome <- getLine
    putStrLn "Senha: "
    senha <- getLine -- add confirme sua senha
    let ctrAluno = (nome ++ " " ++ senha)
    handle <- openFile "cadastro.txt" ReadMode
    contents <- hGetContents handle
    let listAlunos = lines contents
    let controlador = verif (filtro listAlunos) nome -- add exception
    if controlador
        then do
            putStrLn "Usuario ja existe"
            main
            else do
                add ["cadastro.txt", ctrAluno] -- lembrar de fazer as exception


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

verif :: [String] -> String -> Bool
verif listCtr logUser = logUser `elem` listCtr

filtro :: [String] -> [String]
filtro [] = []
filtro (a:as) = (head (splitOn " " (a))):filtro as
