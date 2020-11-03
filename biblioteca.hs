import Data.List
import System.Directory

-- //////////////////////////////////////  DATA  //////////////////////////////////////

data Livro = Livro {
	indice :: Integer,
	nome :: String,
	genero :: String,
	ano :: String,

    autor :: String
} deriving (Show)

-- //////////////////////////////////////  LIVROS INICIAIS  //////////////////////////////////////

livrosCadastrados :: [Livro]
livrosCadastrados = [Livro {indice = 1, nome = "Fahrenheit 451", genero = "distopia", ano = "1953", autor = "Ray Bradbury"},
					 Livro {indice = 2, nome = "1984", genero = "distopia", ano = "1949", autor = "George Orwell"},
					 Livro {indice = 3, nome = "O Iluminado", genero = "terror", ano = "1977", autor = "Stephen King"},
					 Livro {indice = 4, nome = "Doutor Sono", genero = "terror", ano = "2013", autor = "Stephen King"},
					 Livro {indice = 5, nome = "A peste", genero = "filosofia", ano = "1947", autor = "Albert Camus"},
					 Livro {indice = 6, nome = "Cem anos de solidão", genero = "realismo mágico", ano = "2001", autor = "Gabriel García Márquez"},
					 Livro {indice = 7, nome = "O Gene Egoísta", genero = "Biologia", ano = "1976", autor = "Richard Dawkins"}]
					 

-- //////////////////////////////////////  MENU  //////////////////////////////////////

main :: IO ()
main = do
    menuPrint
    menuOpcao


menuPrint :: IO ()
menuPrint = do
	putStrLn "Bem-vindo!"


menuOpcao :: IO ()
menuOpcao = do
    putStrLn "0 - Sair" 
    putStrLn "1 - Listar todos os livros"
    putStrLn "2 - Listar livros disponiveis"
    putStrLn "3 - Listar os seus livros alugados"
    putStrLn "4 - Realizar Aluguel"
    putStrLn "5 - Realizar Devolução"
    putStrLn "6 - Listar livros por genero"
    putStrLn "7 - Enviar sugestão de livro"
    putStrLn "8 - Visuzalizar suas sugestões de livro"
    putStrLn "\nOpcao: "
    opcao <- getLine
    if (read opcao) == 0 then putStrLn("Fim") else do opcaoEscolhida (read opcao)


menuListagem :: IO()
menuListagem = do
    putStrLn "0 - Voltar para o menu principal" 
    putStrLn "1 - Descobrir mais informações sobre um livro"
    putStr "\nOpção: "
    opcao <- getLine
    if (read opcao) == 1
        then do
            visualizarInfoLivro
    else if (read opcao) /= 0 
        then do
            putStrLn "==> Opção inválida"
    else
        putStr ""
    printEspaco
    
    
-- //////////////////////////////////////  ESCOLHER OPÇÃO  //////////////////////////////////////

opcaoEscolhida :: Int -> IO()
opcaoEscolhida opcao | opcao == 1 = do {imprimeLivros ; menuListagem; menuOpcao} 
                     | opcao == 2 = do {imprimeDisponiveis ; menuOpcao}
                     | opcao == 3 = do {imprimeAlugados ; menuOpcao}
                     | opcao == 4 = do {realizarAluguel ; menuOpcao}
                     | opcao == 5 = do {realizarDevolucao ; menuOpcao}
                     | opcao == 6 = do {imprimeListarLivrosPorGenero; menuOpcao}    
                     | opcao == 7 = do {enviarSugestao; menuOpcao}  
                     | opcao == 8 = do {visualizarSugetoes; menuOpcao}  
                     | otherwise =  do {putStrLn "Opcao invalida, Porfavor escolha uma opcao valida" ; menuOpcao}

-- //////////////////////////////////////  PRINTS  //////////////////////////////////////

imprimeLivros :: IO()
imprimeLivros = putStrLn ("\n\n\n" ++ (listarLivros livrosCadastrados) ++ "\n\n")


imprimeDisponiveis :: IO()
imprimeDisponiveis = do
    let todos = [1..toInteger(length livrosCadastrados)]
    alugados <- alugueis
    let disponiveis = todos \\ alugados
    printEspaco
    if disponiveis == []
        then putStrLn "Nao há livros disponíveis."
    else putStrLn (listarLivros (listarInteiroParaLivro disponiveis))
    printEspaco


imprimeAlugados :: IO()
imprimeAlugados = do
    let todos = [1..toInteger(length livrosCadastrados)]
    alugados <- alugueis
    printEspaco
    if alugados == []
        then putStrLn "Você não tem livros alugados."
    else putStrLn (listarLivros (listarInteiroParaLivro alugados))
    printEspaco


imprimeListarLivrosPorGenero :: IO()
imprimeListarLivrosPorGenero = do
    printEspaco
    putStrLn "==> Insira o nome do genero na qual você deseja filtrar: "
    genero <- getLine
    let livrosDoGenero = "\n==> Os livros que possuem esse genero são:\n" ++ unlines(listarLivrosPorGenero ( livrosCadastrados ) ([]) ( genero ))
    if livrosDoGenero == "\n==> Os livros que possuem esse genero são:\n"
		then putStrLn "\nNão há livros desse gênero na biblioteca.\n"
	else
		putStrLn livrosDoGenero
    printEspaco


visualizarSugetoes :: IO()
visualizarSugetoes = do
    printEspaco
    putStrLn "==> Suas sugetões de livros:\n"
    sugetoes <- readFile "sugetoes.txt"
    putStrLn sugetoes
    printEspaco

-- //////////////////////////////////////  OPERAÇÕES  //////////////////////////////////////

realizarAluguel :: IO()
realizarAluguel = do
    printEspaco
    putStrLn "==> Digite o codigo do livro que deseja alugar: "
    indice <- getLine
    disponivel <- estaDisponivel (read indice)
    if (disponivel && ((read indice) > 0) && ((read indice) < length livrosCadastrados))
        then do 
            adicionaAluguel (read indice)
            putStrLn "==> Livro alugado com sucesso"
    else putStrLn "==> Livro não disponível para aluguel"
    printEspaco
    

realizarDevolucao :: IO()
realizarDevolucao = do
    printEspaco
    putStrLn "==> Digite o codigo do livro que deseja devolver: "
    indice <- getLine
    disponivel <- estaDisponivel (read indice)
    if ((not disponivel) && ((read indice) > 0) && ((read indice) < length livrosCadastrados))
        then do
            removerAluguel (read indice)
            putStrLn "==> Livros devolvido com sucesso."
    else
        putStrLn "==> Este livro não foi alugado por você."
    printEspaco


enviarSugestao :: IO()
enviarSugestao = do
    printEspaco
    putStr "==> Digite o nome do livro que deseja sugerir: "
    nomeLivro <- getLine
    appendFile "sugestoes.txt" (nomeLivro ++ "\n")
    printEspaco

-- //////////////////////////////////////  AUXILIARES  //////////////////////////////////////

toStringLivro :: Livro -> String
toStringLivro (Livro {indice = i, nome = n, genero = g, ano = a, autor = d}) = show i ++ " - " ++ n     


listarLivros :: [Livro] -> String
listarLivros [] = ""
listarLivros (x:xs) = toStringLivro x ++ ['\n'] ++ listarLivros xs


alugueis :: IO [Integer]
alugueis = do
  conteudo <- readFile "alugueis.txt"
  let linhas = lines conteudo
  let numeros = fmap (read::String->Integer) linhas
  return numeros


adicionaAluguel :: Integer -> IO()
adicionaAluguel numero = do
    let conteudo = conteudoAdicionar numero
    appendFile "alugueis.txt" (conteudo)
    where
        conteudoAdicionar :: Integer -> String
        conteudoAdicionar numero
            | numero == (-1) = ""
            | otherwise = ((show numero) ++ "\n")


estaDisponivel :: Integer -> IO Bool
estaDisponivel indice = do
    alugados <- alugueis
    return $ estaDisponivel' indice alugados
    where
        estaDisponivel' :: Integer -> [Integer] -> Bool
        estaDisponivel' indice [] = True
        estaDisponivel' indice (a:as)  
            | indice == a = False           
            | otherwise = True && estaDisponivel' indice as


removerAluguel :: Integer -> IO()
removerAluguel numero = do
    listaNova <- removeElemento numero
    removeFile "alugueis.txt"
    refazArquivo listaNova
    where
        refazArquivo :: [Integer] -> IO()
        refazArquivo [] = adicionaAluguel (-1)
        refazArquivo (a:as) = do
            adicionaAluguel a
            refazArquivo as


removeElemento :: Integer -> IO [Integer]
removeElemento numero = do
    numeros <- alugueis
    return $ removeElemento' numeros numero
    where
        removeElemento' :: [Integer] -> Integer -> [Integer]
        removeElemento' [] numero = []
        removeElemento' (a:as) numero
            | a == numero = [] ++ removeElemento' as numero
            | otherwise = [a] ++ removeElemento' as numero


listarInteiroParaLivro :: [Integer] -> [Livro]
listarInteiroParaLivro lista = do
    let disponiveis = sort lista
    listarInteiroParaLivro' disponiveis livrosCadastrados
    where
        listarInteiroParaLivro' :: [Integer] -> [Livro] -> [Livro]
        listarInteiroParaLivro' [] _ = []
        listarInteiroParaLivro' (a:as) (b:bs)
            | a == (indice b) = [b] ++ listarInteiroParaLivro' as bs
            | otherwise = [] ++ listarInteiroParaLivro' (a:as) bs


ehDoGenero:: Livro -> String -> Bool
ehDoGenero (Livro {indice = i, nome = n, genero = g, ano = a, autor = d}) gen = if g == gen then True else False


listarLivrosPorGenero:: [Livro] -> [String] -> String -> [String]
listarLivrosPorGenero [] arrayDoGenero _ = arrayDoGenero
listarLivrosPorGenero (cabeca:cauda) arrayDoGenero genero
	| ehDoGenero cabeca genero = arrayDoGenero ++ [toStringLivro cabeca] ++ listarLivrosPorGenero cauda arrayDoGenero genero
	| otherwise = listarLivrosPorGenero cauda arrayDoGenero genero

infoLivro :: Livro -> String  
infoLivro livro = "- Nome: " ++ (nome livro) ++ ['\n'] ++ "- Autor(a): " ++ (autor livro) ++ ['\n'] ++ "- Gênero: " ++ (genero livro) ++ ['\n'] ++ "- Ano de Lançamento: " ++ (ano livro) ++ ['\n'] 


visualizarInfoLivro:: IO()
visualizarInfoLivro = do
    putStrLn "==> Escolha o indice do livro que você deseja visualizar:"
    indice <- getLine
    let parseIndice = read (indice)
    let indiceNaLista = parseIndice-1

    if parseIndice > 0 && parseIndice < ( length livrosCadastrados )+1
        then putStrLn ( infoLivro ( livrosCadastrados !!  indiceNaLista  ) ) 
    else putStrLn "Livro não existente."


printEspaco :: IO()
printEspaco = putStrLn "\n\n\n"

