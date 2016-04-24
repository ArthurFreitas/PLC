type Ponto = (Float, Float)
type Reta = (Ponto,Ponto)

fstCordinate :: Ponto -> Float
fstCordinate (x,y) = x

sndCordinate :: Ponto -> Float
sndCordinate (x,y) = y

isVertical :: Reta -> Bool
isVertical ((x1,y1),(x2,y2)) = x1 == x2

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),("Andre","Duna"),("Fernando","Jonathan Strange & Mr.Norrell"),("Fernando","A Game of Thrones")]

membro :: [Int] -> Int -> Bool
membro list number = [y| y <- list, y == number] /= []

livros :: BancoDados -> Pessoa -> [Livro]
livros dados pessoa = [y| (x,y) <- dados, x == pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos dados livro = [x| (x,y) <- dados, y == livro]

emprestado :: BancoDados -> Livro -> Bool
emprestado dados livro =  emprestimos dados livro /= []

qtdEmprestados :: BancoDados -> Pessoa -> Int
qtdEmprestados dados pessoa =  length (livros dados pessoa )

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar dados pessoa livro = (pessoa, livro): dados

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver dados pessoa livro = [(x,y)| (x,y) <- dados, x /= pessoa && livro /= y]