--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a _ _) = a
tripleSnd (Triple _ b _) = b
tripleThr (Triple _ _ c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quad a a b b deriving (Eq, Show) 

firstTwo (Quad a b _ _) = (a, b)
secondTwo (Quad _ _ c d) = (c, d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d

tuple1 (Tuple1 a) = Just a 
tuple1 (Tuple2 a _) = Just a
tuple1 (Tuple3 a _ _) = Just a
tuple1 (Tuple4 a _ _ _) = Just a

tuple2 (Tuple2 _ b) = Just b 
tuple2 (Tuple3 _ b _) = Just b
tuple2 (Tuple4 _ b _ _) = Just b
tuple2 _ = Nothing

tuple3 (Tuple3 _ _ c) = Just c
tuple3 (Tuple4 _ _ c _) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 _ _ _ d) = Just d
tuple4 _ = Nothing

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs
-- listLength (Cons 1 (Cons 2 (Cons 1 Nil)))

listHead Nil = error "Empty list"
listHead (Cons x xs) = x
-- listHead (Cons 2 (Cons 3 (Cons 1 Nil)))

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs
-- listTail (Cons 1 (Cons 2 (Cons 1 Nil)))

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)
-- listFoldr (-) 0 (Cons 1 (Cons 2 (Cons 1 Nil)))

listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = True
isBST (Node a NIL NIL) = True
isBST (Node a left NIL) = (a >= getValueNode(maximumBST (left))) && (isBST (left))
isBST (Node a NIL right) = (a <= getValueNode(minimumBST (right))) && (isBST (right))
isBST (Node a left right) = (a >= minL) && (a <= minR) && (isBST (left)) && (isBST (right))
                    where 
                        minR = getValueNode(minimumBST (right))
                        minL = getValueNode(maximumBST (left))

--insere uma nova chave na BST retornando a BST modificada
insert value NIL = (Node value NIL NIL)

insert value (Node a left right) | value < a = (Node a (insert value left) right)
                                 | otherwise = (Node a left (insert value right))

--retorna o Node da BST contendo o dado procurado ou entao NIL
search value NIL = NIL
search value (Node a left right) | value == a = (Node a left right)
                                 | value < a = search value left
                                 | otherwise = search value right
                        
--retorna o elmento maximo da BST
maximumBST NIL = NIL
maximumBST (Node a left NIL) = (Node a left NIL)
maximumBST (Node a _ right) = maximumBST (right)

--retorna o elemento minimo da BST
minimumBST NIL = NIL
-- minimumBST NIL
minimumBST (Node a NIL right) = (Node a NIL right)
-- minimumBST (Node 1 (Node 2 NIL NIL) NIL)
minimumBST (Node a left _) = minimumBST (left) 
-- minimumBST (Node 1 (Node (-1) (Node (-2) NIL NIL) NIL) NIL)

getValueNode (Node a _ _) = a

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor NIL = NIL
predecessor (Node _ left _) = maximumBST left

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor NIL = NIL
successor (Node _ _ right) = minimumBST right

--remove ume lemento da BST INCOMPLETO
remove NIL = NIL
remove (Node _ NIL NIL) = NIL
remove (Node _ left NIL) = left
remove (Node _ NIL right) = right

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node a left right) = ([a] ++ (preOrder left)) ++ (preOrder right)
-- preOrder (Node 4 (Node 1 NIL NIL) (Node 8 (Node 5 NIL NIL) NIL)) == [4,1,8,5]

order NIL = []
order (Node a left right) = ((order left) ++ [a]) ++ (order right)
-- order (Node 4 (Node 1 NIL NIL) (Node 8 (Node 5 NIL NIL) NIL)) == [1,4,5,8]
postOrder NIL = []
postOrder (Node a left right) = ((postOrder left) ++ (postOrder right)) ++ [a]
-- postOrder (Node 4 (Node 1 NIL NIL) (Node 8 (Node 5 NIL NIL) NIL)) == [1,5,8,4]