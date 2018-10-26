so3 x y z = x + y + z

so3_1 (x, y) z = x + y + z

so3_2 x (y, z) = x + y + z

so3_3 (x, y , z) = x + y + z

so3_4 (x) y (z) = x + y + z

fargs x y z t = x + y + y + z

-- [x | x <- [1..], x < 6] = [1, 2, 3, 4, 5 ... never ending since the range is infinite
-- [1, 2, 3, 4, 5] & [x | x <- [1..5]]

inorder::Ord a => [a] -> Bool
inorder [] = True
inorder (x:[]) = True
inorder (x:y:xs) = if x < y then inorder (y:xs) else False

insert::Ord a => a -> [a] -> [a]
insert a (x:[]) = if a <= x then a:x:[] else x:a:[]
insert a (x:xs) = if a < x then a:x:xs else x:insert a xs

sort:: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = 
    let smaller = sort [a | a <- xs, a <= x]
        bigger = sort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

data BinaryTree a = EmptyTree | Node a  (BinaryTree a) (BinaryTree a) deriving (Show)

toList:: BinaryTree a -> [a]
toList (EmptyTree) = []
toList (Node x EmptyTree EmptyTree) = [x]
toList (Node x l r) =
    let left = toList l
        right = toList r
    in left ++ [x] ++ right

isTreeSorted::(Ord a) => BinaryTree a -> Bool
isTreeSorted tree = (inorder . toList) tree

insertTree::(Ord a) => BinaryTree a -> a -> BinaryTree a
insertTree (EmptyTree) x = Node x EmptyTree EmptyTree
insertTree (Node v l r) x
    |x > v = Node v l (insertTree r x)
    |x < v = Node v (insertTree l x) r
    |otherwise = Node x l r
