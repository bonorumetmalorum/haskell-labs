--(*) :t is Num -> Num -> Num
--(&&) True :t is Bool -> Bool
-- \x -> \f -> f(f x) :t is a->(a -> a)->a
-- tail [1, 2, 3] :t is [Num]
-- error :t is [char] -> a
-- \(x, y) -> x&&True :t (Bool, a) -> Bool
equal::(Eq a) => [a] -> [a] -> Bool
equal [] [] = True
equal (x:xs) (y:ys)
    |x == y = equal xs ys
    |otherwise = False
equal _ _ = False

myReverse::[a] -> [a]
myReverse [] = []
myReverse xs = foldl (\acc x -> [x] ++ acc ) [] xs

palindrom::[Char] -> Bool
palindrom xs = equal xs (myReverse xs)

data BinaryTree a = EmptyTree | Node a  (BinaryTree a) (BinaryTree a) deriving Show

mapTree::(a -> b) -> BinaryTree a -> BinaryTree b
mapTree f EmptyTree = EmptyTree
mapTree f (Node v EmptyTree EmptyTree) = Node (f v) EmptyTree EmptyTree
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)



