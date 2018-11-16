sumAll::[Int] -> Int
sumAll x = foldl (+) 0 x

multAll::[Int] -> Int
multAll x = foldl (*) 1 x

myfoldr::(a -> b -> b) -> b -> [a] -> b 
myfoldr f acc [] = acc
myfoldr f acc (x:xs) =  f x (myfoldr f acc xs) 

myfoldl::(a -> b -> b) -> b -> [a] -> b
myfoldl f z [] = z
myfoldl f z (x:xs) = myfoldl f (f x z) xs

len::[a] -> Int
len x = myfoldr (\v acc -> acc + 1) 0 x

maxElem::[Int] -> Int
maxElem x = myfoldr (\v acc -> if v > acc then v else acc) 0 x

flatten::[[a]] -> [a]
flatten x = myfoldr (\v acc -> v++acc) [] x

data IntOrBool = Int | Boolean deriving Show

x = [Int, Int, Boolean, Boolean] --this is a polymorphic data type

data Name = String

data Lambda = Var | Lam | App Lambda Lambda

fix::(a -> a) -> a
fix f a = f (fix f)

fact:: Int -> Int
fact f x = if x == 0 then 1 else x * (f (x - 1))

