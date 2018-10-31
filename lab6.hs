sumAll::[Int] -> Int
sumAll x = foldl (+) 0 x

multAll::[Int] -> Int
multAll x = foldl (*) 1 x

myfoldr::(a -> b -> b) -> b -> t a -> b 
myfoldr f acc (x:xs) = 