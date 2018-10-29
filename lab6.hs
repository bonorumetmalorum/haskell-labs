sumAll::(Integer x) => [x] -> [x]
sumAll x = x.foldr(\a, b -> a + b)(0)