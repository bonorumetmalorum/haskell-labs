import Data.Maybe
--LISTS
lastly::[a] -> Maybe a
lastly [] = Nothing
lastly (h:[]) = Just h
lastly (h:t) =  lastly t

penult::[a] -> Maybe a
penult [] = Nothing
penult (h:t:[]) = Just h
penult (h:t) = penult t

largest::(Ord a) => [a] -> a -> a
largest [] acc = acc
largest (h:t) acc = if h > acc then largest t h else largest t acc

smallest::(Ord a) => [a] -> a -> a
smallest [] acc = acc
smallest (h:t) acc = if h < acc then smallest t h else smallest t acc

s_n_l::(Ord a, Num a) => [a] -> (a, a)
s_n_l list = foldr (\a acc -> (if a < (fst acc) then a else fst acc, if a > (snd acc) then a else snd acc)) (999999, 0) list

no_dup::(Eq a) => [a] -> [a]
no_dup list = foldr (\a acc -> case acc of [] -> [a]; (h:t) -> if a == h then acc else [a] ++ acc) [] list

dup::[a] -> [a]
dup list = foldr (\a acc -> a:a:acc) [] list

duplicaten::[a] -> Integer -> [a]
duplicaten [] num = []
duplicaten list 0 = list
duplicaten (h:t) num = (go h (num - 1)) ++ duplicaten t num where go elem no = if no == 0 then elem:[] else elem:(go elem (no - 1))

drop_nth::[a] -> Integer -> [a]
drop_nth list num = go list num where 
                                    go [] no = []
                                    go (h:t) no = if no == 1 then go t num else h:(go t (no - 1))

change::Integer -> [Integer] -> [Integer]
change n [] = []
change amnt (h:t) = (div amnt h):(change (mod amnt h) t)

index :: (Num t1, Eq t1) => [t] -> t1 -> Maybe t
index [] n = Nothing
index (h:t) 0 = Just h
index (h:t) n = index t (n-1)

index2:: (Num a, Eq a) => ([b], a) -> Maybe b
index2 (([]), n) = Nothing
index2 ((h:t), 0) = Just h
index2 ((h:t), n) = index2 (t, (n - 1))

rotate_1r::[a] -> [a]
rotate_1r [] = []
rotate_1r (h:t) = t ++ [h]

split::[a] -> Integer -> [a] -> ([a], [a])
split [] n a = ([], a)
split (h:t) 0 a = (a, (h:t))
split (h:t) num a = split t (num -1) (a ++ [h])

rotn::[a] -> Integer -> [a]
rotn list n = let (a, b) = split list n [] in b++a

num_of_lower::[Char] -> Int
num_of_lower list = length [x|x<-list, x >= 'a', x <= 'z'] 

num_of_upper::[Char] -> Int
num_of_upper list = length [x|x<-list, x >= 'A', x <= 'Z'] 