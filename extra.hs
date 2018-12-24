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