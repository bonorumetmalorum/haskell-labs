type Clause = (String, [String])

query::String -> [Clause] -> (Bool, [String])
query q (x:xs) = if q == (fst x) then (True, (snd x)) else query q xs
query q ([]) = (False, [])

solution::[String] -> [Clause] -> Bool
solution (x:xs) ((c, tail):cs) = if (fst (query x ((c, tail):cs))) && (solution tail cs) then solution xs ((c, tail):cs) else False
solution ([]) clauses = True

knowledgeBase = [ ("likeItalianFood",["likeRissoto","likePasta","likePizza"]),("likeRissoto",[]), ("likePasta",[]), ("likePizza",[])]

putString :: String -> IO ()
putString [] = return ()
putString (x:xs) = do putChar x
                      putStr xs

len :: IO ()
len = do
    putStrLn "please type something and press return"
    input <- getLine
    putStrLn ("the length of your text is " ++ (show (length input)))

