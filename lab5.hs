data MyBool = T|F deriving Show

myor:: MyBool -> MyBool -> MyBool
myor T T = T
myor T F = T
myor F T = T
myor F F = F

data RPS = Rock | Paper | Scissors deriving Show

beats::RPS -> RPS -> Bool
beats Rock Scissors = True
beats Scissors Paper = True
beats Paper Rock = True
beats _ _ = False

data N = Zero | Succ N deriving (Show, Eq)

add::N -> N -> N
add Zero x = x
add (Succ n) y = add n (Succ y)

multiply:: N -> N -> N
multiply Zero _ = Zero
multiply (Succ Zero) y = y
multiply (Succ x) y = multiply x (add y y)

test:: N -> N -> Bool
test x y | x == y = True | otherwise = False

