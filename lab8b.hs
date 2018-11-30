module Language.Functional(
    Lam,
    Name,
    Set,
    add,
    join,
    remove,
    subst,
)where

data Name = Name [Char] deriving (Show, Eq)

data Lam = Var Name | Abs Name Lam| App Lam Lam deriving (Show, Eq)

data Set = Set Name Set | Nil deriving (Show, Eq)

add :: Name -> Set -> Set
add name Nil = Set name Nil
add name (Set x xs) = add name xs

join :: Set -> Set -> Set
join Nil y = y
join (Set x xs) y = Set x (join xs y)

remove :: Name -> Set -> Set
remove name Nil = Nil
remove name (Set x xs) = if name == x then xs else Set x (remove name xs)

free_variables :: Lam -> Set
free_variables (Var name) = Set name Nil
free_variables (Abs name lam) = remove name (free_variables lam)
free_variables (App lamx lamy) = join (free_variables lamx) (free_variables lamy)

is_closed :: Lam -> Bool
is_closed lam = if (free_variables lam) == Nil then True else False

subst :: Name -> Lam -> Lam -> Lam
subst name v (Var namex) = if name == namex then v else Var namex
subst name v (App lamx lamy) = App (subst name v lamx) (subst name v lamy)
subst name v (Abs param lam) = if name == param then Abs param lam else Abs param (subst name v lam)

eval :: Lam -> Lam
eval (App (Abs namex lamx) (Abs namey lamy)) = eval (subst namex lamy lamx)
eval (Abs name lam) = Abs name (eval lam)
eval (Var name) = Var name