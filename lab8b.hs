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
subst name (Var namex) lamy = if name == namex then Var name else lamy
subst name (Abs namex lam) substlam  = if name == namex then Abs namex lam else Abs namex (subst name lam substlam)
subst name (App lamx lamy) substlam = App (subst name lamx substlam) (subst name lamx lamy)
