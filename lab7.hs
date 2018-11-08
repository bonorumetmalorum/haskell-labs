i x = x

k x y = x

zero f x = x

one f x = f x

two f x = f(f x)

three f x = f(f(f x))

s x y z = x z (y z)

w x y = x y y

--d x y = x x y

newi = s k k

fib n x y = if n==1 then x else fib (n-1) (x+y) x

fib2 (n,x,y) = if n==1 then x else fib2 (n-1, x+y, x)
