double x = x + x

quad x = double(double(x))

square x = x * x

i x = x

firstone x y = x

secondone x y = y

fortytwo x = 42

infinity = infinity + 1

apply x y = x(y)

twice x y = x(x(y))

fibonacci :: (Integral x) => x -> x
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x-2) + fibonacci(x-1)
