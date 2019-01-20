infixl 10 ..
(..) : a -> (a -> b) -> b
(..) a f = f a

infixl 10 $$
($$) : a -> (a -> b) -> b
($$) a f = f a

infixl 10 ^
(^) : a -> (a -> b) -> b
(^) a f = f a

inc: Num a => a -> a
inc a = a + 1

double: Num a => a -> a
double a = a * 2