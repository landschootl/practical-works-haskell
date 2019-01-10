------------- Q3
sommeDeXaY_1::Int -> Int -> Int
sommeDeXaY_1 x y = sum [x..y]

sommeDeXaY_2 x y = if x > y then 0 else x + sommeDeXaY_2 (x+1) y

------------- Q4
somme :: [Int] -> Int
somme [] = 0
somme (x:xs) = x + somme xs

------------- Q5
-- Pour last
last_1::[a]->a
last_1 [x] = x
last_1 (_:xs) = last_1 xs

last_2 ls = head (reverse ls)

last_3 [x] = x
last_3 ls = last_3 (tail ls)

last_4 ls = ls !! ((length ls)-1)

-- Pour init
init_1::[a]->[a]
init_1 ls = reverse (tail (reverse ls))

init_2 [x] = []
init_2 (x:xs) = x:(init_2 xs)

init_3 ls = take ((length ls)-1) ls

------------- Q6
-- !!
(!!!)::[a]->Int->a
(!!!) ls n = head (drop n ls)

(!!!!) (x:xs) n = if n==0 then x else (!!!!) xs (n-1)

-- ++
(+++)::[a]->[a]->[a]
(+++) [] ls2 = ls2
(+++) ls1 ls2 = (+++) (init ls1) ((last ls1):ls2)

(++++) [] ls = ls
(++++) (x:xs) ls = x:((++++) xs ls)

-- concat
concat'::[[a]]->[a]
concat' [[]] = []
concat' [] = []
concat' (x:xs) = x ++ concat'(xs)

-- map
map'::(a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

------------- Q7
-- On déclare une fonction 'x' qui attend un argument de type Int et qui renvoi l'élément x de la liste l.

------------- Q8
longueur::[Int] -> Int
longueur [] = 0
longueur (xs) = somme (map' ((^) 1) xs)

------------- Q9
apply_1::(a->a) -> a -> Int -> [a]
apply_1 f x 0 = [x]
apply_1 f x n = x : apply_1 f ((f) x) (n-1)

apply_2::(a->a) -> a -> Int -> [a]
apply_2 f x n = take (n+1) (iterate f x)

------------- Q10
consecutif n = apply_1 ((+) 1) 0 n