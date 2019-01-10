------------- Q8
import Graphics.Gloss

main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))
--dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))

------------- Q1
alterne :: [a] -> [a]
alterne [] = []
alterne [a] = [a]
alterne (x:y:xs) = x:(alterne xs)

------------- Q2
combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine f [] _ = []
combine f _ [] = []
combine f (x1:xs1) (x2:xs2) = (f x1 x2):(combine f xs1 xs2)

------------- Q3
pasPascal :: [Integer] -> [Integer]
pasPascal [] = []
pasPascal [a] = [a]
pasPascal (x:y:xs) = ((+) x y):(pasPascal (y:xs))
triangle l = 1:(pasPascal l)

pasPascal' :: [Integer] -> [Integer]
pasPascal' [] = []
pasPascal' l = (zipWith (+) l (0:l))++[1]

------------- Q4
pascal :: [[Integer]]
pascal = iterate pasPascal' [1]

pascalDix = take 10 pascal

------------- Q5
pointAintercaler :: (Float,Float) -> (Float,Float) -> (Float,Float)
pointAintercaler (xa,ya) (xb,yb) = (((xa+xb)/2)+((yb-ya)/2),((ya+yb)/2)+((xa-xb)/2))

------------- Q6
pasDragon :: [(Float,Float)] -> [(Float,Float)]
pasDragon [x] = [x]
pasDragon [p1,p2] = [p1, (pointAintercaler p1 p2), p2]
pasDragon (p1:p2:p3:xs) = p1:(pointAintercaler p1 p2):p2:(pointAintercaler p3 p2):(pasDragon (p3:xs))

------------- Q7
dragon :: (Float,Float) -> (Float,Float) -> [[(Float,Float)]]
dragon p1 p2 = iterate pasDragon [p1,p2]

------------- Q8
dragonOrdre :: (Float,Float) -> (Float,Float) -> Int -> [(Float,Float)]
dragonOrdre p1 p2 0 = [p1,p2]
dragonOrdre p1 p2 n = (dragonOrdre p1 (pointAintercaler p1 p2) (n-1)) ++ (dragonOrdre p2 (pointAintercaler p1 p2) (n-1))
-- Il y a des lignes parasites. Je n'arrive pas à faire en sorte qu'elles disparaissent.