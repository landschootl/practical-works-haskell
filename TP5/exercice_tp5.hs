--TP Interprète - Quentin GUILLEMINOT

import Parser
import Data.Char
import Data.Maybe

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)

--Pour la question 14
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA) 

instance Show ValeurA where
    show (VFonctionA _) = "λ"
    show (VLitteralA (Entier x)) = show x
    show (VLitteralA (Bool b)) = show b

type Environnement a = [(Nom, a)]

--Question 1 : Parser supprimant les espaces de début.

espacesP :: Parser ()
espacesP = do zeroOuPlus ( carCond ( `elem` " \t")) 
              return ()

--Question 2 : Parser analysant le premier nom?

nomP :: Parser Nom
nomP = do s <- unOuPlus ( carCond (`elem` ['A'..'Z']++['a'..'z']))
	  espacesP
	  return s

--Question 3 : Parser de variables (noms)

varP :: Parser Expression
varP = do n <- nomP
	  return (Var n)

--Question 4 : Applicateur d'expressions
applique :: [Expression] -> Expression
applique [e] = e
applique [e1, e2] = App e1 e2
applique l = (App(applique deb) (last l)) 
		where deb = (init l)

--Test de l'applicateur
testApplique = applique [(Var "un"),(Var "deux"),(Var "trois"),(Var "quatre"),(Var "cinq"),(Var "six"),(Var "sept")]

--Question 5 : definition des types d'expression qu'on peut rencontrer
uneExpP = lambdaP ||| exprParentheseeP ||| boolP ||| nombreP ||| varP
testChaine = "aaaa   a"

-- Premiere version du exprP, celle qui ne prends que varP
--exprP :: Parser Expression
--exprP = do n <- varP
--	   return n

--Parser d'expressions multiples se basant sur la question 7
exprsP :: Parser Expression
exprsP = do espacesP
            e <- unOuPlus ( exprP )
	    return (applique e)

--Question 6 : Parser de lambda (formés ainsi : "λx -> x")
lambdaP :: Parser Expression
lambdaP = do car 'λ'
             espacesP 
             v <- nomP
             chaine "->"
             espacesP
	     e <- exprsP 
             return (Lam v e)

--Question 7 : Parser d'expression basé sur le tableau de type d'expressions
exprP :: Parser Expression
exprP = uneExpP

--Question 8 : Parser d'expression parenthésées (une expression entre les caractères ( et ) )
exprParentheseeP :: Parser Expression
exprParentheseeP = do car '('
                      y <- exprsP
                      car ')'
	              espacesP
		      return y 

--Testeur d'expressions parenthésées
testExPar = runParser exprP "(a (a (a (a))))"

--Question 9 : Parser 
nombreP :: Parser Expression
nombreP = do unp <- (unOuPlus (carCond(`elem` ['0'..'9'])))
             espacesP
	     return (Lit(Entier (toNum unp)))

--EDIT : Tout ceci aurait pu se faire avec un read, mais vu que je suis du 
--genre a m'enquiquiner la vie, voila mon nombreP basé sur un toNum fait main
toNum :: [Char] -> Integer
toNum [] = 0
toNum l@(x:xs) = (toInteger((ord x)-48))*(10 ^ ((length l)-1)) +(toNum xs)

testN = runParser nombreP "111 22"

--Question 10 : Parser de boolees renvoyant des expressions contenant des Littéraux
boolP :: Parser Expression
boolP = (do chaine "True"
            espacesP
	    return (Lit(Bool (True)))
            ) |||
	(do chaine "False"
            espacesP
	    return (Lit(Bool (False)))
            )

--Question 11 
--Fait précédemment avec le tableau uneExpP contenant les types d'expressions

--Question 12 Analyseur de chaine qui renvoi une expression
ras :: String -> Expression
ras s = if((isNothing resul))||(length(snd(fromJust resul))>0)
        then error "Erreur d’analyse syntaxique"
	else fst(fromJust resul)
	where resul = (runParser exprsP s)

--Question 13 : L'acces a la valeur de notre Litteral n'est pas defini, show ne peut donc s'appliquer

--Question 14 : Voir en haut avec les déclarations des types/data 

--Question 15 : Interpreteur d'expression en fonction du contenu de l'environnement

interpreteA :: Environnement ValeurA ->Expression ->ValeurA
interpreteA env (Lit litteral) = VLitteralA litteral
interpreteA env (Var var) = fromJust (lookup var env)
interpreteA env (Lam nom exp) = VFonctionA (\lambda -> interpreteA ((nom,lambda):env) exp )
interpreteA env (App exp1 exp2) = let (VFonctionA lambda) = (interpreteA env exp1) in lambda (interpreteA env exp2)

--Question 16 : Intégration des nombres négatifs

negA :: ValeurA
negA = VFonctionA(\ (VLitteralA (Entier (x))) -> (VLitteralA (Entier (-x))))

--Question 17 : Intégration des additions

addA :: ValeurA
addA = VFonctionA (\ (VLitteralA (Entier x)) -> VFonctionA (\ (VLitteralA (Entier y)) -> VLitteralA (Entier (x+y))))
 
--Question 18 : Fonction permettant la création d'une VFonctionA de type :
-- (\x -> \y -> x op y) soit l'application de la fonction op aux deux arguments x et y
-- qui seraient liés à deux lambdas.
releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA op = VFonctionA (\ (VLitteralA (Entier x)) -> VFonctionA (\ (VLitteralA (Entier y)) -> (VLitteralA (Entier (op x y))) ))

--Ci dessous l'environnement permettant les calculs d'opérateurs
envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot) ]

--Question 19
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\ (VLitteralA (Bool x)) -> VFonctionA (\ (VLitteralA (Entier y)) -> VFonctionA (\ (VLitteralA (Entier z)) -> if x then (VLitteralA (Entier y)) else (VLitteralA (Entier z)))))

--Question 20 : Interprete pourri <3
main :: IO()
main = do putStr "int >"
	  l <- getLine
	  print $ interpreteA envA (ras l)
	  main
