import Test.QuickCheck
import Control.Concurrent (threadDelay)

data Couleur = R | N deriving Show

------------ Q1
-- Un arbre est soit une feuille, soit un noeud avec deux arbres.
-- Une feuille c'est un élément null qui indique juste la fin.
-- Un noeud possède une couleur et une valeur et qui possède deux fils qui sont deux Arbres.
data Arbre couleur valeur = Feuille | Noeud couleur valeur (Arbre couleur valeur) (Arbre couleur valeur) deriving Show

------------ Q2
-- La fonction map' prend une fonction, et applique cette fonction à tout les valeurs des noeuds de l'arbre.
map' :: (t -> valeur) -> Arbre couleur t -> Arbre couleur valeur
map' _ Feuille =  Feuille
map' fonction (Noeud couleur valeur gauche droite) = (Noeud couleur (fonction valeur) (map' fonction gauche) (map' fonction droite))

-- La fonction fold' prend une fonction et l'applique à tout les arbres.
fold' :: (t1 -> t2 -> t2 -> t2) -> t2 -> Arbre t t1 -> t2
fold' _ elementNeutre Feuille = elementNeutre
fold' fonction elementNeutre (Noeud _ valeur gauche droite) = fonction valeur (fold' fonction elementNeutre gauche) (fold' fonction elementNeutre droite)

------------ Q3
-- arbreTest = (Noeud R 0 (Noeud R 0 Feuille (Noeud R 0 (Noeud R 0 Feuille Feuille) Feuille))(Noeud R 0 Feuille (Noeud R 0 Feuille Feuille))) -- Arbre de test
arbreTest = (Noeud R 0 (Noeud R 1 Feuille (Noeud R 2 (Noeud R 3 Feuille Feuille) Feuille))(Noeud R 4 Feuille (Noeud R 5 Feuille Feuille))) -- Arbre de test

-- Pour hauteur
hauteur arbre = fold' f 0 arbre where f valeur resultGauche resultDroite = 1 + (max resultGauche resultDroite)

hauteur' Feuille = 0
hauteur' (Noeud couleur valeur gauche droite) =  1 + (max (hauteur' gauche) (hauteur' droite))

-- Pour taille
taille arbre = fold' f 0 arbre where f valeur resultGauche resultDroite = 1 + resultGauche + resultDroite

taille' Feuille = 0
taille' (Noeud couleur valeur gauche droite) =  1 + (taille' gauche) + (taille' droite)

------------ Q4
peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche ((couleur,valeur):xs) = (Noeud couleur valeur (peigneGauche xs) Feuille)

------------ Q5
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)
-- elle vérifie si la hauteur du peigneGauche est égale à la longueur du tableau passé en argument.

------------ Q6
prop_taillePeigne xs = length xs == taille (peigneGauche xs)

------------ Q7
estComplet :: Arbre c a -> Bool
estComplet Feuille = True
estComplet (Noeud _ _ gauche droite) = (estComplet gauche) && (estComplet droite) && ((hauteur gauche) == (hauteur droite)) 

arbreTestCompletDeux = complet 2 [(R,'a'),(N,'b'),(N,'c')]
arbreTestCompletTrois = complet 3 [(R,'a'),(N,'b'),(R,'c'),(N,'d'),(R,'e'),(N,'f'),(R,'g')]
------------ Q8
-- Les arbres à gauche complet sont les arbres ou quand il y a un fils gauche, il y a obligatoirement un fils droit.

------------ Q9
complet :: Int -> [(c, a)] -> Arbre c a
complet 0 [] = Feuille
complet h ls = Noeud (fst ((!!) ls (milieu ls))) (snd ((!!) ls (milieu ls))) (complet (h-1) (take (milieu ls) ls)) (complet (h-1) (drop ((milieu ls)+1) ls))
	where milieu ls = (length ls) `div` 2

------------ Q10
-- Il s'agit de la fonction repeat.

repeat' :: a -> [a]
repeat' a = iterate (\x -> x) a

------------ Q11
listCaractere = map (\x -> ((),x)) ['a'..'z']

------------ Q12
aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []
aplatit (Noeud couleur valeur gauche droite) = (aplatit gauche) ++ [(couleur, valeur)] ++ (aplatit droite)

------------ Q13
element :: Eq a => a -> Arbre c a -> Bool
element valeur Feuille = False
element val (Noeud _ valeur gauche droite) = val == valeur || element val gauche || element val droite

------------ Q14
valeurToString :: Char -> String	-- On considère que les valeurs sont toujours des caractères (a,b,c,..)
valeurToString c = [c]

couleurToString :: Couleur -> String
couleurToString R = "red"
couleurToString N = "black"

noeudToString :: (c -> String) -> (a -> String) -> (c, a) -> String
noeudToString fonctionCouleur fonctionValeur (couleur, valeur) = fonctionValeur valeur ++ " [color=" ++ fonctionCouleur couleur ++ ", fontcolor=" ++ fonctionCouleur couleur ++ "]"

------------ Q15
getValeurNoeud :: Arbre c a -> a
getValeurNoeud (Noeud _ valeur _ _) = valeur

arcs :: Arbre c a -> [(a,a)]
arcs Feuille = []
arcs (Noeud couleur valeur Feuille Feuille) = []
arcs (Noeud couleur valeur gauche Feuille) = (valeur, getValeurNoeud(gauche)):arcs gauche
arcs (Noeud couleur valeur Feuille droite) = (valeur, getValeurNoeud(droite)):arcs droite
arcs (Noeud couleur valeur gauche droite) = [(valeur, getValeurNoeud(gauche))] ++ [(valeur, getValeurNoeud(droite))] ++ arcs gauche ++ arcs droite

------------ Q16
arcToString :: (a -> String) -> (a,a) -> String
arcToString fonctionValeur (valeur1, valeur2) = fonctionValeur valeur1 ++ " -> " ++ fonctionValeur valeur2

------------ Q17
noeudsToString :: (c -> String) -> (a -> String) -> [(c, a)] -> String
noeudsToString fonctionCouleur fonctionValeur ln = unlines (map (noeudToString fonctionCouleur fonctionValeur) ln)

dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise nom fonctionCouleur fonctionValeur arbre = "digraph \"" ++ nom ++ " {\n node [fontname=\"DejaVu-Sans\", shape=circle] \n \n" ++ (noeudsToString fonctionCouleur fonctionValeur (aplatit arbre) ) ++ "\n" ++ (unlines (map (arcToString fonctionValeur) (arcs arbre))) ++ "}"

testDotise = putStr(dotise "test" couleurToString valeurToString arbreTestCompletDeux)

------------ Q18
elementR :: Ord a => a -> Arbre c a -> Bool
elementR _ Feuille = False
elementR valeurR (Noeud _ valeur gauche droite) | valeurR < valeur = elementR valeurR gauche
												| valeurR > valeur = elementR valeurR droite
												| otherwise = True
												
------------ Q19
-- Initialisé au départ.
-- data Couleur = R | N

------------ Q20
equilibre :: Arbre Couleur valeur -> Arbre Couleur valeur
equilibre (Noeud N z (Noeud R y (Noeud R x a b) c ) d) = (Noeud R y (Noeud N x a b) (Noeud N z c d))
equilibre (Noeud N z (Noeud R x a (Noeud R y b c)) d) = (Noeud R y (Noeud N x a b) (Noeud N z c d))
equilibre (Noeud N x a (Noeud R z (Noeud R y b c) d)) = (Noeud R y (Noeud N x a b) (Noeud N z c d))
equilibre (Noeud N x a (Noeud R y b (Noeud R z c d))) = (Noeud R y (Noeud N x a b) (Noeud N z c d))
equilibre (Noeud couleur valeur gauche droite) =  (Noeud couleur valeur gauche droite)

------------ Q21
insert :: Ord a => a -> Arbre Couleur a -> Arbre Couleur a
insert valeur Feuille = Noeud R valeur Feuille Feuille
insert newValeur (Noeud R valeur gauche droite) = insert newValeur (Noeud N valeur gauche droite)
insert newValeur (Noeud couleur valeur gauche droite) 	| elementR newValeur (Noeud couleur valeur gauche droite) = (Noeud couleur valeur gauche droite)
														| newValeur < valeur = equilibre (Noeud couleur valeur (insert newValeur gauche) droite)
														| newValeur > valeur = equilibre (Noeud couleur valeur gauche (insert newValeur droite))

------------ Q23														
--arbresDot :: [a] -> [String]

--main = mapM_ ecrit arbres
--    where ecrit a = do writeFile "arbre.dot" a
--                       threadDelay 1000000
--          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"