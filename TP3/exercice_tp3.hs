import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

------------- Q1
motSuivant :: Regles -> Mot -> Mot
motSuivant r [] = []
motSuivant r (x:xs) = (r x)++(motSuivant r xs)

motSuivant' r l = concat (map r l)

motSuivant'' r l = concatMap r l

------------- Q2
vonKoch1 'F' = "F-F++F-F"
vonKoch1  s  = [s]

------------- Q3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme a r = iterate (motSuivant r) a

-- On rajoute une variable qui permet de limiter le nombre de récursion.
lsysteme_limite a r n = take n (iterate (motSuivant r) a)

------------- Q4
type EtatTortue = (Point, Float) -- Point=cordonné de la tortue actuel | Float=son angle actuel
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

config :: Config
config = (((100,100),50),50,50,90,['F','+','-'])

etatInitial :: Config -> EtatTortue
etatInitial (e,_,_,_,_) = e

longueurPas :: Config -> Float
longueurPas (_,l,_,_,_) = l

facteurEchelle :: Config -> Float
facteurEchelle (_,_,f,_,_) = f

angleRotation :: Config -> Float
angleRotation (_,_,_,a,_) = a

symbolesComprisTortue :: Config -> [Symbole]
symbolesComprisTortue (_,_,_,_,ls) = ls

------------- Q5
avance :: Config -> EtatTortue -> EtatTortue
avance config ((x,y),angleActuel) = (((x + (longueurPas config) * (cos angleActuel)) , (y + (longueurPas config) * (sin angleActuel))) , angleActuel)

------------- Q6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche config ((x,y),angleActuel) = ((x,y),angleActuel+(angleRotation config))

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite config ((x,y),angleActuel) = ((x,y),angleActuel-(angleRotation config))

------------- Q7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue _ [] = []
filtreSymbolesTortue config (x:xs) = if (elementInList x (symbolesComprisTortue config)) then x:(filtreSymbolesTortue config xs) else filtreSymbolesTortue config xs

elementInList :: Eq a => a -> [a] -> Bool
elementInList m [] = False
elementInList m (x:xs) = if m==x then True else elementInList m xs

------------- Q8
type EtatDessin = (EtatTortue, Path) -- Path est un tableau de point qui représente tout les points dont la tortue est passé !

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole config (etatDessin,path) 'F' = (avance config etatDessin, (fst etatDessin):path)
interpreteSymbole config (etatDessin,path) '+' = (tourneAGauche config etatDessin, path)
interpreteSymbole config (etatDessin,path) '-' = (tourneADroite config etatDessin, path)
interpreteSymbole config (etatDessin,path) _ = (etatDessin, path)

------------- Q9
interpreteMot :: Config -> Mot -> Picture
interpreteMot config mot = Line(interpreteMot' config (filtreSymbolesTortue config mot) (etatInitial config, []))

interpreteMot' :: Config -> Mot -> EtatDessin -> Path
interpreteMot' _ [] ((positionActuel,_),path) = (positionActuel:path)
interpreteMot' config (x:xs) etatDessin = interpreteMot' config xs (interpreteSymbole config etatDessin x)

dessin = interpreteMot (((-150,0),0),10,1,pi/3,"F+-") "F+F--F+F"

--main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin

------------- Q10
-- Attention : fromIntegral permet de convertir un int en float.
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime lSys (e, longeurPas, facteurEchelle, a, ls) t =
  let i = round t `mod` 8 in
  interpreteMot (e, longeurPas * (facteurEchelle ** fromIntegral i), facteurEchelle, a, ls) (lSys !! i)
-- Attention : On multiplie la longueur d'un pas par le facteur d'echelle multiplié par l'etape en cours (le modulo). Cela permet que l'image soit toujours de la même taille et ne depace pas l'écran.

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch3 :: LSysteme
vonKoch3 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch3Anime :: Float -> Picture
vonKoch3Anime = lsystemeAnime vonKoch3 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white (vonKoch3Anime)