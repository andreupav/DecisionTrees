------------------------------------
-----------DECISION--TREES----------
---Andreu--Pavia-Pardo,-39413034G---
------------------------------------

--definicio de l'arbre de decisio
--Fulles son sinonims d'String, Nodes son sinonim d'String i llista de tupla dels valors i arbre corresponent
data DTree a = Fulla ValorAtribut | Node NomAtribut [(ValorAtribut, DTree a)]
    deriving (Eq,Show) 

--definicio de tipus d'un diccionari  amb key, un caracter, i value una tupla d'enters
--les keys son els valors de cada atribut, i el valor son el nombre de comestibles/verinosos d'aquell valor
--de l'atribut.
type Dict = (Char -> (Int,Int))
create = const
search = ($)
insert dict key value x
    | key == x      = value
    | otherwise     = dict x

--Nom de l'atribut, sinonim d'String
type NomAtribut = String
--Valor de l'atribut, sinonim d'String
type ValorAtribut = String


main :: IO ()
main = do
    input <- readFile "agaricus-lepiota.data"
    let x = removeCommas (lines input)
    let atr = ["class", "cap-shape","cap-surface","cap-color","bruises","odor","gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape","stalk-root","stalk-surface-above-ring","stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring","veil-type","veil-color","ring-number","ring-type","spore-print-color","population","habitat"]
    let arbre = buildTree (transpose x) atr
    printArbre arbre
    askQuestion arbre

--Tractament de la data de l'input
removeCommas :: [String] -> [String]
removeCommas [] = []
removeCommas (x:xs) = (removeCommas' x):(removeCommas xs)

removeCommas' :: String -> String
removeCommas' xs = [ x | x <- xs, not (x `elem` ",.;'") ]

-------------------
---Classificacio---
-------------------

askQuestion :: DTree a -> IO()
askQuestion a = do 
    putStrLn $ "Which " ++ (getValueTree a) ++ "?"
    value <- getLine
    if isNode (recorrerArbre a value)
        then askQuestion (recorrerArbre a value)
        else putStrLn $ "Prediction: " ++ getValueTree (recorrerArbre a value)

--Obté el valor o atribut d'un arbre      
getValueTree :: DTree a -> String
getValueTree (Fulla val) = val
getValueTree (Node atr _) = atr

--Retorna true si l'arbre es un node
isNode :: DTree a -> Bool
isNode (Fulla a) = False
isNode (Node a x) = True

--Recorre l'arbre en preordre
recorrerArbre :: DTree a -> String -> DTree a
recorrerArbre (Fulla word) str = (Fulla word)
recorrerArbre (Node a []) str = (Fulla ("We don't know any mushrooms with this " ++ a ++ "."))
recorrerArbre (Node a (x:xs)) val
    | (fst x) == val = snd x 
    | otherwise = recorrerArbre (Node a xs) val

----------------------------
---Construccio de l'arbre---
----------------------------

--Funcions d'inmersió per generar l'arbre--
buildTree::  [String] -> [NomAtribut] -> DTree a 
buildTree x n = buildTree' x n (getIndexOfAtr x) (getVals x (getIndexOfAtr x) )

buildTree' ::[String] -> [NomAtribut] -> Int -> String -> DTree a
buildTree' x n k  vals = buildNode k n vals (getNewAtr k vals (transpose x))

--Obte els nous atributs amb els que es genera un node
getNewAtr :: Int -> String -> [String] -> [[String]]
getNewAtr k els x = dataSplit k els x

--Obte un set dels valors d'un atribut
getVals :: [String] -> Int -> String
getVals x k = (elems (x!!k))

--Obte l'index de l'atribut seleccionat com a node
getIndexOfAtr :: [String] -> Int
getIndexOfAtr x = ((maxIndex (maximum (getInfGain x)) (getInfGain x)) + 1)

--Obte la information gain del conjunt d'atributs
getInfGain :: [String] -> [Float]
getInfGain x = (calcInfGain (allEntropies (head x) (rm1elem x)) (entropyCalc (countRepeticions (head x)) (length (head x))))
----

--Crea un node amb nomAtribut a la posicio k, amb els valors apropiats
buildNode :: Int -> [NomAtribut] -> String -> [[String]] -> DTree a
buildNode k nom vals valsIn = Node (nom!!k) (buildValorsNode k nom vals valsIn)

--Obte els valors del node k
buildValorsNode :: Int -> [NomAtribut] -> String -> [[String]] -> [(ValorAtribut,DTree a)]
buildValorsNode _ _ [] [] = []
buildValorsNode k nom (c:cs) (x:xs) = ((chooseAtribute (nom!!k) c),(isFulla k x nom )):(buildValorsNode k nom cs xs)

--Genera fulla si tots els valors son d'una de les dues classes, o crea un nou node sino. 
isFulla :: Int -> [String] -> [NomAtribut] -> DTree a
isFulla k x y
    | allEqual ((transpose x)!!0) = Fulla (charToClass ((x!!0)!!0))
    | otherwise = buildTree (take k (transpose x) ++ drop (1 + k) (transpose x))  (take k y ++ drop (1 + k) y)

--Comprova si tots els caracters d'una string son iguals
allEqual :: String -> Bool 
allEqual xs = and $ map (== head xs) (tail xs)

--Genera una llista de les subllistes de valors corresponents
dataSplit :: Int -> String -> [String] -> [[String]]
dataSplit _ [] _ = []
dataSplit k (c:cs) x = (dataSplit' k c x):(dataSplit k cs x)

dataSplit' :: Int -> Char -> [String] -> [String] 
dataSplit' _ _ [] = []
dataSplit' k c (x:xs)
    | (x!!k) == c = (x):(dataSplit' k c xs)
    | otherwise = dataSplit' k c xs

---

--Calcula una llista de les entropies dels valors d'un conjunt d'atributs
allEntropies :: String -> [String] -> [Float]
allEntropies _ [] = []
allEntropies x (y:ys) = (entropyTotal (elems y) y (dictAtribut x y)):(allEntropies x ys)
         
--Calcula l'entropia d'un atribut      
entropyTotal :: String -> String -> Dict -> Float
entropyTotal [] _ _ = 0
entropyTotal (x:xs) f dict = (((countFrequencies f x)) * (entropyCalc (tup2list (search dict x)) (sum (tup2list (search dict x))))) + (entropyTotal xs f dict)

--Crea un diccionari amb key=valor unic d'atribut, valor=tupla amb el nombre de cada una de les classes del valor
dictAtribut :: String -> String -> Dict
dictAtribut x y = foldl insertDict (create(0,0)) (elems y)
    where insertDict dict key = insert dict key value
            where value = ( (countEdible ((transpose filesEliminades) !! 0)), (countPoisonous ((transpose filesEliminades) !! 0) ))
                    where filesEliminades = filter (\fila -> fila !! 1== key) (transpose (x:y:[]))

--Calcula la frequencia d'un valor
countFrequencies :: String -> Char -> Float
countFrequencies word c = (fromIntegral(length  (filter (== c) word )))/ (fromIntegral(length word))                    

--Calcula l'information gain d'una llista d'entropies
calcInfGain :: [Float] -> Float -> [Float]
calcInfGain [] _ = []
calcInfGain (x:xs) e = (e - x):(calcInfGain xs e)

--Compta el nombre de valors de classe = 'poisonous'
countPoisonous :: String -> Int
countPoisonous [] = 0
countPoisonous (x:xs)
    | x == 'p'  = countPoisonous xs +1
    | otherwise = countPoisonous xs

--Compta el nombre de valors de classe = 'edible'
countEdible :: String -> Int
countEdible [] = 0
countEdible (x:xs)
    | x == 'e'  = countEdible xs+1
    | otherwise = countEdible xs

--Compta el nombre de repeticions de caracters unics a una string
countRepeticions :: String -> [Int]
countRepeticions [] = []
countRepeticions word = [ c | x<-['A'..'z'], let c = (length . filter (==x)) word, c>0 ]

--Calcula l'entropia donana una llista de repeticions de caracters unics
entropyCalc :: [Int] -> Int -> Float
entropyCalc [] _ = 0
entropyCalc (x:xs) n
    |x == 0 = 0
    |otherwise = (-(( ( (fromIntegral x)/(fromIntegral n) ) * (logBase 2 ((fromIntegral x)/(fromIntegral n))) )) + (entropyCalc xs n))

---------------------
---Impressio arbre---
---------------------

--Funcions per canviar el caracter d'un valor al nom del valor apropiat i aixi imprimir l'arbre corresponent


--Donat un nom d'atribut i el caracter d'un valor, retorna el nom del valor
chooseAtribute :: NomAtribut -> Char -> ValorAtribut
chooseAtribute n x
    | n == "class" = charToClass x
    | n == "cap-shape" = charToCapShape x
    | n == "cap-color" = charToCapColor x
    | n == "gill-color" = charToGillColor x
    | n == "cap-surface" = charToCapSurface x
    | n == "bruises" = charToBruises x
    | n == "odor" = charToOdor x
    | n == "gill-attachment" = charToGillAttachment x
    | n == "gill-spacing" = charToGillSpacing x
    | n == "gill-size" = charToGillSize x
    | n == "stalk-shape" = charToStalkShape x
    | n == "stalk-root" = charToStalkRoot x
    | n == "stalk-surface-above-ring" = charToSsar x
    | n == "stalk-surface-below-ring" = charToSsbr x
    | n == "stalk-color-above-ring" = charToScar x
    | n == "stalk-color-below-ring" = charToScbr x
    | n == "veil-type" = charToVeilType x
    | n == "veil-color" =charToVeilColor x
    | n == "ring-number" =charToRingNumber x
    | n == "ring-type" =charToRingType x
    | n == "spore-print-color" =charToSporePrintColor x
    | n == "population" = charToPopulation x
    | n == "habitat" =charToHabitat x
    |otherwise = error $ unwords ["Unexpected class : ", n]    
           
charToClass :: Char -> ValorAtribut
charToClass c
    |c == 'p' = "poisonous"
    |c == 'e' = "edible"
    |otherwise = error $ unwords ["Unexpected class value :", [c]]
    
charToCapShape :: Char -> ValorAtribut
charToCapShape c
 | c == 'b' = "bell"
 | c == 'c' = "conical"
 | c == 'x' = "convex"
 | c == 'f' = "flat"
 | c == 'k' = "knobbed"
 | c == 's' = "sunken"
 | otherwise = error $ unwords ["Unexpected shape value :", [c]]
 
charToCapColor :: Char -> ValorAtribut
charToCapColor c
 |c == 'n'= "brown"
 |c == 'b'= "buff"
 |c == 'c'= "cinnamon"
 |c == 'g'= "gray"
 |c == 'r'= "green"
 |c == 'p'= "pink"
 |c == 'u'= "purple"
 |c == 'e'= "red"
 |c == 'w'= "white"
 |c == 'y'= "yellow"
 | otherwise = error $ unwords ["Unexpected capColor value :", [c]]
 
charToGillColor :: Char -> ValorAtribut
charToGillColor c
 |c == 'k' = "black"
 |c == 'n' = "brown"
 |c == 'b' = "buff"
 |c == 'h' = "chocolate"
 |c == 'g' = "gray"
 |c == 'r' = "green"
 |c == 'o' = "orange"
 |c == 'p' = "pink"
 |c == 'u' = "purple"
 |c == 'e' = "red"
 |c == 'w' = "white"
 |c == 'y' = "yellow"
 | otherwise = error $ unwords ["Unexpected gillColor value :", [c]]

charToCapSurface :: Char -> ValorAtribut
charToCapSurface c
  | c == 'f' = "fibrous"
  | c == 'g' = "grooves"
  | c == 'y' = "scaly"
  | c == 's' = "smooth"
  | otherwise = error $ unwords ["Unexpected cap-surface value :", [c]]

charToBruises :: Char -> ValorAtribut
charToBruises c
  | c==  't' = "True"
  |c == 'f' = "False"
  |otherwise = error $ unwords ["Unexpected bruises value :", [c]]


charToOdor :: Char -> ValorAtribut
charToOdor c
 |c == 'a' = "almond"
 |c == 'l' = "anise"
 |c == 'c' = "creosote"
 |c == 'y' = "fishy"
 |c == 'f' = "foul"
 |c == 'm' = "musty"
 |c == 'n' = "none"
 |c == 'p' = "pungent"
 |c == 's' = "spicy"
 |otherwise=  error $ unwords ["Unexpected odor value :", show [c]]
 
charToGillAttachment :: Char -> ValorAtribut
charToGillAttachment c
 |c == 'a' ="attached"
 |c == 'd' ="descending"
 |c == 'f' ="free"
 |c == 'n' ="notched"
 |otherwise = error $ unwords ["Unexpected gill-attachment value :", [c]]
  
charToGillSpacing :: Char -> ValorAtribut
charToGillSpacing c
 |c =='c' = "close"
 |c =='w' = "crowded"
 |c =='d' = "distant"
 |otherwise = error $ unwords ["Unexpected gill-spacing value :", [c]]

charToGillSize :: Char -> ValorAtribut
charToGillSize c
  | c == 'b' = "broad"
  | c == 'n' = "narrow"
  | otherwise = error $ unwords ["Unexpected gill-size value :", [c]]
 
charToStalkShape :: Char -> ValorAtribut
charToStalkShape c
  |c == 'e' = "enlarging"
  |c == 't' = "tapering"  
  | otherwise = error $ unwords ["Unexpected stalk-shape value :", [c]]

  
charToStalkRoot :: Char -> ValorAtribut
charToStalkRoot c
  | c == 'b' = "Bulbous"
  | c == 'c' = "Club"
  | c == 'u' = "Cup"
  | c == 'e' = "Equal"
  | c == 'z' = "Rhizomorphs"
  | c == 'r' = "Rooted"
  | c == '?' = "none"  
  | otherwise = error $ unwords ["Unexpected stalk-root value :", [c]]

charToSsar :: Char -> ValorAtribut
charToSsar c
  |c == 'f' = "fibrous"
  |c == 'y' = "scaly"
  |c == 'k' = "silky"
  |c == 's' = "smooth"
  | otherwise = error $ unwords ["Unexpected stalk-surface-above-ring value :", [c]]
  
charToSsbr :: Char -> ValorAtribut
charToSsbr c
  |c == 'f' ="fibrous"
  |c == 'y' ="scaly"
  |c == 'k' ="silky"
  |c == 's' ="smooth"
  | otherwise = error $ unwords ["Unexpected stalk-surface-below-ring value :", [c]]

charToScar :: Char -> ValorAtribut
charToScar c
  | c== 'n' = "brown"
  | c== 'b' = "buff"
  | c== 'c' = "cinnamon"
  | c== 'g' = "gray"
  | c== 'o' = "orange"
  | c== 'p' = "pink"
  | c== 'e' = "red"
  | c== 'w' = "white"
  | otherwise = error $ unwords ["Unexpected stalk-color-above-ring value :", [c]]
 
charToScbr :: Char -> ValorAtribut
charToScbr c
  |c=='n' = "brown"
  |c=='b' = "buff"
  |c=='c' = "cinnamon"
  |c=='g' = "gray"
  |c=='o' = "orange"
  |c=='p' = "pink"
  |c=='e' = "red"
  |c=='w' = "white"
  |c=='y' = "yellow"
  | otherwise = error $ unwords ["Unexpected stalk-color-below-ring value :", [c]]
  
charToVeilType :: Char -> ValorAtribut
charToVeilType c
  |c == 'p' = "partial"
  |c == 'u' = "universal"
  | otherwise = error $ unwords ["Unexpected veil-type value :", [c]]
  
charToVeilColor :: Char -> ValorAtribut
charToVeilColor c
  |c == 'n' = "brown"
  |c == 'o' = "orange"
  |c == 'w' = "white"
  |c == 'y' = "yellow"
  | otherwise = error $ unwords ["Unexpected veil-color value :", [c]]
  
charToRingNumber :: Char -> ValorAtribut
charToRingNumber c
  |c =='n' ="none"
  |c =='o' ="one"
  |c =='t' ="two"
  | otherwise = error $ unwords ["Unexpected ring-number value :", [c]]

charToRingType :: Char -> ValorAtribut
charToRingType c
  |c =='c'="cobwebby"
  |c =='e'="evanescent"
  |c =='f'="flaring"
  |c =='l'="large"
  |c =='n'="none"
  |c =='p'="pendant"
  |c =='s'="sheathing"
  |c =='z'="zone"
  | otherwise = error $ unwords ["Unexpected ring-type value :", [c]]
  
charToSporePrintColor :: Char -> ValorAtribut
charToSporePrintColor c
  |c=='k' ="black"
  |c=='n' ="brown"
  |c=='b' ="buff"
  |c=='h' ="chocolate"
  |c=='r' ="green"
  |c=='o' ="orange"
  |c=='u' ="purple"
  |c=='w' ="white"
  |c=='y' ="yellow"
  | otherwise = error $ unwords ["Unexpected spore-print-color value :", [c]]

charToPopulation :: Char -> ValorAtribut
charToPopulation c
  |c == 'a' = "abundant"
  |c == 'c' = "clustered"
  |c == 'n' = "numerous"
  |c == 's' = "scattered"
  |c == 'v' = "several"
  |c == 'y' = "solitary"  
  | otherwise = error $ unwords ["Unexpected population value :", [c]]
  
charToHabitat :: Char -> ValorAtribut
charToHabitat c
  |c == 'g' ="grasses"
  |c == 'l' ="leaves"
  |c == 'm' ="meadows"
  |c == 'p' ="paths"
  |c == 'u' ="urban"
  |c == 'w' ="waste"
  |c == 'd' ="woods"
  | otherwise = error $ unwords ["Unexpected habitat value :", [c]]
  
--Funcions per imprimir i representar l'arbre amb les tabulacions adients  
printArbre :: DTree a -> IO()
printArbre x = putStrLn $ printArbre' x 0

printArbre' ::  DTree a -> Int -> String
printArbre' (Fulla e) depth = (concatN depth "  ") ++ e ++ "\n"
printArbre' (Node e y) depth = (concatN depth "  ") ++ e ++ "\n"  ++ printArbre'' y (depth+1)
            
printArbre'' :: [(ValorAtribut,DTree a)] -> Int -> String
printArbre'' [] _ = ""
printArbre'' (x:xs) depth = (concatN depth "  ") ++ (fst x) ++ "\n"  ++ printArbre' (snd x) (depth+1) ++ printArbre'' xs depth
---

------------------------
---Funcions auxiliars---
------------------------
  
--Obte l'index del valor mes gran d'una llista  
maxIndex :: Float -> [Float] -> Int
maxIndex _ [] = error $ "out of bounds ¿?"
maxIndex _ [x] = 0
maxIndex v (x:xs)
    | v /= x = (maxIndex v xs) + 1
    |otherwise = maxIndex v [x]

--Funcio per replicar una String n cops
concatN :: Int -> String -> String
concatN n word = concat $ replicate n word

 
--Elimina el primer element d'una llista
rm1elem :: [a] -> [a]
rm1elem (x:xs) = xs

--Transposa una llista de llistes
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

--Obte una llista dels elements unics d'una llista
elems :: Eq a => [a] -> [a]
elems (x:xs) = x : elems (filter (/= x) xs)
elems [] = []

--Transforma una tupla de 2 elements a una llista d'aquests elements
tup2list :: (a,a) -> [a]
tup2list x = (fst x):(snd x):[]


