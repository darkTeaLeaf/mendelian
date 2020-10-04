{-# OPTIONS_GHC -Wall -fno-warn-type-defaults -fdefer-typed-holes #-}
{-# OPTIONS_GHC -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mendelian where

import Data.List
import Data.Char
--import Text.Read
import Data.Maybe

-- ##########################
-- DATA AND TYPES DECLARATION
-- ##########################

type Label = Char
type Trait = String
type TraitExpression = String
type IsDominant = Bool
type Ratio = Double

data Gen =  Gen Label Trait
  deriving (Show)

data Allele = Allele Gen IsDominant TraitExpression

data Genotype = Genotype [(Allele, Allele)]
  deriving (Eq, Ord, Show)

data Phenotype = Phenotype [Allele]

data Population = Population [(Genotype, Ratio)]
  deriving (Show)

data PopulationPhenotype = PopulationPhenotype [(Phenotype, Ratio)]

-- ####################
-- DATA CLASS INSTANCES
-- ####################

instance Eq Gen where
  (Gen label1 _) == (Gen label2 _) = label1 == label2
  
instance Ord Gen where
   compare (Gen l1 _) (Gen l2 _) = compare l1 l2

instance Eq Allele where
  (Allele gen1 isDominant1 _) == (Allele gen2 isDominant2 _) = 
      gen1 == gen2 && isDominant1 == isDominant2

instance Ord Allele where
  compare (Allele gen1 isDominant1 _) (Allele gen2 isDominant2 _) 
    | not (gen1 == gen2) = compare gen1 gen2
    | otherwise  = compare isDominant1 isDominant2

instance Show Allele where
  show (Allele (Gen l _) True tr) = show (toUpper l) ++ " " ++ tr
  show (Allele (Gen l _) False tr)= show (toLower l) ++ " " ++ tr

-- ###################
-- COMPUTE GENERATIONS
-- ###################

-- | Compute all possible children genotypes from given parents
computeOffsprings :: Genotype -> Genotype -> Population
computeOffsprings (Genotype parent1) (Genotype parent2) = Population(
  map (\(x, num) -> (x, fromIntegral(num)))
  $ count
  $ map (\a -> (Genotype a)) (generateCombinations possibleGens))
  
  where
    translate (x, y) = [x, y]

    possibleGens = map (\pairs -> map(\(a1, a2)-> ordered a1 a2) pairs)
        (zipWith generatePairs (map translate parent1) (map translate parent2))

    ordered a1 a2
      = case compare a1 a2 of 
        LT -> (a2, a1)
        _ -> (a1, a2)

-- | Generate all possible tuple combinations from different lists
generateCombinations :: [[(a, a)]] -> [[(a, a)]]
generateCombinations [] = [[]]
generateCombinations (x:xs) =  concat $ 
    map (\i -> map (\iTail -> i:iTail)(generateCombinations xs)) x

-- | Generate all possible pairs of elements from different lists
generatePairs :: [a] -> [a] -> [(a, a)]
generatePairs [] _ = []
generatePairs (x: xs) ys = (simpleGen x ys) ++ (generatePairs xs ys) 
 
-- | Generate all possible pairs with given element
simpleGen :: a -> [a] -> [(a, a)]
simpleGen _ [] = []
simpleGen a (x:xs) = (a, x) : simpleGen a xs

-- | Count and store the number of repeated elements
count :: Ord a => Eq a => [a] -> [(a, Int)] 
count = map (\xs@(x:_) -> (x, length xs)) . group . sort

--computeGeneration :: Population -> Population

--guessParentChildren :: Phenotype -> PopulationPhenotype -> Genotype

--guessParentsChildren :: PopulationPhenotype -> PopulationPhenotype -> Population

--genoToPheno :: Genotype -> Phenotype

--commandHandler :: IO()
-- look task manager lab 2-3


--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------
makeAlleleLbl :: Label -> IsDominant -> String
makeAlleleLbl c True = ([toUpper c, toUpper c]) ++ "/" ++ ([toUpper c, toLower c])
makeAlleleLbl c False = [toLower c, toLower c]

parseGene :: String -> Maybe Gen
parseGene input = makeGene lbl trait
  where
    lbl :: Maybe Char
    lbl = (listToMaybe (take 1 input))

    trait :: String
    trait = drop 2 input

    makeGene :: Maybe Char -> String -> Maybe Gen
    makeGene Nothing _ = Nothing
    makeGene _ "" = Nothing
    makeGene (Just l) t = Just (Gen (toLower l) t)

parseAllele :: [Gen] -> String -> Maybe Allele
parseAllele genes input = makeAllele (lookUpGene lbl) trait (checkIfDominant lbl) 
  where
    lbl :: Maybe Char
    lbl = (listToMaybe (take 1 input))

    trait :: String
    trait = drop 2 input

    checkIfDominant :: Maybe Label -> IsDominant
    checkIfDominant Nothing = False
    checkIfDominant (Just c) = isUpper c

    lookUpGene :: Maybe Label -> Maybe Gen
    lookUpGene Nothing = Nothing
    lookUpGene (Just l) = findGene l genes

    makeAllele :: Maybe Gen -> String -> IsDominant -> Maybe Allele
    makeAllele Nothing _ _ = Nothing
    makeAllele _ "" _ = Nothing
    makeAllele (Just l) t isDominant = Just (Allele l isDominant t)

data GenoBaseInputState = GenoBaseInputState [Gen] [Allele] Bool Genotype Genotype
  deriving (Show)    

glue :: Maybe a -> [a] -> [a]
glue Nothing lst = lst
glue (Just g) lst = g:lst

glueAlleles :: Maybe Allele -> Maybe Allele -> [Allele] -> [Allele]
glueAlleles Nothing Nothing lst = lst
glueAlleles Nothing (Just al) lst = al:lst
glueAlleles (Just al) Nothing lst = al:lst
glueAlleles (Just al) (Just ar) lst = [al, ar] ++ lst

findGene :: Label -> [Gen] -> Maybe Gen
findGene lbl genes = find cnd genes
  where
    cnd (Gen geneLabel _) = geneLabel == (toLower lbl) 

findAllele :: Label -> [Allele] -> Maybe Allele
findAllele lbl alleles = find cnd alleles
  where
    cnd (Allele (Gen geneLabel _) isDominant _) = (geneLabel == (toLower lbl)) && (isDominant == (isUpper lbl)) 

makeGenotype :: String -> [Allele] -> Maybe [(Allele, Allele)]
makeGenotype "" _ = Nothing
makeGenotype (a:b:rest) alleles = newAlleleLst (makeAllelePair (findAllele a alleles) (findAllele b alleles))
  where
    newAlleleLst :: Maybe [(Allele, Allele)] -> Maybe [(Allele, Allele)]
    newAlleleLst Nothing = Nothing
    newAlleleLst pair = (pickyGlue pair (makeGenotype rest alleles))

    makeAllelePair :: Maybe Allele -> Maybe Allele -> Maybe [(Allele, Allele)]
    makeAllelePair Nothing _ = Nothing
    makeAllelePair _ Nothing = Nothing
    makeAllelePair (Just a1) (Just a2) = Just [(a1, a2)]

pickyGlue :: Maybe [a] -> Maybe [a] -> Maybe [a]
pickyGlue Nothing Nothing = Nothing
pickyGlue Nothing l = l
pickyGlue l Nothing = l
pickyGlue (Just l1) (Just l2) = Just (l1 ++ l2)

data Result = Result String (Maybe GenoBaseInputState)

data Command
  = AddGene String
  | ShowGenoBase
  | AddAllele String
  | SetP1Geno String
  | SetP2Geno String
  | CalcOffsprings
  | Error String
  | Show
  | NOP
  | Exit
  deriving (Show)

addGene :: String -> GenoBaseInputState -> Result
addGene geneStr (GenoBaseInputState genes alleles _ g1 g2) = Result
  (ifSuccessThen newGBS "Gene added!" "Adding gene failed!")
  (Just newGBS)
  where
    parsedGene = parseGene geneStr
    newGenes = glue parsedGene genes

    newGBS :: GenoBaseInputState
    newGBS = maybeUpdateGBS parsedGene

    maybeUpdateGBS :: Maybe Gen -> GenoBaseInputState
    maybeUpdateGBS Nothing = (GenoBaseInputState genes alleles False g1 g2)
    maybeUpdateGBS (Just _) = (GenoBaseInputState newGenes alleles True g1 g2)

addAllele :: String -> GenoBaseInputState -> Result
addAllele alleleStr (GenoBaseInputState genes alleles _ g1 g2) = Result
  (ifSuccessThen newGBS "Allele added!" "Adding allele failed!")
  (Just newGBS)
  where
    parsedAllele = parseAllele genes alleleStr
    newAlleles = glue parsedAllele alleles

    newGBS :: GenoBaseInputState
    newGBS = maybeUpdateGBS parsedAllele

    maybeUpdateGBS :: Maybe Allele -> GenoBaseInputState
    maybeUpdateGBS Nothing = (GenoBaseInputState genes alleles False g1 g2)
    maybeUpdateGBS (Just _) = (GenoBaseInputState genes newAlleles True g1 g2)

setParentGeno :: String -> Int -> GenoBaseInputState -> Result
setParentGeno genoStr parentN gbs@(GenoBaseInputState genes alleles _ g1 g2) = 
  Result
  (ifSuccessThen newGBS ("Parent " ++ (show parentN) ++ "genotype set!") ("Setting parent " ++ (show parentN) ++ " genotype failed!"))
  (Just newGBS)
  where
    newGeno = makeGenotype genoStr alleles

    newGBS :: GenoBaseInputState
    newGBS = maybeUpdateGBS newGeno

    maybeUpdateGBS :: Maybe [(Allele, Allele)] -> GenoBaseInputState
    maybeUpdateGBS Nothing = (GenoBaseInputState genes alleles False g1 g2)
    maybeUpdateGBS (Just ng) = updateGenotype (Genotype ng) -- (GenoBaseInputState genes alleles True (Genotype newP1Geno) g2)

    updateGenotype genotype
      | parentN == 1 = GenoBaseInputState genes alleles True genotype g2
      | parentN == 2 = GenoBaseInputState genes alleles True g1 genotype
      | otherwise = gbs

calculateOffsprings :: GenoBaseInputState -> Result
calculateOffsprings gbs@(GenoBaseInputState _ _ _ g1 g2) = 
  Result
  (show (computeOffsprings g1 g2))
  (Just gbs)

ifNothingThen :: Maybe a -> b -> b -> b
ifNothingThen Nothing x _ = x
ifNothingThen _ _ x = x

ifSuccessThen :: GenoBaseInputState -> b -> b -> b
ifSuccessThen (GenoBaseInputState _ _ True _ _) x _ = x
ifSuccessThen (GenoBaseInputState _ _ False _ _) _ x = x

parseCommand :: String -> Command
parseCommand input =
  case words input of
    ["/exit"]   -> Exit
    ("/gene":_) -> AddGene (drop 6 input)
    ("/geno1":_) -> SetP1Geno (drop 7 input)
    ("/geno2":_) -> SetP2Geno (drop 7 input)
    ("/allele":_) -> AddAllele (drop 8 input)
    ["/offs"] -> CalcOffsprings
    ["/show"] -> Show
    [""] -> NOP -- ?????
    _      -> Error input

handleCommand :: Command -> (GenoBaseInputState -> Result)
handleCommand command =
  case command of
    Exit             -> (\_state -> Result "Bye!" Nothing)
    NOP              -> (\_state -> Result "" (Just _state))
    Error input       -> (\_state -> Result ("Unknown command '" ++ input ++ "'") (Just _state))
    Show -> (\_state -> Result (show _state) (Just _state))
    AddGene geneStr   -> addGene geneStr
    AddAllele alleleStr   -> addAllele alleleStr
    SetP1Geno genoStr -> setParentGeno genoStr 1
    SetP2Geno genoStr -> setParentGeno genoStr 2
    CalcOffsprings -> calculateOffsprings

executeCommand :: GenoBaseInputState -> (GenoBaseInputState -> Result) -> IO ()
executeCommand state handler =
  case handler state of
    Result msg newState -> do
      putStrLn msg
      case newState of
        Nothing               -> return ()
        Just actuallyNewState -> runWith actuallyNewState

runWith :: GenoBaseInputState -> IO ()
runWith state = do
  input <- getLine
  executeCommand state (handleCommand (parseCommand input))

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------

genoBaseInputIO :: String -> GenoBaseInputState -> (String -> GenoBaseInputState -> GenoBaseInputState) -> IO (GenoBaseInputState)
genoBaseInputIO input gbs updateGBS = do
  return (updateGBS input gbs)

run :: IO ()
run = runWith (GenoBaseInputState [] [] True (Genotype []) (Genotype []))
  {-let a = Gen 'a' "color"
  let b = Gen 'b' "smoothness"
  let dad = Genotype [((Allele a True "green"), (Allele a True "green")), ((Allele b True "smooth"), (Allele b True "smooth"))]
  let mom = Genotype [((Allele a True "green"), (Allele a False "yellow")), ((Allele b False "wrinkle"), (Allele b False "wrinkle"))]
  print (computeOffsprings dad mom) -}
  --putStrLn "Please input the number of genes: "
  
