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
-- | State of the user input.
data InputState = InputState {
    allGenes :: [Gen],        -- ^ All known genes.
    allAlleles :: [Allele],   -- ^ All known genes.
    lastOpSuccessful :: Bool, -- ^ Whether the last command finished ok.
    p1Geno :: Genotype,       -- ^ Genotype of parent 1.
    p2Geno :: Genotype        -- ^ Genotype of parent 2.
  }
  deriving (Show)    

data Command
  = AddGene String
  | ShowGenoBase
  | AddAllele String
  | SetParentGeno Int String
  | CalcOffsprings
  | Error String
  | Show
  | Exit
  deriving (Show)

data Result = Result String (Maybe InputState)

-- | Make Gen from string "B trait description".
parseGene 
  :: String -- ^ Given string.
  -> InputState
  -> Maybe Gen
parseGene input _ = makeGene lbl trait
  where
    lbl = (listToMaybe (take 1 input))
    trait = drop 2 input
    
    makeGene Nothing _ = Nothing
    makeGene _ "" = Nothing
    makeGene (Just l) t = Just (Gen (toLower l) t)

-- | Make Allele from string "b trait expression".
parseAllele 
  :: String 
  -> InputState  -- ^ List of known genes.
  -> Maybe Allele
parseAllele input (InputState genes _ _ _ _)  = makeAllele (lookUpGene lbl) trait (checkIfDominant lbl) 
  where
    lbl = (listToMaybe (take 1 input))
    trait = drop 2 input

    checkIfDominant Nothing = False
    checkIfDominant (Just c) = isUpper c

    lookUpGene Nothing = Nothing
    lookUpGene (Just l) = findGene l genes

    makeAllele Nothing _ _ = Nothing
    makeAllele _ "" _ = Nothing
    makeAllele (Just l) t isDominant = Just (Allele l isDominant t)

-- | Add to a list if element is not Nothing.
glue :: Maybe a -> [a] -> [a]
glue Nothing lst = lst
glue (Just g) lst = g:lst

-- | Look for the gene with the specified label.
findGene 
  :: Label -- ^ Specified label.
  -> [Gen] -- ^ List of genes.
  -> Maybe Gen
findGene lbl genes = find cnd genes
  where
    cnd (Gen geneLabel _) = geneLabel == (toLower lbl) 

-- | Look for the allele associated with the gene with the specified label.
-- Label is case-sensitive and represents the dominance of the allele.
findAllele 
  :: Label    -- ^ Specified label. Case-sensitive.
  -> [Allele] -- ^ List of alleles.
  -> Maybe Allele
findAllele lbl alleles = find cnd alleles
  where
    cnd (Allele (Gen geneLabel _) isDominant _) = (geneLabel == (toLower lbl)) && (isDominant == (isUpper lbl)) 

-- | Create pairs of alleles based on the given genotype string.
-- Examples: "AABb" / "Aabb".
-- Returns Nothing if at least one allele is not defined in the given list.
parseGenotype 
  :: String -- ^ Given genotype string.
  -> InputState -- ^ 
  -> Maybe [(Allele, Allele)]
parseGenotype "" _ = Nothing
parseGenotype (a:b:rest) gbs@(InputState _ alleles _ _ _) = newAlleleLst (makeAllelePair (findAllele a alleles) (findAllele b alleles))
  where
    newAlleleLst Nothing = Nothing
    newAlleleLst pair = (pickyGlue pair (parseGenotype rest gbs))

    makeAllelePair Nothing _ = Nothing
    makeAllelePair _ Nothing = Nothing
    makeAllelePair (Just a1) (Just a2) = Just [(a1, a2)]

-- | Concatenate lists. Ignore Nothings.
pickyGlue :: Maybe [a] -> Maybe [a] -> Maybe [a]
pickyGlue Nothing Nothing = Nothing
pickyGlue Nothing l = l
pickyGlue l Nothing = l
pickyGlue (Just l1) (Just l2) = Just (l1 ++ l2)

-- |
updateState 
  :: String                                -- ^ What to parse
  -> (String -> InputState -> Maybe a)     -- ^ How to parse
  -> (Maybe a -> InputState -> InputState) -- ^ How to update the state
  -> (String, String)                      -- ^ Success/Fail messages
  -> InputState                            -- ^ Current input state
  -> Result
updateState str parseStr maybeUpdateState (successMsg, failMsg) gbs = 
  Result (ifSuccessThen newGBS successMsg failMsg)
  (Just newGBS)
  where
    parsed = parseStr str gbs
    newGBS = maybeUpdateState parsed gbs

updateGene :: Maybe Gen -> InputState -> InputState
updateGene Nothing (InputState genes alleles _ g1 g2) = 
                                            InputState genes alleles False g1 g2
updateGene gene (InputState genes alleles _ g1 g2) = 
                                          InputState newGenes alleles True g1 g2
  where
    newGenes = glue gene genes

updateAllele :: Maybe Allele -> InputState -> InputState
updateAllele Nothing (InputState genes alleles _ g1 g2) = 
                                            InputState genes alleles False g1 g2
updateAllele allele (InputState genes alleles _ g1 g2) = 
                                          InputState genes newAlleles True g1 g2
  where
    newAlleles = glue allele alleles

updateGenotype :: Int -> Maybe [(Allele, Allele)] -> InputState -> InputState
updateGenotype _ Nothing (InputState genes alleles _ g1 g2) = 
                                            InputState genes alleles False g1 g2
updateGenotype parentN (Just genotype) (InputState genes alleles _ g1 g2) 
  | parentN == 1 = InputState genes alleles True (Genotype genotype) g2
  | parentN == 2 = InputState genes alleles True g1 (Genotype genotype)
  | otherwise = InputState genes alleles False g1 g2

calculateOffsprings :: InputState -> Result
calculateOffsprings gbs@(InputState _ _ _ g1 g2) = 
  Result
  (show (computeOffsprings g1 g2))
  (Just gbs)

ifSuccessThen :: InputState -> b -> b -> b
ifSuccessThen (InputState _ _ True _ _) x _ = x
ifSuccessThen (InputState _ _ False _ _) _ x = x

parseCommand :: String -> Command
parseCommand input =
  case words input of
    ["/exit"]   -> Exit
    ("/gene":_) -> AddGene (drop 6 input)
    ("/geno1":_) -> SetParentGeno 1 (drop 7 input)
    ("/geno2":_) -> SetParentGeno 2 (drop 7 input)
    ("/allele":_) -> AddAllele (drop 8 input)
    ["/offs"] -> CalcOffsprings
    ["/show"] -> Show
    _      -> Error input

handleTypo :: String -> InputState -> Result
handleTypo input state =  Result (msg input) (Just state)
  where
    msg "" = "\r"
    msg str = "Unknown command '" ++ str ++ "'"

handleCommand :: Command -> (InputState -> Result)
handleCommand command =
  case command of
    Exit                -> (\_state -> Result "Bye!" Nothing)
    Error input         -> handleTypo input
    Show                -> (\_state -> Result (show _state) (Just _state))
    AddGene geneStr     -> updateState geneStr parseGene updateGene 
                                     ("Gene added!", "Adding gene failed!")
    AddAllele alleleStr -> updateState alleleStr parseAllele updateAllele 
                                     ("Allele added!", "Adding allele failed!")
    SetParentGeno n genoStr -> updateState genoStr parseGenotype (updateGenotype n) 
                                     ("Parent " ++ (show n) ++ " genotype set!", 
                                      "Setting parent " ++ (show n) ++ " genotype failed!")
    CalcOffsprings      -> calculateOffsprings

executeCommand :: InputState -> (InputState -> Result) -> IO ()
executeCommand state handler =
  case handler state of
    Result msg newState -> do
      putStrLn msg
      case newState of
        Nothing               -> return ()
        Just actuallyNewState -> runWith actuallyNewState

runWith :: InputState -> IO ()
runWith state = do
  input <- getLine
  executeCommand state (handleCommand (parseCommand input))

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------

run :: IO ()
run = runWith (InputState [] [] True (Genotype []) (Genotype []))
  {-let a = Gen 'a' "color"
  let b = Gen 'b' "smoothness"
  let dad = Genotype [((Allele a True "green"), (Allele a True "green")), ((Allele b True "smooth"), (Allele b True "smooth"))]
  let mom = Genotype [((Allele a True "green"), (Allele a False "yellow")), ((Allele b False "wrinkle"), (Allele b False "wrinkle"))]
  print (computeOffsprings dad mom) -}
  --putStrLn "Please input the number of genes: "
  
