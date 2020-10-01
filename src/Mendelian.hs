module Mendelian where

import Data.List
import Data.Char

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
  show (Allele (Gen l _) True _) = show (toUpper l)
  show (Allele (Gen l _) False _)= show (toLower l)

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

run :: IO ()
run = do
  let a = Gen 'a' "color"
  let b = Gen 'b' "smoothness"
  let dad = Genotype [((Allele a True "green"), (Allele a True "green")), ((Allele b True "smooth"), (Allele b True "smooth"))]
  let mom = Genotype [((Allele a True "green"), (Allele a False "yellow")), ((Allele b False "wrinkle"), (Allele b False "wrinkle"))]
  print (computeOffsprings dad mom)