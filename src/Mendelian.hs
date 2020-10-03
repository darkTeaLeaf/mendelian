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
  deriving (Eq, Ord)

data Phenotype = Phenotype [Allele]

data Population = Population [(Genotype, Ratio)]

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

instance Show Phenotype where
  show (Phenotype phenotype) = "\nPhenotype: " 
                            ++ alleleString phenotype 
                            ++ generPhenoString phenotype
    where
      generPhenoString :: [Allele] -> [Char]
      generPhenoString [] = ""
      generPhenoString ((Allele (Gen l trait) dom expr) : rest) = "\n" 
              ++ [changeCase l dom] ++ ": " ++ trait ++ " " 
              ++ expr ++ generPhenoString rest

instance Show Genotype where
  show (Genotype genotype) = "\nGenotype: " ++ genotypeString genotype
    where
      genotypeString :: [(Allele, Allele)] -> String
      genotypeString genot = alleleString (flatten genot)

      flatten :: [(Allele, Allele)] -> [Allele]
      flatten [] = []
      flatten ((allele1, allele2) : rest) = allele1 : allele2 : flatten rest 


alleleString :: [Allele] -> [Char]
alleleString [] = ""
alleleString ((Allele (Gen l _) dom _) : rest) = [changeCase l dom] 
                                              ++ alleleString rest

changeCase :: Char -> Bool -> Char
changeCase c isDom
  | isDom    = toUpper c
  | otherwise = toLower c

instance Show Population where
  show (Population population) = "\n--Population description--\n" 
                              ++ speciesDescrip population
    where
      speciesDescrip :: [(Genotype, Ratio)] -> String
      speciesDescrip [] = ""
      speciesDescrip ((genotype, ratio) : rest) = show genotype
        ++ "\nGenopype ratio: " ++ show ratio
        ++ show (genoToPheno genotype) ++ "\n" ++ speciesDescrip rest


-- ###################
-- COMPUTE GENERATIONS
-- ###################

-- | Compute all possible children genotypes from given parents
computeOffsprings :: Genotype -> Genotype -> Population
computeOffsprings (Genotype parent1) (Genotype parent2) = Population(
  numberToRatio
  $ count
  $ map (\a -> (Genotype a)) (generateCombinations possibleGens))
  
  where
    translate (x, y) = [x, y]

    possibleGens = map (\pairs -> map(\(a1, a2)-> ordered a1 a2) pairs)
        (zipWith generatePairs (map translate parent1) (map translate parent2))

    ordered a1 a2
      = case compare a1 a2 of 
        LT -> (a2, a1)
        _  -> (a1, a2)

    numberToRatio arr = map(\(x, num) -> (x, ratio num)) arr
      where 
        ratio num = (fromIntegral num) / (fromIntegral (sum (map snd arr)))

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

--commandHandler :: IO()
-- look task manager lab 2-3


isDominant :: Allele -> Bool
isDominant (Allele (Gen _ _) dom _)  = dom

getDominant :: (Allele, Allele) -> Allele
getDominant (allele1, allele2) 
  | isDominant allele1 = allele1
  | otherwise = allele2

genoToPheno :: Genotype -> Phenotype
genoToPheno (Genotype genotype) = Phenotype (generPheno genotype)
  where
    generPheno :: [(Allele, Allele)] -> [Allele]
    generPheno [] = []
    generPheno (alleles : rest) = [getDominant alleles] ++ generPheno rest


run :: IO ()
run = do
  let a = Gen 'a' "color"
  let b = Gen 'b' "smoothness"
  let dad = Genotype [((Allele a True "green"), (Allele a True "green")), ((Allele b True "smooth"), (Allele b False "wrinkle"))]
  let mom = Genotype [((Allele a True "green"), (Allele a False "yellow")), ((Allele b False "wrinkle"), (Allele b False "wrinkle"))]

--  print (genoToPheno dad)
--  print (genoToPheno mom)
  print (computeOffsprings dad mom)