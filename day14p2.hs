import Data.Graph
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.List as L

data Reactant = Reactant Int String deriving (Show)

type Reaction = ([Reactant], Reactant)

addReactant :: Reactant -> Reaction -> Reaction
addReactant newReactant (oldReactants, finalProduct) =
  (newReactant : oldReactants, finalProduct)

parseReaction :: [String] -> Reaction
parseReaction ["=>", num, finalProduct] = ([], Reactant (read num) finalProduct)
parseReaction (num:reactant:rst) =
  addReactant (Reactant (read num) sanitizedReactant) $ parseReaction rst
  where sanitizedReactant = if (last reactant) == ','
                            then take (length reactant-1) reactant
                            else reactant
parseReaction _ = error "Unfinished reaction"

parseInput :: String -> [Reaction]
parseInput = map (parseReaction . words) . lines

type Node = (String, String, [String])

createEdge :: Reaction -> Node
createEdge (children, (Reactant _ parent)) =
  (parent, parent, map getName children)
  where getName (Reactant _ name) = name

type GraphStruct = (Graph, Vertex -> Node, String -> Maybe Vertex)

buildGraph :: [Reaction] -> GraphStruct
buildGraph = graphFromEdges . addOre . map createEdge
  where addOre = (:) ("ORE","ORE",[])

calcProcessingOrder :: GraphStruct -> [String]
calcProcessingOrder (g, getNode, _) = map (getName . getNode) $ topSort g
  where getName (_, name, _) = name

-- String key represents the product of reaction
-- Int in value represents amount of product produced
-- [Reactant] in value represents reactants needed to product product
type ReactionTable = M.Map String ([Reactant], Int)

buildReactionTable :: [Reaction] -> ReactionTable
buildReactionTable = foldr addReaction M.empty
  where addReaction (reactants, (Reactant num finalProduct))
          = M.insert finalProduct (reactants, num)

-- String key represents a material
-- Int value represents the amount of that material currently present
type MaterialsTable = M.Map String Int

calcMaterialsNeeded' :: ReactionTable -> [String] -> Int -> MaterialsTable
calcMaterialsNeeded' reactionTable processingOrder fuelNeeded =
  L.foldl' updateMaterials initialMaterials processingOrder
  where initialMaterials = M.singleton "FUEL" fuelNeeded

        ceilingQuot num1 num2 = (num1+num2-1) `quot` num2

        multiplyReactant multiplier (Reactant curNum reactant) =
          Reactant (curNum*multiplier) reactant

        updateReactant (Reactant num reactant) =
          M.alter (Just . (+num) . fromMaybe 0) reactant
        
        updateMaterials curMaterials newProduct =
          case (M.lookup newProduct reactionTable) of
            Nothing -> if newProduct == "ORE"
                       then curMaterials
                       else error $ "Invalid product: " ++ newProduct
                       
            Just (reactants, numProductProduced) ->
              let numProductNeeded = M.findWithDefault 0 newProduct curMaterials
                  multiplier = numProductNeeded `ceilingQuot` numProductProduced
                  multipliedReactants =
                    map (multiplyReactant multiplier) reactants in
                M.delete newProduct $
                  foldr updateReactant curMaterials multipliedReactants

calcMaterialsNeeded :: [Reaction] -> Int -> MaterialsTable
calcMaterialsNeeded =
  calcMaterialsNeeded'
    <$> buildReactionTable
    <*> (calcProcessingOrder . buildGraph)

calcOreNeeded :: [Reaction] -> Int -> Int
calcOreNeeded = (.) (M.findWithDefault 0 "ORE") <$> calcMaterialsNeeded

findMaxPossible' :: Int -> Int -> Int -> (Int -> Int) -> Int
findMaxPossible' lo hi desired f
  | hi-lo <= 1  = lo
  | otherwise   = let mid = (lo+hi) `quot` 2 in
                  if f mid < desired
                    then findMaxPossible' mid hi desired f
                    else findMaxPossible' lo mid desired f

oneTrillion :: Int
oneTrillion = 1000000000000

findMaxPossible :: Int -> (Int -> Int) -> Int
findMaxPossible = findMaxPossible' 1 oneTrillion

main :: IO ()
main = interact (show . findMaxPossible oneTrillion . calcOreNeeded . parseInput)