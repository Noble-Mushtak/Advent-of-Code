import qualified Data.List as L
import Data.List.Split
import Control.Applicative
import Data.Maybe

layerLength :: Int
layerLength = 25 * 6

type Layer = [Maybe Char]

defaultLayer :: Layer
defaultLayer = replicate layerLength Nothing

updateLayer :: Layer -> String -> Layer
updateLayer origLayer newLayer = zipWith (<|>) origLayer $ map wrapChar newLayer
  where wrapChar '2' = Nothing
        wrapChar ch  = Just ch

main :: IO ()
main = interact (L.intercalate "\n" . chunksOf 25 . map (fromMaybe '2') . foldl updateLayer defaultLayer . chunksOf layerLength)