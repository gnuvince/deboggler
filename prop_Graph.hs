import Test.QuickCheck
import Control.Monad (replicateM)
import qualified Data.Set as S

import qualified Graph as G

instance Arbitrary G.Graph where
    arbitrary = do return G.initialized

prop_16Cubes = \g -> length (G.cubes g) == 16

prop_minMaxAdj = \g -> all (`elem` [3..8]) [S.size (G.neighbors n g) | n <- [0..15]]
