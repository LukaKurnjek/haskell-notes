import Control.Monad (replicateM)
import Data.Foldable (foldl')
import qualified System.Random.Stateful as Rand

data Drone = Drone
  { xPos :: Int
  , yPos :: Int
  , zPos :: Int
  } deriving Show

data Movement
  = Forward | Back | ToLeft | ToRight | Up | Down
  deriving (Show, Enum, Bounded)

main :: IO ()
main = do
  let initDrone = Drone { xPos = 0, yPos = 100, zPos = 0 }
  -- Generate 15 moves randomly
  randomMoves <- replicateM 15 $ Rand.uniformEnumM Rand.globalStdGen
  let resultDrone = foldl' moveDrone initDrone randomMoves
  print resultDrone

moveDrone :: Drone -> Movement -> Drone
moveDrone drone move =
  case move of
    Forward -> drone { zPos = zPos drone + 1 }
    Back    -> drone { zPos = zPos drone - 1 }
    ToLeft  -> drone { xPos = xPos drone - 1 }
    ToRight -> drone { xPos = xPos drone + 1 }
    Up      -> drone { yPos = yPos drone + 1 }
    Down    -> drone { yPos = yPos drone - 1 }
