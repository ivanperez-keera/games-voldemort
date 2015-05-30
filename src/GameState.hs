-- | The state of the game during execution. It has two
-- parts: general info (level, points, etc.) and
-- the actual gameplay info (objects).
--
-- Because the game is always in some running state
-- (there are no menus, etc.) we assume that there's
-- always some gameplay info, even though it can be
-- empty.
module GameState where

-- import FRP.Yampa as Yampa

import Objects
import Physics.TwoDimensions.Dimensions

-- | The running state is given by a bunch of 'Objects' and the current general
-- 'GameInfo'. The latter contains info regarding the current level, the number
-- of points, etc.
--
-- Different parts of the game deal with these data structures.  It is
-- therefore convenient to group them in subtrees, even if there's no
-- substantial difference betweem them.
data GameState = GameState
  { gameObjects :: Objects
  , gameInfo    :: GameInfo
  , player      :: Maybe (NodeId, Maybe TransitionInfo)
  , graph       :: Graph
  }

data TransitionInfo = TransitionInfo
  { relativePos  :: RelativePos
  , transitionId :: NodeId -- This assumes max one link between any two nodes
  }

data Graph = Graph
  { nodes  :: [Node]
  , arrows :: [Arrow]
  }

data Node = Node
  { nodeId    :: NodeId
  , nodePos   :: Pos2D
  , nodeFinal :: Bool
  }

type NodeId = Int

data Arrow = Arrow
  { arrowNode1 :: NodeId
  , arrowNode2 :: NodeId
  , speedF     :: RelativePos -> RelativeSpeed
  , coordsF    :: RelativePos -> (Pos2D, Vel2D)
  }

type RelativePos   = Float -- [0,1] numerical range
type RelativeSpeed = Float -- [0,1] numerical range

-- | Initial (default) game state.
neutralGameState :: GameState
neutralGameState = GameState
  { gameObjects = []
  , gameInfo    = neutralGameInfo
  , player      = Nothing
  , graph       = Graph [] []
  }

-- | The gameinfo tells us the current game state (running, paused, etc.)
-- and general information, in this case, the number of lives, the level
-- and the points.
--
-- Since this info is then presented together to the users in a top panel, it
-- is convenient to give this product of values a proper name.
data GameInfo = GameInfo
  { gameStatus :: GameStatus
  , gameLives  :: Int
  , gameLevel  :: Int
  , gamePoints :: Int
  }

-- | Initial (default) game info (no points, no lives, no level).
neutralGameInfo :: GameInfo
neutralGameInfo = GameInfo
  { gameStatus = GameStarted
  , gameLevel  = 0
  , gameLives  = 0
  , gamePoints = 0
  }

-- | Possible actual game statuses. The game is always in one of these.
-- Interaction and presentation depend on this. Yampa switches are
-- used to jump from one to another, and the display module
-- changes presentation depending on the status.
data GameStatus = GamePlaying
                | GamePaused
                | GameLoading Int
                | GameOver
                | GameFinished
                | GameStarted
 deriving Eq
