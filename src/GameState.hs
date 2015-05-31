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

import Debug.Trace
import Control.Applicative
import Data.List
import Data.Maybe (fromJust, isJust, catMaybes)
import qualified Data.IntSet as S
import Objects
import Physics.TwoDimensions.Dimensions
import FRP.Yampa.VectorSpace
import FRP.Yampa (Time)

-- | The running state is given by a bunch of 'Objects' and the current general
-- 'GameInfo'. The latter contains info regarding the current level, the number
-- of points, etc.
--
-- Different parts of the game deal with these data structures.  It is
-- therefore convenient to group them in subtrees, even if there's no
-- substantial difference betweem them.
data GameState = GameState
  { gameObjects :: !Objects
  , gameInfo    :: !GameInfo
  , player      :: !Player
  , graph       :: !Graph
  }

playerPosition :: Graph -> Player -> Maybe Pos2D
playerPosition _     Nothing                = Nothing
playerPosition graph (Just (orig, Nothing)) = nodePosition graph orig
playerPosition graph (Just (orig, Just (TransitionInfo prog dest))) =
  (fst . (`coordsF` prog)) <$> findArrowByNodes orig dest (arrows graph)

nodePosition :: Graph -> NodeId -> Maybe Pos2D
nodePosition graph nid = nodePos <$> (find (\x -> nodeId x == nid) (nodes graph))

nodeAtPos :: Graph -> Pos2D -> Maybe NodeId
nodeAtPos graph pos =
  nodeId <$> (find (pos `ontoNode`) $ nodes graph)

ontoNode :: Pos2D -> Node -> Bool
ontoNode pos node =
  norm (pos ^-^ (nodePos node)) < nodeSize
 where nodeSize = 20

startMoving :: Graph -> NodeId -> NodeId -> (Player, Graph)
startMoving graph orig dest = (Just (orig, Just (TransitionInfo 0 dest)), graph)

type Player = Maybe (NodeId, Maybe TransitionInfo)

data TransitionInfo = TransitionInfo
  { relativePos  :: RelativePos
  , transitionId :: NodeId -- This assumes max one link between any two nodes
  }
 deriving Show

data Graph = Graph
  { nodes  :: [Node]
  , arrows :: [Arrow]
  }

data Node = Node
  { nodeId    :: NodeId
  , nodePos   :: Pos2D
  , nodeFinal :: Bool
  }
 deriving Show

type NodeId = Int

data Arrow = Arrow
  { arrowNode1 :: NodeId
  , arrowNode2 :: NodeId
  , speedF     :: RelativePos -> RelativeSpeed
  , coordsF    :: RelativePos -> (Pos2D, Vel2D)
  , arrowHeads :: [RelativePos]
  }

connectedNodes :: Graph -> NodeId -> NodeId -> Bool
connectedNodes graph orig dest = isJust (findArrowByNodes orig dest (arrows graph))

graphSpeedF :: Graph -> NodeId -> NodeId -> RelativePos -> RelativeSpeed
graphSpeedF graph orig dest =
  speedF $ fromJust $ findArrowByNodes orig dest (arrows graph)

findNode :: Graph -> NodeId -> Maybe Node
findNode graph nid = find (\x -> nodeId x == nid) $ nodes graph

findArrowByNodes :: NodeId -> NodeId -> [Arrow] -> Maybe Arrow
findArrowByNodes _    _    [] = Nothing
findArrowByNodes orig dest (a:as)
   | arrowNode1 a == orig && arrowNode2 a == dest
   = Just a
   | otherwise
   = findArrowByNodes orig dest as

type RelativePos   = Double -- [0,1] numerical range
type RelativeSpeed = Double -- [0,1] numerical range

stateLocked :: Player -> Graph -> Bool
stateLocked p g = case p of
  Just (n,Nothing) -> not (finalReachable g n)
  _                -> False

reachable1 :: Graph -> NodeId -> [NodeId]
reachable1 graph nid =
  map arrowNode2 $ filter (isOrig nid) (arrows graph)

reachableN :: Graph -> NodeId -> [NodeId]
reachableN graph nid = S.toList $ reachableN' graph nid (S.singleton nid)

reachableN' graph nid visited =
  if reachable `S.isSubsetOf` visited
   then visited
   else S.foldr (reachableN' graph) (visited `S.union` reachable) new
  where reachable = S.fromList (reachable1 graph nid)
        new       = reachable S.\\ visited
  
isOrig :: NodeId -> Arrow -> Bool
isOrig nid arrow = nid == (arrowNode1 arrow)
  
isDest :: NodeId -> Arrow -> Bool
isDest nid arrow = nid == (arrowNode2 arrow)

finalReachable g n = let reachable = map (findNode g) $ reachableN g n
  in -- trace (show (n, reachable))
      (any nodeFinal $ catMaybes reachable)

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
  , gameTime   :: Time
  }

-- | Initial (default) game info (no points, no lives, no level).
neutralGameInfo :: GameInfo
neutralGameInfo = GameInfo
  { gameStatus = GameStarted
  , gameLevel  = 0
  , gameLives  = 0
  , gamePoints = 0
  , gameTime   = 0
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

quickTrace x = trace (show x) x 
