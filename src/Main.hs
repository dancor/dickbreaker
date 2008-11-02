module Main where

-- todo: brick bounce, paddle bounce, death, brick dissappear, win

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Random
import Data.Array
import FUtil
import Paths_dickbreaker
import System.FilePath
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Img
import qualified Graphics.UI.SDL.Rotozoomer as Rot
import qualified Graphics.UI.SDL.TTF as Font

data Color = Red | Grey deriving Eq
data Power = Laser | Catch | Gun | Flip | Multi | Bomb | Life | Wrap
  deriving Eq
data PowerTypes = BallAlter | PaddleAlter
data BdSq = Emp | Block Color Bool deriving Eq
type Bd = Array (Int, Int) BdSq
data Game = Game {
  gPics :: [SDL.Surface],
  gScreen :: SDL.Surface,
  gBd :: TVar Bd,
  gBallX :: TVar Float, -- 0..761
  gBallY :: TVar Float, -- 0..865
  gBallXV :: TVar Float,
  gBallYV :: TVar Float,
  gGunX :: TVar Int  -- 0..637 inclusive
  }

powerGetTypes Catch = [PaddleAlter, BallAlter]
powerGetTypes Life = [PaddleAlter, BallAlter]
powerGetTypes Wrap = []
powerGetTypes Multi = [BallAlter]
powerGetTypes Bomb = [BallAlter]
powerGetTypes _ = [PaddleAlter]

randPower :: (RandomGen g) => Rand g Power
randPower = choice [Laser, Catch, Gun, Flip, Multi, Bomb, Life, Wrap]

bdEmp :: Bd
bdEmp = listArray ((1, 1), (7, 15)) $ repeat Emp

level :: Int -> Bd
--level 1 = listArray ((1, 1), (7, 15)) $ repeat $ Block Red False
level 1 = bdEmp // [
  ((3, 2), Block Red False),
  ((5, 2), Block Red True),
  ((2, 3), Block Red False),
  ((6, 3), Block Red False),
  ((2, 5), Block Red True),
  ((6, 5), Block Red False),
  ((2, 6), Block Red False),
  ((6, 6), Block Red False),
  ((2, 8), Block Red False),
  ((6, 8), Block Red False),
  ((3, 9), Block Red False),
  ((5, 9), Block Red False)
  ]
level _ = error "level out of range"

scrW = 1400
scrH = 1050

drawBd (Game pics screen bdA ballXA ballYA ballXVA ballYVA gunXA) = do
  bd <- atomically $ readTVar bdA
  ballX <- atomically $ readTVar ballXA
  ballY <- atomically $ readTVar ballYA
  ballXV <- atomically $ readTVar ballXVA
  ballYV <- atomically $ readTVar ballYVA
  gunX <- atomically $ readTVar gunXA
  let
    isFull myBd x y = case myBd ! (x, y) of
      Emp -> False
      _ -> True
    x1 = ballX + ballXV
    -- these aren't quite right..
    xBlockL = floor (x1 / 116) + 1
    xBlockR = floor ((x1 + 50) / 116) + 1
    (ballX', ballXV', bd') = fX
    fX
      | xBlockL < 1 || xBlockR > 7 =
        (x1 - 2 * ballXV, negate ballXV, bd)
      | yBlockL >= 1 && yBlockL <= 15 && isFull bd xBlockL yBlockL =
        (x1 - 2 * ballXV, negate ballXV, bd // [((xBlockL, yBlockL), Emp)])
      | yBlockL >= 1 && yBlockL <= 15 && isFull bd xBlockR yBlockL =
        (x1 - 2 * ballXV, negate ballXV, bd // [((xBlockR, yBlockL), Emp)])
      | yBlockR >= 1 && yBlockR <= 15 && isFull bd xBlockL yBlockR =
        (x1 - 2 * ballXV, negate ballXV, bd // [((xBlockL, yBlockR), Emp)])
      | yBlockR >= 1 && yBlockR <= 15 && isFull bd xBlockR yBlockR =
        (x1 - 2 * ballXV, negate ballXV, bd // [((xBlockR, yBlockR), Emp)])
      | otherwise = (x1, ballXV, bd)
    y1 = ballY + ballYV
    yBlockL = floor (y1 / 61) + 1
    yBlockR = floor ((y1 + 50) / 61) + 1
{-
        xBlockL >= 1 && xBlockL <= 7 && isFull bd' xBlockL yBlockL ||
        xBlockL >= 1 && xBlockL <= 7 && isFull bd' xBlockL yBlockR ||
        xBlockR >= 1 && xBlockR <= 7 && isFull bd' xBlockR yBlockL ||
        xBlockR >= 1 && xBlockR <= 7 && isFull bd' xBlockR yBlockR
      then (y1 - 2 * ballYV, negate ballYV, bd')
      else (y1, ballYV, bd')
        -}
    (ballY', ballYV', bd'') = fY
    fY
      | yBlockL < 1 || yBlockR > 15 =
        (y1 - 2 * ballYV, negate ballYV, bd)
      | xBlockL >= 1 && xBlockL <= 7 && isFull bd xBlockL yBlockL =
        (y1 - 2 * ballYV, negate ballYV, bd // [((xBlockL, yBlockL), Emp)])
      | xBlockL >= 1 && xBlockL <= 7 && isFull bd xBlockL yBlockR =
        (y1 - 2 * ballYV, negate ballYV, bd // [((xBlockL, yBlockR), Emp)])
      | xBlockR >= 1 && xBlockR <= 7 && isFull bd xBlockR yBlockL =
        (y1 - 2 * ballYV, negate ballYV, bd // [((xBlockR, yBlockL), Emp)])
      | xBlockR >= 1 && xBlockR <= 7 && isFull bd xBlockR yBlockR =
        (y1 - 2 * ballYV, negate ballYV, bd // [((xBlockR, yBlockR), Emp)])
      | otherwise = (y1, ballYV, bd)
    bd''' = if bd'' == bdEmp then level 1 else bd''
  -- are we in a brick
  atomically $ writeTVar ballXA ballX'
  atomically $ writeTVar ballYA ballY'
  atomically $ writeTVar ballXVA ballXV'
  atomically $ writeTVar ballYVA ballYV'
  atomically $ writeTVar bdA bd'''
  SDL.blitSurface (head pics) (Just $ SDL.Rect 0 0 scrW scrH) screen . Just $
    SDL.Rect 0 0 0 0
  mapM_ (\ ((x, y), bdSq) -> do
    case bdSq of
      Emp -> return ()
      Block col _ -> do
        let pic = pics !! (if col == Red then 1 else 2)
        SDL.blitSurface pic Nothing screen
          (Just $ SDL.Rect (116 * (x - 1) + 180) (61 * (y - 1) + 132) 0 0)
        return ()
    ) $ assocs bd
  SDL.blitSurface (pics !! 3) Nothing screen
    (Just $ SDL.Rect (gunX + 180) 1000 0 0)
  SDL.blitSurface (pics !! 4) Nothing screen
    (Just $ SDL.Rect (floor ballX + 180) (floor ballY + 132) 0 0)
  SDL.flip screen
  --print gunX

debug :: (Monad m) => t -> m ()
debug s = return ()

eventLoop :: Game -> IO ()
eventLoop gm = do
  event <- SDL.waitEvent
  (_, quit) <- case event of
{-
    SDL.VideoExpose -> do
      debug "expose"
      --putMVar (gRedraw gm) ()
      drawBd gm
      return (gm, False)
      -}
    SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _) -> do
      debug "q"
      return (gm, True)
    SDL.MouseMotion x y xrel yrel -> do
      let gunXA = gGunX gm
      gunX <- atomically $ readTVar gunXA
      let bound x = if x < 0 then 0 else if x > 637 then 637 else x
      atomically . writeTVar gunXA . bound $ gunX + fromIntegral xrel
      return (gm, False)
    SDL.Quit -> do
      debug "quit"
      return (gm, True)
    _ -> return (gm, False)
  unless quit $ eventLoop gm

redrawLoop gm = do
  drawBd gm
  let fps = 60
  threadDelay (1000000 `div` fps)
  redrawLoop gm

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  SDL.showCursor False
  Font.init
  --screen <- SDL.setVideoMode scrW scrH 0 [SDL.DoubleBuf, SDL.Fullscreen]
  --screen <- SDL.setVideoMode scrW scrH 0 [SDL.Fullscreen]
  screen <- SDL.setVideoMode scrW scrH 0 [SDL.Fullscreen, SDL.DoubleBuf,
    SDL.HWAccel, SDL.HWSurface]
  putStrLn "hi"
  dataDir <- getDataDir
  let
    imgDir = dataDir ++ "/pics"
    imgNames = map (++ ".png") ["bg", "red-brick", "grey-brick", "hitter", "ball"]
  pics <- mapM (\ x -> Img.load $ imgDir </> x) imgNames
  font <- Font.openFont "/usr/share/gnubg/fonts/Vera.ttf" 12
  bdA <- atomically . newTVar $ level 1
  gunXA <- atomically $ newTVar 319
  ballXA <- atomically $ newTVar 319
  ballYA <- atomically $ newTVar 800
  ballXVA <- atomically $ newTVar $ 36 * 1.6
  ballYVA <- atomically . newTVar $ -27 * 2
  let gm = Game {
    gPics = pics,
    gScreen = screen,
    gBd = bdA,
    gBallX = ballXA,
    gBallY = ballYA,
    gBallXV = ballXVA,
    gBallYV = ballYVA,
    gGunX = gunXA
    }
  forkIO $ redrawLoop gm
  eventLoop gm
