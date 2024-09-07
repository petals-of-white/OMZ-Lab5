module DoG where
import qualified Data.Vector.Storable   as VS
import           Vision.Image           as I
import           Vision.Primitive.Shape

-- ### TYPES

data Connectivity = FourConnectivity | EightConnectivity

-- | A cross, represents 4-connectivity
data Four a = Four {
  top4 :: a,
  left4 :: a, center4 :: a, right4 :: a,
  bottom4 :: a} deriving Functor

-- | A square, represents 8-connectivity
data Eight a = Eight {
  tl8 :: a, tm8 :: a, tr8 :: a,
  ml8 :: a, center8 :: a, mr8 :: a,
  bl8 :: a, bm8 :: a, br8 :: a
  } deriving Functor



-- ### FUNCTIONS

-- | DoG operator
doGOp :: (Num a, Integral a, VS.Storable a) => Int -> Maybe Float -> Maybe Float ->  Manifest a -> Manifest a

doGOp r sigma1 sigma2 src = Manifest {manifestSize = size, manifestVector= VS.zipWith (-) g1 g2}
  where
    Manifest {manifestSize = size, manifestVector = g1} = gaussianBlur r sigma1 src
    Manifest {manifestVector = g2} = gaussianBlur r sigma2 src

-- | DoG method
doG :: (Num a, Integral a, VS.Storable a) => Int -> Maybe Float -> Maybe Float -> Connectivity -> Float -> Manifest a -> Manifest Bool

doG r sigma1 sigma2 connectivity thresholdPercent src =
  let dogged = doGOp r sigma1 sigma2 src
      at = index dogged
      (Z :. h :. w) = shape dogged
      noBorderCoords = [ix2 y x | y <- [1..h-2], x <- [1..w-2]] in

  Manifest (ix2 (h-2) (w-2)) $ VS.fromList $
    case connectivity of

      FourConnectivity ->
        fmap (zeroCrossing4 (VS.maximum (vector src)) thresholdPercent . fmap at)
        [Four {top4=ix2 (y-1) x, left4=ix2 y (x-1), center4 = ix2 y x, right4=ix2 y (x+1), bottom4=ix2 (y+1) x}
        | (Z :. y :. x) <- noBorderCoords]

      EightConnectivity ->
        fmap (zeroCrossing8 (VS.maximum (vector src)) thresholdPercent . fmap at)
        [Eight
          (ix2 (y-1) (x-1)) (ix2 (y-1) x) (ix2 (y-1) (x+1))
          (ix2 y (x-1)) (ix2 y x) (ix2 y (x+1))
          (ix2 (y+1) (x-1)) (ix2 (y+1) x) (ix2 (y+1) (x+1))

        | (Z :. y :. x) <- noBorderCoords]



zeroCrossing4 :: (Real a, Eq a) => a -> Float -> Four a -> Bool

zeroCrossing4 maxV percent Four {top4, left4, right4, bottom4} =
  signum top4 /= signum bottom4 && abs (realToFrac (top4 - bottom4)) > thresh
  || signum left4 /= signum right4 && abs (realToFrac (left4 - right4)) > thresh
  where thresh = percent * realToFrac maxV


zeroCrossing8 :: (Real a, Eq a) => a -> Float -> Eight a -> Bool

zeroCrossing8 maxV percent Eight {tl8, tm8, tr8, ml8, mr8, bl8, bm8, br8} =
  signum tl8 /= signum br8 && abs (realToFrac (tl8 - br8)) > thresh
  || signum bl8 /= signum tr8 && abs (realToFrac (bl8 - tr8)) > thresh
  || signum tm8 /= signum bm8 && abs (realToFrac (tm8 - bm8)) > thresh
  || signum ml8 /= signum mr8 && abs (realToFrac (ml8 - mr8)) > thresh
  where thresh = percent * realToFrac maxV
