{-# LANGUAGE LambdaCase #-}

module Main where
import           App
import           Codec.BMP                  (parseBMP)
import           Data.Binary
import qualified Data.ByteString.Lazy       as L
import           Data.Char                  (toLower)
import           Data.DICOM                 (readObjectFromFile)
import           Data.DICOM.Utilities       as DicomMap
import           Graphics.Gloss
import           System.Environment         (getArgs)
import           Text.Read
import           Vision.Image               as I
import           Vision.Image.Storage.DevIL (BMP (BMP), saveBS)
import DoG


main :: IO ()
main =
  do
    args <- getArgs

    case parseArgs args of

      Right ProgramArgs {
        argDicom=dicomPath, argRadius=r, argSigma1=sigma1, argSigma2=sigma2,
        argThreshold=thresh, argConnectivity=connectivity} -> do

        dicomObj <- either error id <$> readObjectFromFile dicomPath
        let fridayImg = either (error . show) id $ dicomToFriday16 dicomObj
            fridayOgNormalized = I.map (GreyPixel . convertPropToBounds) (normalizePeaks 0 (maxBound :: Word16) fridayImg) :: Grey

            dogged =
              I.map (\case True -> RGBAPixel 255 0 0 255; False -> RGBAPixel 0 0 0 0) $
              doG r (Just sigma1) (Just sigma2) connectivity thresh $
              I.map (fromIntegral :: Word16 -> Int) fridayImg :: RGBA

            origPic =
              fmap bitmapOfBMP $
              (mapLeft show . parseBMP) . L.fromStrict
              =<<
              mapLeft show (saveBS BMP fridayOgNormalized)


            dogPic =
              fmap bitmapOfBMP $
              (mapLeft show . parseBMP) . L.fromStrict
              =<<
              mapLeft show (saveBS BMP dogged)

        case (origPic, dogPic) of

          (Right origP, Right dogP) ->
            play (InWindow "Lab5" (256,256) (10,10)) black 0 Original
              (\case Original -> origP; DoG -> origP <> dogP) keyEvents (const id)

          (Left err1, Left err2)     -> error (err1 ++ err2)
          (Left err1, Right _)       -> error err1
          (Right _, Left err2)       -> error err2

      Left err -> error err


data ProgramArgs = ProgramArgs {
  argDicom :: FilePath, argRadius :: Int,
  argSigma1 :: Float, argSigma2 :: Float,
  argThreshold :: Float, argConnectivity :: Connectivity}

parseArgs :: [String] -> Either String ProgramArgs
parseArgs [path, rStr, sigma1Str, sigma2Str, thresholdStr, connectivityStr] = do
  r <- readEither rStr
  sigma1 <- readEither sigma1Str
  sigma2 <- readEither sigma2Str
  thresh <- readEither thresholdStr
  connectivity <-
        case fmap toLower connectivityStr of
          "8" -> Right EightConnectivity
          "eight" -> Right EightConnectivity
          "4" -> Right FourConnectivity
          "four" -> Right FourConnectivity
          _ -> Left "Uknown Connectivity. should be one of '4,four, 8, eight'"

  return ProgramArgs {
    argDicom=path, argRadius=r, argSigma1=sigma1, argSigma2=sigma2,
    argThreshold=thresh, argConnectivity=connectivity}

parseArgs other = Left ("Wrong number of arguments: " ++ show (length other) ++ ". Expected format: dicompath radius sigma1 sigma2 threshold connectivity")
