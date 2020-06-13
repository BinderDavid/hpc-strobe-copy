-- StrobeClock.hs: Demonstrate hpc-strobe by rendering a crude analog
-- clock in the marked-up code
-- Copyright (c) 2009, Thorkil Naur
--
-- Usage: ./StrobeClock tixfile-directory
--
-- Note: The indicated tixfile-directory must exist.
--

module Main where

  import System
  import IO
  import Time
  import Control.Concurrent
  import List

  import Trace.Hpc.Strobe

  progName = "StrobeClock"
  progStamp = "2009-May-08 17.26"

  -- The clock will be rendered in the following part of the code. The
  -- number of lines and their width may be adjusted; the width of the
  -- shortest line will determine the width of the clock.

  canvas x
    = [
          [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
        , [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
      ]

  -- Canvas dimensions:

  pixelHeight = (length $ canvas 0) - 1
  pixelWidth = minimum $ map ((subtract 1) . length) $ canvas 0

  -- Defines the shape of pixels: To calibrate, measure the height and
  -- width of a square part of the canvas as rendered and enter the
  -- ratio here:

  pixelHeightDividedByWidth = 16.2/17.4

  -- World coordinate system is such that a 2x2 square with (0,0) in its
  -- center fits exactly inside the canvas rectangle:

  worldDimensions
    = let
        rawDimensions = [1.0,pixelHeightDividedByWidth]
      in
        map ((*2.0) . (/(minimum rawDimensions))) rawDimensions

  worldLowerLeftCorner = map (negate . (/2.0)) worldDimensions

  -- Converting between coordinate systems:

  canvasToWorld [row,col]
    = zipWith (+) worldLowerLeftCorner $ zipWith (*) worldDimensions
        [fromIntegral col / fromIntegral pixelWidth,
          1.0 - fromIntegral row / fromIntegral pixelHeight]

  worldToCanvas wcs
    = let
        [c0,r0]
          = zipWith (/) (zipWith (-) wcs worldLowerLeftCorner)
              worldDimensions
      in
        [round $ (1.0 - r0) * fromIntegral pixelHeight,
          round $ c0 * fromIntegral pixelWidth]

  -- Shade outside unit circle:

  circleShade
    = filter ((>1.0) . sum . map (^2) . canvasToWorld)
        [ [row,col] | row <- [0..pixelHeight], col <- [0..pixelWidth] ]

  -- Line:

  canvasLine [r1,c1] [r2,c2]
    = if abs (r2 - r1) > abs (c2 - c1) then
        map reverse $ canvasLine [c1,r1] [c2,r2]
      else
        if abs (c2 - c1) > 0 then
          let
            [(c1',r1'),(c2',r2')] = sort [(c1,r1),(c2,r2)]
          in
            [ [r,c] | c <- [c1'..c2'],
              let r = r1' + (round $ fromIntegral (r2' - r1')
                              * fromIntegral (c - c1')
                              / fromIntegral (c2' - c1')) ]
        else
          [[r1,c1]]

  worldLine [x1,y1] [x2,y2]
    = canvasLine (worldToCanvas [x1,y1]) (worldToCanvas [x2,y2])

  -- Piece of radial line, angle measured in degrees from vertical,
  -- clockwise:

  worldRadial angle from to
    = let
        radians = (90.0 - angle) / 180.0 * pi
        [p1,p2]
          = map (\t -> map (*t) [cos radians,sin radians]) [from,to]
      in
        worldLine p1 p2

  -- Clock:

  worldClockFixed
    = concat [ worldRadial (fromIntegral a) 0.9 1.0
               | a <- [30,60..360] ]
      ++ concat [ worldRadial (fromIntegral a) 0.8 1.0
                  | a <- [90,180..360] ]
      ++ circleShade

  worldClockVariable h m s
    = concat [ worldRadial ah 0.0 0.55 ]
      ++ concat [ worldRadial (ah+4.0) 0.0 0.45 ]
      ++ concat [ worldRadial (ah-4.0) 0.0 0.45 ]
      ++ concat [ worldRadial am 0.0 0.9 ]
      ++ concat [ worldRadial (am-2.5) 0.0 0.8 ]
      ++ concat [ worldRadial (am+2.5) 0.0 0.8 ]
      ++ concat [ worldRadial as (-0.15) 1.0 ]
      where
      as = fromIntegral s * (360.0/60.0)
      m' = fromIntegral m + fromIntegral s / 60.0
      am = m'*(360/60.0)
      ah = (fromIntegral h + m'/60.0)*(360.0/12.0)

  main'
    = do
        putStrLn $
          progName ++ "(" ++ progStamp ++ "): Canvas pixel height "
            ++ show pixelHeight ++ ", pixel width " ++ show pixelWidth
        mapM_
          (\n ->
            do
              clTime <- getClockTime
              localTime@(CalendarTime{ctHour = hr,
                ctMin = mn, ctSec = sc}) <- toCalendarTime clTime
              let
                timeStamp = calendarTimeToString localTime
                in
                  do
                    putStrLn $ progName ++ "(" ++ progStamp ++ "): "
                      ++ (show $
                            sum [ ((canvas n)!!i)!!j
                                  | [i,j] <- worldClockFixed
                                     ++ worldClockVariable hr mn sc ])
              threadDelay 950000
          ) [1..]

  mainArgsInterpret [tixfileDirectory]
    = withStrobesWrittenRegularly tixfileDirectory progName 1000000
        main'

  mainArgsInterpret args
    = error $ "Usage: \"./" ++ progName ++ " tixfile-directory\""

  main
    = do
        hSetBuffering stdout NoBuffering
        args <- getArgs
        mainArgsInterpret args
