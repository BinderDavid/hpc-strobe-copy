------------------------------------------------------------------------
-- |
-- Module      : Trace.Hpc.Strobe
-- Copyright   : (C) 2009, Thorkil Naur
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Thorkil Naur
-- Stability   : unstable
-- Portability : Requires GHC with Hpc support.
--
-- A rudimentary library that demonstrates the possibility of
-- using Hpc (Haskell Program Coverage) to inspect the state of
-- a running Haskell program. Use of the library involves a
-- simple change of the main function and also requires the
-- program to be enabled for hpc. At the time of writing, this
-- means using a fairly recent version of GHC and compiling the
-- Haskell code with the @-fhpc@ option.
--
------------------------------------------------------------------------

module Trace.Hpc.Strobe (

    -- * Write strobes (tix files) regularly while program is running
      withStrobesWrittenRegularly

  ) where

import System.FilePath
import Control.Concurrent
import Trace.Hpc.Tix
import Trace.Hpc.Reflect

strobe :: FilePath -> String -> Int -> IO ()
strobe directory filePrefix interval
  = do mapM_ go [0..]
    where
    go :: Integer -> IO ()
    go d = do strobeTix d
              threadDelay interval
    fileName n = (filePrefix ++ "_" ++ show n) <.> "tix"
    strobeTix n = do tix <- examineTix
                     writeTix (directory </> fileName n) tix

-- | Replace
--
-- @
-- main = \<your main function>
-- @
--
-- by
--
-- @
-- myMain = \<your main function>
-- main = withStrobesWrittenRegularly
--          \"\<tixfile directory>\"
--          \"myMain\"
--          1000000
--          myMain
-- @
--
-- and strobes (separate Hpc tix files) will be written regularly,
-- with 1 second (1000000 microseconds) between each writing of a
-- strobe. The tix files will be deposited in the indicated tixfile
-- directory and be named \"myMain_\<index>.tix\" where the
-- index is taken from [0..].

withStrobesWrittenRegularly
  :: FilePath     -- ^ Name of directory where .tix files are deposited.
                  -- If this directory does not exist, no tix files are
                  -- written.
  -> String       -- ^ Prefix of file name for .tix files.
  -> Int          -- ^ Microseconds between writing a .tix file.
  -> IO ()        -- ^ The main function to be strobe'd.
  -> IO ()        -- ^ The modified main function.

withStrobesWrittenRegularly directory filePrefix interval mainFunction
  = do
      forkIO $ strobe directory filePrefix interval
      mainFunction
