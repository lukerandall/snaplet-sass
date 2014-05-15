module Snap.Snaplet.Sass.Internal
    (
    -- * Sass types
      Sass (..)
    , CompileMode (..)
    , Style (..)
    -- * helper functions
    , srcDir
    , destDir
    , createDirUnlessExists
    , compileModeFromString
    , styleFromString
    , verbosePut
    ) where

import Control.Monad
import System.Directory
import System.FilePath ((</>))


-- | Configuration
data Sass = Sass {
    snapletFilePath :: FilePath
  , compileMode     :: CompileMode
  , style           :: Style
  , sourcemap       :: Bool
  , verbose         :: Bool
  } deriving (Show)


-- | Compile on every request or when Snap starts.
data CompileMode
     = Development
     | Production
       deriving (Eq, Show)


-- | Style of generated CSS
data Style
     = Nested
     | Compact
     | Compressed
     | Expanded
       deriving (Eq, Show)


-- | Location of Sass files
srcDir :: Sass -> FilePath
srcDir = (</> "src") . snapletFilePath


-- | Location of CSS files
destDir :: Sass -> FilePath
destDir = (</> "css") . snapletFilePath


-- | Create given directory unless it exists
createDirUnlessExists :: FilePath -> IO ()
createDirUnlessExists fp = do
    dirExists <- doesDirectoryExist fp
    unless dirExists $ createDirectory fp


-- | Lookup CompileMode for string
compileModeFromString :: String -> Maybe CompileMode
compileModeFromString "Development" = Just Development
compileModeFromString "Production" = Just Production
compileModeFromString _ = Nothing


-- | Lookup Style for string
styleFromString :: String -> Maybe Style
styleFromString "nested" = Just Nested
styleFromString "compact" = Just Compact
styleFromString "compressed" = Just Compressed
styleFromString "expanded" = Just Expanded
styleFromString _ = Nothing


-- | Print log messages when the verbose flag is set
verbosePut :: Sass -> String -> IO ()
verbosePut config = when (verbose config) . putStrLn . ("snaplet-sass: " ++ )
