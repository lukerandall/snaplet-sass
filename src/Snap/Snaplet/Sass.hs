{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Snap.Snaplet.Sass (
         Sass
       , initSass
       , sassServe
       ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Trans.Writer
import           Data.Char (toLower)
import qualified Data.Configurator          as C
import           Data.List (intercalate)
import           Data.Maybe (isNothing)
import           Snap.Core (modifyResponse, setContentType)
import           Snap.Snaplet
import           Snap.Util.FileServe
import           System.Process (rawSystem)

import           Paths_snaplet_sass
import           Snap.Snaplet.Sass.Internal

-- | Snaplet initialization
initSass :: SnapletInit b Sass
initSass = makeSnaplet "sass" description datadir $ do
  config <- getSnapletUserConfig
  fp <- getSnapletFilePath

  (opts, errs) <- runWriterT $ do
    cmStr <- logErr "Must specify compileMode" $ C.lookup config "compileMode"
    cm    <- case cmStr of
                  Just x -> logErr "Invalid compileMode" . return $ compileModeFromString x
                  Nothing -> return Nothing
    stStr <- logErr "Must specify style" $ C.lookup config "style"
    st    <- case stStr of
                  Just x  -> logErr "Invalid style" . return $ styleFromString x
                  Nothing -> return Nothing
    sm    <- logErr "Must specify sourcemap" $ C.lookup config "sourcemap"
    v     <- logErr "Must specify verbose" $ C.lookup config "verbose"
    return (cm, st, sm, v)

  let sass = case opts of
              (Just cm, Just st, Just sm, Just v) ->
                Sass
                  { snapletFilePath = fp
                  , compileMode     = cm
                  , style           = st
                  , sourcemap       = sm
                  , verbose         = v
                  }
              _ -> error $ intercalate "\n" errs

  -- Make sure snaplet/sass, snaplet/sass/src, snaplet/sass/css are present.
  liftIO $ mapM_ createDirUnlessExists [fp, srcDir sass, destDir sass]

  when (Production == compileMode sass) (liftIO $ compileAll sass)

  return sass

  where
    datadir = Just $ liftM (++ "/resources") getDataDir

    description = "Automatic (re)compilation and serving of Sass files"

    logErr :: MonadIO m => t -> IO (Maybe a) -> WriterT [t] m (Maybe a)
    logErr err m = do
        res <- liftIO m
        when (isNothing res) (tell [err])
        return res


-- | Serves the compiled Fay scripts using the chosen compile mode.
sassServe :: Handler b Sass ()
sassServe = do
  modifyResponse . setContentType $ "text/css;charset=utf-8"
  get >>= compileWithMode . compileMode


-- | Compiles according to the specified mode.
compileWithMode :: CompileMode -> Handler b Sass ()
compileWithMode Development = do
    config <- get
    compileAll config
    compileWithMode Production
-- Production compilation has already been done.
compileWithMode Production = get >>= serveDirectory . destDir

compileAll :: MonadIO m => Sass -> m ()
compileAll cfg = liftIO $ compile >> return ()
  where
    compile = verboseLog >> rawSystem "sass" args
    verboseLog = verbosePut cfg $ "compiling " ++ srcDir cfg
    args = ["--update", ioDirs, "--style", st] ++ sm
    ioDirs = srcDir cfg ++ ":" ++ destDir cfg
    sm = if sourcemap cfg then ["--sourcemap"] else []
    st = map toLower . show $ style cfg
