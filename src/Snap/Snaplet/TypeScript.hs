{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.TypeScript ( TypeScriptConf(..)
                               , initTypeScript
                               , typeScriptServe
                               ) where

import           Control.Applicative           ((<$>))
import           Control.Monad                 (liftM)
import           Control.Monad.Reader          (MonadIO, liftIO, when)
import           Control.Monad.State.Class     (get)
import qualified Data.ByteString.Char8         as BS (unpack)
import qualified Data.Configurator             as C (lookup)
import           Paths_snaplet_typescript
import           Snap.Core                     (getRequest, modifyResponse,
                                                rqURI, setContentType)
import           Snap.Snaplet
import           Snap.Snaplet.TypeScript.Utils
import           Snap.Util.FileServe           (serveDirectory)
import           System.Exit                   (ExitCode (..))
import           TypeScript.Bindings           (TypeScript (..),
                                                typeScriptCompile)

-- | Snaplet-TypeScript initializer
initTypeScript :: SnapletInit c TypeScriptConf
initTypeScript = makeSnaplet "TypeScript" "Snaplet for TypeScript" dataDir $ do
    config <- getSnapletUserConfig
    fp <- getSnapletFilePath
    [comp, dev, dDir] <- liftIO $ mapM (C.lookup config) configOptions
    let typeScript = TypeScriptConf fp comp (getCompilerMode dev) (getDestDir dDir typeScript)
    liftIO $ mapM_ createDirUnlessExists [fp, srcDir typeScript, destDir typeScript]
    allTypeScripts <- liftIO $ allTypeScriptFiles typeScript
    when (Production == compileMode typeScript) $ liftIO $ compileFiles typeScript allTypeScripts
    return typeScript
  where dataDir = Just $ liftM (++ "/resources") getDataDir
        configOptions = ["compilerPath", "compilerMode", "destinationPath"]

-- | Serves the compiled TypeScript files
typeScriptServe :: Handler b TypeScriptConf ()
typeScriptServe = do
    modifyResponse . setContentType $ "text/javascript;charset=utf-8"
    cfg <- get
    requestedFile <- (srcDir cfg ++) . requestedTypeScriptFile .
                     BS.unpack . rqURI <$> getRequest
    when (Development == compileMode cfg) $ liftIO $ compileFiles cfg [requestedFile]
    serveDirectory $ destinationDir cfg

-- | Compiles the files that are being served, but ignores all other files.
compileFiles :: MonadIO m => TypeScriptConf -> [FilePath] -> m ()
compileFiles cfg fp = do
    let typeScriptStruct = TypeScript (compiler cfg)
    result    <- liftIO $ typeScriptCompile fp
                 (Just (destinationDir cfg)) typeScriptStruct
    case result of
        ExitSuccess   -> return ()
        ExitFailure x -> error $ show x ++ errMsg
  where errMsg = unlines
                 [ " - Error while compiling TypeScript"
                 , "Does this file really exist?"
                 , "Is your /snaplets/typeScript/devel.cfg correct?"
                 , "You might need to restart the server after fixing the issue."
                 ]
