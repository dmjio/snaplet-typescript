module Snap.Snaplet.TypeScript.Utils
    ( CompileMode(..)
    , TypeScriptConf(..)
    , allTypeScriptFiles
    , destDir
    , srcDir
    , createDirUnlessExists
    , getCompilerMode
    , getDestDir
    , requestedTypeScriptFile
    ) where

import           Control.Monad
import           System.Directory
import           System.FilePath

-- | Simple data structure for CompileMode
data CompileMode = Development | Production deriving (Show, Eq)

-- | The TypeScript Snaplet Configuration
data TypeScriptConf = TypeScriptConf
    { snapletFilePath :: FilePath
    , compiler        :: Maybe String
    , compileMode     :: CompileMode
    , destinationDir  :: FilePath
    } deriving (Show)

-- | Gets all the .typeScript files in the source directory
allTypeScriptFiles :: TypeScriptConf -> IO [FilePath]
allTypeScriptFiles cfg = do
    let p = srcDir cfg
    a <- getDirectoryContents p
    return $ map (</> srcDir cfg) $ filterTypeScript a

-- | Simple reverse taker
takeReverse :: Int -> [a] -> [a]
takeReverse i = reverse . take i . reverse

-- | Filters the .typeScript files
filterTypeScript :: [String] -> [String]
filterTypeScript = filter ((==) ".typeScript" . takeReverse 7)

-- | Source directory
srcDir :: TypeScriptConf -> FilePath
srcDir = (</> "typeScript") . snapletFilePath

-- | Destination directory
destDir :: TypeScriptConf -> FilePath
destDir = (</> "js") . snapletFilePath

-- | Function to create directories if they don't exist
createDirUnlessExists :: FilePath -> IO ()
createDirUnlessExists fp = do
    dirExists <- doesDirectoryExist fp
    unless dirExists $ createDirectory fp

-- | Gets the CompileMode
getCompilerMode :: Maybe String -> CompileMode
getCompilerMode Nothing              = Production
getCompilerMode (Just "Development") = Development
getCompilerMode (Just "Production")  = Production
getCompilerMode (Just x)             = error $ x ++ " is not a valid Compiler mode for snaplet-typeScriptscript. -- devel.cfg"

-- | Gets the destination directory
getDestDir :: Maybe String -> TypeScriptConf -> FilePath
getDestDir Nothing   c = destDir c
getDestDir (Just "") c = destDir c
getDestDir (Just x)  _ = x

-- | Converts path/to/file.js to file.typeScript
requestedTypeScriptFile :: String -> FilePath
requestedTypeScriptFile =
     ("/"++) . (++"typeScript") . reverse .dropWhile con2 . takeWhile con1 . reverse
  where con1 = (/= '/')
        con2 = (/= '.')
