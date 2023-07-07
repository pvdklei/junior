module Main (main, compile) where

import qualified Lex
import qualified Parse
import qualified Ast
import qualified Write
import qualified Utils
import qualified Analyse

import qualified System.Environment as E
-- import qualified System.Directory.Tree as SDT
import qualified System.Directory.Recursive as SDR
import qualified Data.List as List
import Debug.Trace (trace)
import qualified Control.Monad as M
import qualified Data.Map as Map

main =  E.getArgs >>= run . packArgs . parseArgs
main' = readFile "test.jr" >>= putStrLn . compile

run args = case args of
        Args Nothing (Just fpath) opath 
            -> do f <- readFile fpath
                  let src = compile f
                  writeFile fname src
                where 
                    fname = case opath of 
                        Nothing -> oldfname <> ".rs"
                        Just p -> p 
                    oldfname = Utils.getFileNameFromPath fpath
        Args (Just dpath) Nothing opath
            -> do paths <- SDR.getFilesRecursive dpath 
                  let mods = M.mapM makeJrMod paths
                  let mods' = fmap (Map.elems . combineMods) mods
                  rsSrc <- fmap createRustFile mods'
                  writeFile outPath rsSrc
                where 
                    outPath = case opath of
                        Just p -> p
                        Nothing -> dpath <> ".rs"
        Args (Just dirpath) (Just fpath) _
            -> error "Cannot compile both a filename and a directory"
        _ -> error "Mmmmm wrong arg use"
        
data JrMod = JrMod { name :: String
                   , src  :: String }

combineMods :: [JrMod] -> Map.Map String JrMod
combineMods = foldl addToMap Map.empty
    where addToMap map nmod @ (JrMod name src') = 
            let fetch = Map.lookup name map
            in case fetch of 
                Just ogmod -> Map.insert name (ogmod { src = (src ogmod) <> src' }) map
                Nothing -> Map.insert name nmod map 

createRustFile :: [JrMod] -> String
createRustFile = foldl addModToRsFile startSrc
    where startSrc = "use lib::*;\n\n"
          addModToRsFile rs jr = rs <> "\nmod " <> name jr <> " {\n\n" 
                <> modHeader <> "\n\n" <> src jr <> "}\n\n"

makeJrMod :: FilePath -> IO JrMod
makeJrMod path = do
    src <- fmap compile (readFile path)
    let fname = case Utils.stripSuffix ".jr" $ Utils.getFileNameFromPath path of
            Just n -> n
            Nothing -> error "Filename for some reason did not end with .jr"
    let mpath = Utils.splitExc (=='.') fname
    if length mpath < 3 
    then return $ JrMod (head mpath) src
    else error "Jr filenames can only contain one mod name and one secundary name"

modHeader = "use crate::*; type Int = i32; type Int64 = i64; type Bool = bool; " 
            <> "type Float = f64; type Float32 = f32; type Uint = u32;"

parseArgs :: [String] -> [Arg]
parseArgs = parseArgs' []

parseArgs' :: [Arg] -> [String] -> [Arg]
parseArgs' args sargs = case sargs of
        "-dir" : path : rest -> parseArgs' (DirPath path : args) rest
        "-file" : path : rest -> parseArgs' (FilePath path : args) rest
        "-out" : path : rest -> parseArgs' (OutputPath path : args) rest
        arg : rest -> error $ "Could not parse program args because of '" <> arg <> "'"
        [] -> args

packArgs :: [Arg] -> Args
packArgs = foldl updateArgs (Args Nothing Nothing Nothing)
    where updateArgs args arg = case arg of
            DirPath path -> args { dirpath = Just path }
            FilePath path -> args { filepath = Just path }
            OutputPath path -> args { outpath = Just path }

data Arg =
      DirPath String
    | FilePath String
    | OutputPath String
    deriving Show

data Args =
    Args { dirpath  :: Maybe String
         , filepath :: Maybe String
         , outpath  :: Maybe String }
    deriving (Show)


compile :: String -> String
compile = Write.writeMod . Parse.buildTree . Lex.tokenize


