import Control.Applicative (liftA2, (<$>))
import Control.Monad (when,forM)
import Data.Char (isSpace)
import Data.List (nub)
import Data.List (isPrefixOf)
import Data.List.Split (splitOneOf)
import Development.Shake
import Development.Shake.FilePath
import System.Directory (removeFile)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do

    want ["thesis.pdf"]

    "*.pdf" %> \pdf -> do
        need =<< readFileLines ("_build" </> pdf -<.> "deps")
        let tex = pdf -<.> "tex"
        opt <- addEnv [("TEXINPUTS","_build//:")]
        command_ [opt] "latexmk" ["-xelatex","-outdir=_build",tex]
        moveFile ("_build" </> pdf) pdf

    "_build//*.tex" %> \tex -> do
        let lhs = dropDirectory1 $ tex -<.> "lhs"
        let gnu = lhs -<.> "gnuplot"
        isPlot <- doesFileExist gnu
        if isPlot
            then do
                need [gnu]
                let out = "set output '" ++ tex -<.> "eps" ++ "';"
                        ++ "set loadpath '" ++ takeDirectory1 gnu ++ "'"
                cmd "gnuplot -e" [out] [gnu]
            else do
                need [lhs]
                cmd "lhs2tex -o" [tex] [lhs]

    "_build//*.deps" %> \out -> do
        let tex = dropDirectory1 $ out -<.> "tex"
        me <- tex `orElse` return ("_build" </> tex)
        dep <- readFileLines $ out -<.> "dep"
        deps <- mapM (readFileLines . ("_build" </>) . (<.> "deps")) dep
        writeFileLines out $ nub $ me : concat deps

    "_build//*.dep" %> \out -> do
        let tex = dropDirectory1 $ out -<.> "tex"
        let lhs = tex -<.> "lhs"
        let gnu = tex -<.> "gnuplot"
        me <- tex `orElse` (gnu `orElse` return lhs)
        includes <- getIncludes me
        writeFileLines out includes

    phony "clean" $ do
        removeFilesAfter "_build" ["//*"]

orElse :: FilePath -> Action FilePath -> Action FilePath
orElse a b = doesFileExist a >>= \e -> if e then return a else b

getIncludes :: FilePath -> Action [FilePath]
getIncludes tex = map ((!!1) . splitOneOf "{}")
                . filter isInclude
              <$> readFileLines tex
  where
    isInclude = liftA2 (||) ("\\include" `isPrefixOf`)
                            ("\\input" `isPrefixOf`)
              . dropWhile isSpace

-- for the benefit of preview apps, the pdf is actually updated in-place
moveFile :: FilePath -> FilePath -> Action ()
moveFile fp1 fp2 = do
    command_ [] "sh" ["-c", "cat " ++ fp1 ++ " > " ++ fp2]
    liftIO $ removeFile fp1
