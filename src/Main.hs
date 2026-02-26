module Main where

import Data.Foldable
import Control.Monad
import Options.Applicative as A

import Parsers.NiParser as Ni
import Parsers.CParser as C
import ComposePasses
import X86Passes.WriteToFile

data CompOpts = CompOpts
  { lang :: String
  , showAst :: Bool
  , outFile :: String
  , files :: [String]
  }

compOpts :: A.Parser CompOpts
compOpts = CompOpts
  <$> strOption
      (  long "language"
      <> short 'l'
      <> metavar "LANGUAGE"
      <> value "ni"
      <> help "Source language to be compiled")
  <*> flag False True
      (  long "ast"
      <> short 'a'
      <> help "Print the AST" )
  <*> strOption
      (   long "outFile"
      <>  short 'o'
      <>  metavar "OUTFILE"
      <>  value ""
      <>  help "file name to write output to"
      )
  <*> some (argument str
      ( metavar "FILES..."
      <> help "Source file(s) to compile"))

opts :: ParserInfo CompOpts
opts = info (compOpts <**> helper)
  ( fullDesc
  <> progDesc "Compiles Ni programs"
  <> header "nic - a compiler for Ni programs"
  )

main :: IO ()
main = do
  CompOpts { lang = lang
           , showAst = showAst
           , outFile = outFile
           , files = files } <- execParser opts

  -- If -o is given, require exactly one input file (avoids overwriting/confusion)
  when (outFile /= "" && length files /= 1) $ do
    error "When using -o/--outFile, provide exactly ONE input file."

  forM_ files $ \arg -> do
    putStr $ "Parsing " ++ arg ++ "..."

    if lang == "ni" then do
      res <- Ni.niParseFile arg
      case res of
        Left err -> do
          putStrLn "Failed."
          putStrLn err
        Right ast -> do
          putStrLn "Done."
          when showAst $ print ast

          case allPasses ast of
            Left err -> putStrLn err
            Right finalProg -> do
              let output = show finalProg
              if outFile == "" then
                writeToStdio output
              else
                writeToFile outFile output

    else if lang == "c" then do
      res <- C.cParseFile arg
      case res of
        Left err -> do
          putStrLn "Failed."
          putStrLn err
        Right ast -> do
          putStrLn "Done."
          when showAst $ print ast

    else
      putStrLn "language must be 'ni' or 'c'"
