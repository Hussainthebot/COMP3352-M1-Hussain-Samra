module X86Passes.WriteToFile where

writeToFile :: FilePath -> String -> IO ()
writeToFile fp s = writeFile fp s

writeToStdio :: String -> IO ()
writeToStdio = putStrLn
