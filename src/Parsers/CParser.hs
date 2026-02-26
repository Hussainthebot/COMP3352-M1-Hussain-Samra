module Parsers.CParser where

cParseFile :: FilePath -> IO (Either String ())
cParseFile _ = pure (Left "C parsing not available in this project (no C parser provided).")
