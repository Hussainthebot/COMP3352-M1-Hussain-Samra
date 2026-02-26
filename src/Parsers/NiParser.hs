module Parsers.NiParser (niParseFile, niParseString) where

import Data.Int (Int64)
import N1
import Text.ParserCombinators.ReadP
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Control.Applicative (optional)

-- =========================
-- Public API
-- =========================

niParseFile :: FilePath -> IO (Either String Program)
niParseFile fp = do
  s <- readFile fp
  pure (niParseString s)

niParseString :: String -> Either String Program
niParseString input =
  case readP_to_S (skipWS *> pProgram <* skipWS <* eof) input of
    [(prog, "")] -> Right prog
    _            -> Left "Ni parse error: could not parse program."

-- =========================
-- Whitespace & Comments
-- =========================

-- Skip whitespace and // line comments, repeatedly
skipWS :: ReadP ()
skipWS = do
  skipSpaces
  s <- look
  case s of
    ('/':'/':_) -> lineComment >> skipWS
    _           -> pure ()

lineComment :: ReadP ()
lineComment = do
  _ <- string "//"
  _ <- munch (/= '\n')
  -- consume the newline if present, without using optional
  s <- look
  case s of
    ('\n':_) -> char '\n' >> pure ()
    _        -> pure ()

-- At least one whitespace character or a comment counts
skipWS1 :: ReadP ()
skipWS1 = do
  s <- look
  case s of
    ('/':'/':_) -> lineComment >> skipWS
    (c:_) | isSpace c -> skipWS
    _ -> pfail

-- =========================
-- Reserved Words
-- =========================

reserved :: [String]
reserved = ["let", "ni", "is", "in", "end", "read"]

-- Match a keyword, ensuring it's not a prefix of an identifier
pKeyword :: String -> ReadP ()
pKeyword kw = do
  _ <- string kw
  s <- look
  case s of
    (c:_) | isAlphaNum c || c == '_' -> pfail
    _ -> pure ()

-- =========================
-- Grammar
-- =========================

pProgram :: ReadP Program
pProgram = Program <$> pExp

-- Full expression: try let first (most complex), then addition/atoms
pExp :: ReadP Exp
pExp = pLet <++ pAdd

-- Atoms: things that don't involve infix or prefix structure
pAtom :: ReadP Exp
pAtom =
      pRead
  <++ pInt
  <++ pVar
  <++ parens pExp

-- -------- integers --------

pInt :: ReadP Exp
pInt = Int <$> pUnsignedInt

pUnsignedInt :: ReadP Int64
pUnsignedInt = do
  ds <- munch1 (\c -> c >= '0' && c <= '9')
  pure (read ds)

-- -------- variables --------

pVar :: ReadP Exp
pVar = Var <$> pIdent

pIdent :: ReadP String
pIdent = do
  first <- satisfy isAlpha
  rest  <- munch (\c -> isAlphaNum c || c == '_')
  let name = first : rest
  if name `elem` reserved then pfail else pure name

-- -------- read --------

pRead :: ReadP Exp
pRead = do
  pKeyword "read"
  pure Read

-- -------- negation --------

pNeg :: ReadP Exp
pNeg = do
  _ <- char '-'
  skipWS
  e <- pNegAtom
  pure (Negate e)

-- Things that can follow a unary minus
pNegAtom :: ReadP Exp
pNegAtom = parens pExp <++ pNeg <++ pUnsignedIntExp

pUnsignedIntExp :: ReadP Exp
pUnsignedIntExp = Int <$> pUnsignedInt

-- -------- addition --------

pAdd :: ReadP Exp
pAdd = chainl1 pTerm addOp
  where
    pTerm = pNeg <++ pLet <++ pAtom
    addOp = do
      skipWS
      _ <- char '+'
      skipWS
      pure Add

-- -------- let syntax --------

pLet :: ReadP Exp
pLet = do
  pKeyword "let"
  skipWS
  pKeyword "ni"
  skipWS1
  name <- pIdent
  skipWS1
  pKeyword "is"
  skipWS1
  bound <- pExp
  skipWS1
  pKeyword "in"
  skipWS1
  body <- pExp
  skipWS
  pKeyword "end"
  pure (Let name bound body)

-- -------- helpers --------

parens :: ReadP a -> ReadP a
parens p = between (char '(' *> skipWS) (skipWS *> char ')') p