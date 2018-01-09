-- A file to help you get started with the programming puzzle.

import Text.PrettyPrint(Doc, render, parens, (<>), text, hang)
import Text.ParserCombinators.ReadP(ReadP, between, char, skipSpaces, munch1, many1, string, (+++), (<++), readP_to_S)
import Data.Char(isLower)

-- Lambda calculus terms.
data Term
  = Term :@: Term
  | Var String
  | Lam String Term
    -- Extra terms for successor and constant integers.
  | Succ | Num Integer
  deriving Eq

infixl 7 :@:

-- Normalise a term in a clever way?
normalise :: Term -> Term
normalise t = error "not implemented"

-- Evaluate a Church numeral.
evaluateChurch :: Term -> Integer
evaluateChurch t =
  case normalise (t :@: Succ :@: Num 0) of
    Num n -> n
    _ -> error "term did not reduce to value"

-- Pretty-printing.
pretty :: Bool -> Term -> Doc
pretty _ (Var x) = text x
pretty False (t :@: u) = hang (pretty False t) 2 (pretty True u)
pretty False t@Lam{} =
  text ("\\" ++ unwords (args t) ++ ".") <> pretty False (body t)
  where
    args (Lam x t) = x:args t
    args _ = []
    body (Lam _ t) = body t
    body t = t
pretty True t = parens (pretty False t)

instance Show Term where
  show = render . pretty False

instance Read Term where
  readsPrec _ = readP_to_S readTerm

-- Parsing.
parse :: String -> Term
parse = read

readTerm :: ReadP Term
readTerm = atomic +++ nonatomic
  where
    atomic = fmap Var id
         <++ bracketed readTerm
    nonatomic = lam <++ app
    lam = do
      token (char '\\')
      ids <- many1 id
      token (string ".")
      body <- readTerm
      return (foldr Lam body ids)
    app = do
      fun <- atomic
      args <- many1 atomic
      return (foldl (:@:) fun args)

    id = token (munch1 isLower)
    token p = do { skipSpaces; p }
    bracketed p = between (token (char '(')) (token (char ')')) p
