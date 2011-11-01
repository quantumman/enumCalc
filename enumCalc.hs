module EnumCalc ( repl
                , runRepl
                ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Attoparsec
import Data.Attoparsec.Char8
import Data.Attoparsec.Enumerator
import qualified Data.ByteString as B
import Data.ByteString.Char8 hiding (putStrLn, putStr)
import Data.Enumerator
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import System.IO hiding (putStrLn)


chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = chainl1 p op <|> return x

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do x <- p
                  rest x
    where
      rest x = (do f <- op
                   y <- p
                   rest $ f x y)
               <|> return x
               <?> "chainl"

natural :: Parser Number
natural = skipSpace *> number <* skipSpace <?> "natural"

symbol :: String -> Parser ByteString
symbol x = skipSpace *> (string $ pack x) <* skipSpace <?> "symbol"

operator :: String -> (a -> a -> a) -> Parser (a -> a -> a)
operator x f = symbol x *> return f <?> "operator"

terminate :: Parser a -> String -> Parser a
terminate p end = do x <- p
                     skipSpace
                     string $ pack end
                     return x

expr = expr' `terminate` "="
expr' = chainl1 term (operator "+" (+) <|> operator "-" (-))
term = chainl1 fact (operator "*" (*) <|> operator "/" (/)) <?> "term"
fact = natural <|> symbol "(" *> expr' <* symbol ")" <?> "fact"

calc :: ByteString -> Result Number
calc = parse expr

calcI :: Iteratee ByteString IO Number
calcI = iterParser expr

printI = liftIO . putStrLn . show
prompt = liftIO . putStr

enumStdin :: Enumerator ByteString IO b
enumStdin = EB.enumHandle 100 stdin

calculator :: Iteratee ByteString IO Number
calculator = enumStdin $$ calcI

repl =  prompt "$> " >> calculator >>= printI >> repl

runRepl = run_ repl