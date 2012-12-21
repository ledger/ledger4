{-# LANGUAGE OverloadedStrings #-}

module Ledger.Parser.Text
       ( parseJournalFile
       , RawJournal(..)
       , RawEntity(..)
       , RawEntityInSitu(..)
       , RawPosting(..)
       , RawTransaction(..)
       , RawAutoTxn(..)
       , RawPeriodTxn(..)
       , main
       ) where

import Control.Applicative
import Data.ByteString as B hiding (pack, unpack, singleton, zipWith)
import Data.Maybe
import Data.Text as T hiding (zipWith)
import Data.Text.Encoding as E
import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath, readFile, until)
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta
import Text.Trifecta.Delta

infixl 4 <$!>

(<$!>) :: TokenParsing m => (a -> b) -> m a -> m b
f <$!> ma = ($!) <$> pure f <*> ma

data RawJournal = RawJournal [RawEntity]
                deriving (Show, Eq)

data RawEntity = Whitespace Text
               | FileComment Text
               | Directive { directiveChar :: Maybe Char
                           , directiveName :: Text
                           , directiveArg  :: Maybe Text }
               | RawTransactionEntity RawTransaction
               | RawAutoTxnEntity RawAutoTxn
               | RawPeriodTxnEntity RawPeriodTxn
               | EndOfFile
               deriving (Show, Eq)

data RawEntityInSitu = RawEntityInSitu { rawEntityIndex    :: Int
                                       , rawEntityStartPos :: Rendering
                                       , rawEntity         :: RawEntity
                                       , rawEntityEndPos   :: Rendering }

instance Show RawEntityInSitu where
  show x = show (rawEntity x) ++ "\n"

data RawPosting = RawPosting { rawPostState           :: Maybe Char
                             , rawPostAccount         :: Text
                             , rawPostAmount          :: Maybe Text
                             , rawPostNote            :: Maybe Text }
                | RawPostingNote Text
                deriving (Show, Eq)

data RawTransaction = RawTransaction { rawTxnDate    :: Text
                                     , rawTxnDateAux :: Maybe Text
                                     , rawTxnState   :: Maybe Char
                                     , rawTxnCode    :: Maybe Text
                                     , rawTxnDesc    :: Text
                                     , rawTxnNote    :: Maybe Text
                                     , rawTxnPosts   :: [RawPosting] }
                    deriving (Show, Eq)

data RawAutoTxn = RawAutoTxn { rawATxnQuery :: Text
                             , rawATxnPosts :: [RawPosting] }
                deriving (Show, Eq)

data RawPeriodTxn = RawPeriodTxn { rawPTxnPeriod :: Text
                                 , rawPTxnPosts  :: [RawPosting] }
                  deriving (Show, Eq)

txnDateParser :: TokenParsing m => m Text
txnDateParser = pack <$!> some (digit <|> oneOf "/-." <|> letter)
                     <?> "transaction date"

longSep :: CharParsing m => m ()
longSep = (string "  " *> pure ())
          <|> (tab *> pure ())
          <|> (char ' ' *> tab *> pure ())
          <?> "long separator"

noteParser :: CharParsing m => m Text
noteParser = pack <$> (char ';' *> manyTill anyChar (try (lookAhead endOfLine)))
                  <?> "note"

longSepOrEOL :: CharParsing m => m ()
longSepOrEOL = try (lookAhead (longSep <|> endOfLine))

longSepOrEOLIf :: CharParsing m => m p -> m ()
longSepOrEOLIf p = try (lookAhead ((longSep *> p *> pure ()) <|> endOfLine))

until :: CharParsing m => m () -> m Text
until end = pack <$> ((:) <$> noneOf "\r\n" <*> manyTill anyChar end)

tokenP :: TokenParsing m => m p -> m p
tokenP p = p <* skipMany spaceChars

postingParser :: TokenParsing m => m RawPosting
postingParser =
  (RawPosting <$!> (some spaceChars *>
                   optional (tokenP (char '*' <|> char '!')))
              <*> tokenP (until longSepOrEOL)
              <*> optional (tokenP (until longSepOrEOL))
              <*> (optional (noteParser) <* endOfLine)
              <?> "posting")
  <|>
  (RawPostingNote <$!> (T.concat <$!>
                       some (T.append <$!> (some spaceChars *> noteParser)
                                      <*> endOfLineText))
                  <?> "posting note")

spaceChars :: CharParsing m => m ()
spaceChars = oneOf " \t" *> pure ()

regularTxnParser :: TokenParsing m => m RawEntity
regularTxnParser = RawTransactionEntity <$!> go
  where go = RawTransaction
             <$!> txnDateParser
             <*> optional (char '=' *> txnDateParser)
             <*> (many spaceChars *>
                  optional (tokenP (char '*' <|> char '!')))
             <*> optional
                 (tokenP (parens (pack <$!> many (noneOf ")\r\n"))))
             <*> tokenP (until (longSepOrEOLIf (char ';')))
             <*> optional noteParser
             <*> (endOfLine *> some postingParser)
             <?> "regular transaction"

automatedTxnParser :: TokenParsing m => m RawEntity
automatedTxnParser = RawAutoTxnEntity <$!> go
  where go = RawAutoTxn
             <$!> (tokenP (char '=') *>
                  (pack <$!> manyTill anyChar (try (lookAhead endOfLine))))
             <*> (endOfLine *> some postingParser)
             <?> "automated transaction"

periodicTxnParser :: TokenParsing m => m RawEntity
periodicTxnParser = RawPeriodTxnEntity <$!> go
  where go = RawPeriodTxn
             <$!> (tokenP (char '~') *>
                  (pack <$!> manyTill anyChar (try (lookAhead endOfLine))))
             <*> (endOfLine *> some postingParser)
             <?> "periodic transaction"

transactionParser :: TokenParsing m => m RawEntity
transactionParser = regularTxnParser
                    <|> automatedTxnParser
                    <|> periodicTxnParser
                    <?> "transaction"

directiveParser :: TokenParsing m => m RawEntity
directiveParser =
  Directive <$!> optional (oneOf "@!")
            <*> (pack <$!> ((:) <$!> letter <*> tokenP (many alphaNum)))
            <*> (optional
                 (pack <$!>
                  ((:) <$!> noneOf "\r\n"
                       <*> manyTill anyChar (try (lookAhead endOfLine))))
                 <* endOfLine)
            <?> "directive"

endOfLine :: CharParsing m => m ()
endOfLine = skipOptional (char '\r') *> char '\n' *> pure ()

endOfLineText :: CharParsing m => m Text
endOfLineText = (pack <$> string "\r\n")
            <|> (singleton <$> char '\n')
            <?> "end of line"

commentParser :: TokenParsing m => m RawEntity
commentParser = FileComment
                <$!> (T.concat <$!>
                     some (T.append <$!> noteParser <*> endOfLineText))
                <?> "comment"

whitespaceParser :: TokenParsing m => m RawEntity
whitespaceParser = Whitespace . pack <$!> some space <?> "whitespace"

entityParser :: TokenParsing m => m RawEntity
entityParser = directiveParser
               <|> commentParser
               <|> whitespaceParser
               <|> transactionParser
               <?> "journal"

rendCaret :: DeltaParsing m => m Rendering
rendCaret = addCaret <$!> position <*> rend

journalParser :: DeltaParsing m => m [RawEntityInSitu]
journalParser =
  many (RawEntityInSitu <$!> pure 0 <*> rendCaret <*> entityParser <*> rendCaret)

parseJournalFile :: FilePath -> ByteString -> Result [RawEntityInSitu]
parseJournalFile file contents =
  let filepath = either id id $ toText file
      start    = Directed (encodeUtf8 filepath) 0 0 0 0
  in zipWith (\e i -> e { rawEntityIndex = i})
       <$> parseByteString journalParser start contents
       <*> pure [1..]

testme :: IO (Result [RawEntityInSitu])
testme =
  let file = "/Users/johnw/Documents/Finances/ledger.dat"
  in parseJournalFile (fromText (pack file)) <$> B.readFile file

main = do x <- testme
          case x of
            Success x -> print $ Prelude.take 10 x

-- Text.hs ends here
