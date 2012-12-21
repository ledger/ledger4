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
       ) where

import Control.Applicative
import Data.ByteString as B hiding (pack, unpack, singleton, zipWith)
import Data.Char (isSpace)
import Data.Maybe
import Data.Text as T hiding (zipWith)
import Data.Text.Encoding as E
import Debug.Trace (trace)
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath, readFile)
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta
import Text.Trifecta.Delta

infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a

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
                             , rawPostCost            :: Maybe (Text,Text)
                             , rawPostAssignedBalance :: Maybe Text
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
txnDateParser = pack <$> some (digit <|> oneOf "/-." <|> letter)
                     <?> "transaction date"

longSep :: CharParsing m => m ()
longSep = (string "  " *> pure ())
          <|> (tab *> pure ())
          <|> (char ' ' *> tab *> pure ())
          <?> "long separator"

nonNL :: CharParsing m => m Text
nonNL = pack <$> many (noneOf "\r\n")
             <?> "non-newline"

noteParser :: CharParsing m => m Text
noteParser = pack <$> (char ';' *> manyTill anyChar (try (lookAhead endOfLine)))
                  <?> "note"

untilLongSepOrEOL :: CharParsing m => m Text
untilLongSepOrEOL =
  pack <$> ((:) <$> noneOf "\r\n"
                <*> manyTill anyChar (try (lookAhead (longSep <|> endOfLine))))
       <?> "text before long-separator"

untilLongSepOrEOLOr :: CharParsing m => m p -> m Text
untilLongSepOrEOLOr p = pack <$> ((:) <$> noneOf "\r\n"
                                      <*> manyTill anyChar end)
                             <?> "text before long-separator"
  where end = try (lookAhead (longSep <|> endOfLine <|> (p *> pure ())))

tokenP :: TokenParsing m => m p -> m p
tokenP p = p <* skipMany spaceChars

postingParser :: TokenParsing m => m RawPosting
postingParser =
  (RawPosting
  <$> (some spaceChars *>
       optional (tokenP (char '*' <|> char '!')))
  <*> tokenP untilLongSepOrEOL
  <*> optional (tokenP (untilLongSepOrEOLOr (some (oneOf "@="))))
  <*> optional
      ((,) <$> (T.filter (not . isSpace) . pack <$>
                some (oneOf "@= "))
           <*> tokenP (untilLongSepOrEOLOr (char '=')))
  <*> optional (char '=' *> untilLongSepOrEOL)
  <*> (optional (longSep *> noteParser) <* endOfLine)
  <?> "posting")

  <|>

  (RawPostingNote
  <$> (T.concat <$>
       some (T.append <$> (some spaceChars *> noteParser)
                      <*> endOfLineText))
  <?> "posting note")

spaceChars :: CharParsing m => m ()
spaceChars = oneOf " \t" *> pure ()

anySpaceChars :: CharParsing m => m ()
anySpaceChars = oneOf " \t\r\n" *> pure ()

regularTxnParser :: TokenParsing m => m RawEntity
regularTxnParser = RawTransactionEntity <$> go
  where go = RawTransaction
             <$> txnDateParser
             <*> optional (char '=' *> txnDateParser)
             <*> (many spaceChars *>
                  optional (tokenP (char '*' <|> char '!')))
             <*> optional (tokenP (parens nonNL))
             <*> tokenP untilLongSepOrEOL
             <*> optional noteParser
             <*> (endOfLine *> some postingParser)
             <?> "regular transaction"

automatedTxnParser :: TokenParsing m => m RawEntity
automatedTxnParser = RawAutoTxnEntity <$> go
  where go = RawAutoTxn
             <$> (tokenP (char '=') *>
                  (pack <$> manyTill anyChar (try (lookAhead endOfLine))))
             <*> (endOfLine *> some postingParser)
             <?> "automated transaction"

periodicTxnParser :: TokenParsing m => m RawEntity
periodicTxnParser = RawPeriodTxnEntity <$> go
  where go = RawPeriodTxn
             <$> (tokenP (char '~') *>
                  (pack <$> manyTill anyChar (try (lookAhead endOfLine))))
             <*> (endOfLine *> some postingParser)
             <?> "periodic transaction"

transactionParser :: TokenParsing m => m RawEntity
transactionParser = regularTxnParser
                    <|> automatedTxnParser
                    <|> periodicTxnParser
                    <?> "transaction"

directiveParser :: TokenParsing m => m RawEntity
directiveParser =
  Directive <$> optional (oneOf "@!")
            <*> (pack <$> ((:) <$> letter <*> tokenP (many alphaNum)))
            <*> (optional
                 (pack <$>
                  ((:) <$> noneOf "\r\n"
                       <*> manyTill anyChar (try (lookAhead endOfLine))))
                 <* endOfLine)
            <?> "directive"

endOfLine :: CharParsing m => m ()
endOfLine = (string "\r\n" *> pure ())
            <|> (char '\n' *> pure ())
            <?> "end of line"

endOfLineText :: CharParsing m => m Text
endOfLineText = (pack <$> string "\r\n")
            <|> (singleton <$> char '\n')
            <?> "end of line"

commentParser :: TokenParsing m => m RawEntity
commentParser = FileComment
                <$> (T.concat <$>
                     some (T.append <$> noteParser <*> endOfLineText))
                <?> "comment"

whitespaceParser :: TokenParsing m => m RawEntity
whitespaceParser = Whitespace . pack <$> some space <?> "whitespace"

entityParser :: TokenParsing m => m RawEntity
entityParser = directiveParser
               <|> commentParser
               <|> whitespaceParser
               <|> transactionParser
               <?> "journal"

journalParser :: DeltaParsing m => m [RawEntityInSitu]
journalParser =
  many (RawEntityInSitu <$> pure 0 <*> rend <*> entityParser <*> rend)

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

-- Text.hs ends here
