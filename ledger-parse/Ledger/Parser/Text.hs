{-# LANGUAGE OverloadedStrings #-}

module Ledger.Parser.Text ( parseJournalFile ) where

import Control.Applicative
import Data.ByteString as B hiding (pack, unpack, singleton)
import Data.Char (isSpace)
import Data.Maybe
import Data.Text as T
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
               | RawTxnEntity RawTxn
               | RawAutoTxnEntity RawAutoTxn
               | RawPeriodTxnEntity RawPeriodTxn
               | EndOfFile
               deriving (Show, Eq)

data RawEntityInSitu = RawEntityInSite { rawEntityIndex   :: Int
                                       , rawEntityAbsPath :: Text
                                       , rawEntityLineNo  :: Int
                                       , rawEntity        :: RawEntity }
                     deriving (Show, Eq)

data RawPost = RawPost { rawPostState           :: Maybe Char
                       , rawPostAccount         :: Text
                       , rawPostAmount          :: Maybe Text
                       , rawPostCost            :: Maybe (Text,Text)
                       , rawPostAssignedBalance :: Maybe Text
                       , rawPostNote            :: Maybe Text }
             deriving (Show, Eq)

data RawTxn = RawTxn { rawTxnDate    :: Text
                     , rawTxnDateAux :: Maybe Text
                     , rawTxnState   :: Maybe Char
                     , rawTxnCode    :: Maybe Text
                     , rawTxnDesc    :: Text
                     , rawTxnNote    :: Maybe Text
                     , rawTxnPosts   :: [RawPost] }
            deriving (Show, Eq)

data RawAutoTxn = RawAutoTxn { rawATxnQuery :: Text
                             , rawATxnNote  :: Maybe Text
                             , rawATxnPosts :: [RawPost] }
                deriving (Show, Eq)

data RawPeriodTxn = RawPeriodTxn { rawPTxnPeriod :: Text
                                 , rawPTxnNote   :: Maybe Text
                                 , rawPTxnPosts  :: [RawPost] }
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

postingParser :: TokenParsing m => m RawPost
postingParser =
  RawPost <$> (some spaceChars *>
               optional (tokenP (char '*' <|> char '!')))
          <*> tokenP untilLongSepOrEOL
          <*> optional (tokenP (untilLongSepOrEOLOr (some (oneOf "@="))))
          <*> optional
              ((,) <$> (T.filter (not . isSpace) . pack <$> some (oneOf "@= "))
                   <*> tokenP (untilLongSepOrEOLOr (char '=')))
          <*> optional (char '=' *> untilLongSepOrEOL)
          <*> (optional (longSep *> noteParser) <* endOfLine)
          <?> "posting"

spaceChars :: CharParsing m => m ()
spaceChars = oneOf " \t" *> pure ()

anySpaceChars :: CharParsing m => m ()
anySpaceChars = oneOf " \t\r\n" *> pure ()

regularTxnParser :: TokenParsing m => m RawEntity
regularTxnParser = RawTxnEntity <$> go
  where go = RawTxn
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
automatedTxnParser = undefined

periodicTxnParser :: TokenParsing m => m RawEntity
periodicTxnParser = undefined

transactionParser :: TokenParsing m => m RawEntity
transactionParser = regularTxnParser
                    <|> automatedTxnParser
                    <|> periodicTxnParser
                    <?> "transaction"

directiveParser :: TokenParsing m => m RawEntity
directiveParser =
  Directive <$> optional (oneOf "@!")
            <*> (pack <$> ((:) <$> letter <*> many alphaNum))
            <*> (skipMany spaceChars *>
                 optional
                 (pack <$> manyTill anyChar (try (lookAhead endOfLine)))
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

entityParser :: DeltaParsing m => m (RawEntity,Delta)
entityParser = (,) <$> rawEntityParser <*> position
  where rawEntityParser = directiveParser
                          <|> commentParser
                          <|> whitespaceParser
                          <|> transactionParser
                          <?> "journal"

parseJournalFile :: FilePath -> ByteString -> [RawEntity]
parseJournalFile file contents =
  let filepath = either id id $ toText file
  in go contents (Directed (encodeUtf8 filepath) 0 0 0 0)
  where go bs delt =
          case parseByteString entityParser delt bs of
            Success (result',delt') -> result' : go bs delt'
            Failure err -> error (show err)

-- Text.hs ends here
