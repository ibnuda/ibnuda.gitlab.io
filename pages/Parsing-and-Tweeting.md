Parsing and Tweeting
636509092519324977

Parsing and Tweeting
====================

At `$WORK`, I'm getting a lot of information that have to be parsed in form of
text file.
Whether it's from email, log files, and stuffs like that.
So, I guess I have to automate those information parsing to ease my life a bit.

Here's what I've done.
- Read file.
- Parse stuff.
- Spit out those parsed stuff to whatever I want.

And for this article, I want to put the parsed information of a dumped-archive of
a whatsapp chat group, create a sequence of random words using markov-chain,
and then tweet it.
(The tweet part is just a cherry on top, actually)

### Data Format
Basically, a dumped archive of WhatsApp chat looks like the following format.
```
{DateTime}{CommaSeparator}{Whitespace}{Dash}{Whitespace}{Sender}{Colon}{Whitespace}{Message}
```
where
- `{DateTime}` is the date and time of the message sent, minus the second part.
- `{CommaSeparator}` is a char of `,`.
- `{Whitespace}` is a char of ` `.
- `{Dash}` is a char of `-`.
- `{Sender}`, as far as I know, is an array of valid utf8 characters.
- `{Colon}` is a char of `:`.
- `{Message}` could be a multiline array of utf8 characters.
  While it could contains medias, we will ommit it for the sake of simplicity.
```
20/01/2018, 10:10 - Ibnu Daru Aji: This is a message.
20/01/2018, 10:11 - Ibnu Daru Aji: This is a message.

```

### Parsing
We will use haskell package, `attoparsec`, to parse it.
The following snippet parses the `DateTime` token:
```
import qualified Data.Attoparsec.ByteString.Char8 as BC

ParserDatum :: Parser UTCTime
parserDatum = do
  dd <- count 2 BC.digit
  _ <- BC.char '/'
  mm <- count 2 BC.digit
  _ <- BC.char '/'
  yyyy <- count 4 BC.digit
  _ <- string ", "
  hh <- count 2 BC.digit
  _ <- BC.char ':'
  m <- count 2 BC.digit
  _ <- string " - "
  pure $
    UTCTime
    { utctDay = fromGregorian (read yyyy) (read mm) (read dd)
    , utctDayTime = secondsToDiffTime $ (read hh) * 3600 + (read m) * 60
    }
```
The reason why I use `UTCTime` is I'm familiar with it and there's no particular
constraints that I have.
And then `dd`, `mm`, `yyyy`, `hh`, and `m` are the parsed parts of the `{DateTime}`
token of the messages, respectively.
Each parts with `count` function mean that we have to take `n` char of `digit`.
There's a few things that should be given attentions, for example, I skipped a few
characters and there's no seconds part.
Finally, we will return a parser that return an instance of `UTCTime`.

```
parserVerzender :: Parser ByteString
parserVerzender = do
  BC.takeTill (== ':')

parserPraat :: Parser ByteString
  rest <- BC.takeTill (== '\n')
  end <- atEnd
  if end
    then pure rest
    else (BC.char '\n') >> pure rest
```
The functions in the snippet above are used to parse `{Sender}` and `{Message}`.
Basically, the `parserVerzender` only takes characters until `:` char and `parserPraat`
takes characters until a new line.

```
parserBericht :: Parser Bericht
parserBericht = do
  date <- parserDatum
  crimineel <- parserVerzender
  _ <- take 2
  a <- parserPraat
  b <- manyTill parserPraat $ endOfInput <|> isDatumAhead
  pure $ Bericht date crimineel $ concat $ splitAtSpace a : map splitAtSpace b

isDatumAhead :: Parser ()
isDatumAhead = lookAhead parserDatum *> pure ()

splitAtSpace :: ByteString -> [ByteString]
splitAtSpace = BS.split (' ')

data Bericht = Bericht
  { datum   :: UTCTime
  , sender  :: ByteString
  , content :: [ByteString]
  }
```
`parserBericht` function combines
- `parserDatum` to parse date.
- `parserVerzender` to parse sender.
- skip two characters.
- takes many lines until end of input or the first few characters of a new line
  could be parsed to a `{DateTime}`.
- then, returns a `Bericht` object.
`isDatumAhead` is a parser that look a head whether the next input could be parsed
as `DateTime` or not.
And the reason why there's a `splitAtSpace` is because we want it as per words
for the input of markov chain in the next section.

And because there are many messages, we will create type and parser for that.
```
type ChatLog = [Bericht]
parserChatLog = many parserBericht
```

### Cherries on Top
We will use `markov-chain` package to generate the data we will tweet.
```
import qualified Data.Attoparsec.Lazy as AP
import qualified Data.ByteString as B
import Data.MarkovChain
import System.Random

parseFile :: FilePath -> IO ChatLog
parseFile filename = do
  fileContent <- B.readFile filename
  case AP.parseOnly parserChatLog fileContent of
    Right chats -> return chats
    Left _      -> pure []

generateBullshit :: [[ByteString]] -> StdGen -> ByteString
generateBullshit fileContents randomSeed =
  B.intercalate " " $ take 20 $ concat $ runMulti 1 fileContent 0 randomSeed
```
We will parse the file 
And use the [sample](https://github.com/himura/twitter-conduit/tree/master/sample)
of the `twitter-conduit` package to tweet the result of `generateBullshit` function.

### Main Function
```
mainFunc :: IO ()
mainFunc = do
  randomSeed <- getStdGen
  twInfo <- getTwInfoFromProxy
  mgr <- newManager tlsManagerSettings
  args <- getArgs
  case args of
    file:namen -> do
      fileContent <- parseFile file
      let chats =
            (map content) . (filter (\bericht -> sender bericht `elem` map BC.pack namen)) $
            fileContent
      let bullshit = generateBullshit chats randomSeed
      putStrLn $ T.decodeUtf8 bullshit
      res <- call twInfo mgr $ update $ T.decodeUtf8 bullshit
      print res
    _ -> do
      putStrLn ("<this program> <file archive> <usernames>" :: Text)
      exitFailure
```
That function will:
- create a random seed.
- create twinfo (from the samples of `twitter-conduit`)
- create tls manager.
- get arguments from command line.
- when the arguments are less than than two, the program will error.
- when the argements more than two, the program will:
  - take the first argument as the filename of the caht archive.
  - take the rest of the arguments to decide which sender's messages which
    will be used as the input of `generateBullshit` function.
  - tweet the result of generated chain.

All in all, it was a nice learning experience for a short weekend.
You can read the whole program [here](https://gitlab.com/ibunda/Tiwik).
