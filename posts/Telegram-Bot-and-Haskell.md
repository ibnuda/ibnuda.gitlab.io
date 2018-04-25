Telegram Bot and Haskell
2018-02-22 07:08:41.936771686 UTC


One of my friend asked for a help.
Though this time from a different person and on different problem.
We are going to build a bot for Telegram instant messenger service which talks to database.
For the sake of giving an example, let's say that we will create a telegram bot which
records the income and expense.
Words of caution: trust me, it's really hairy and ugly.

Okay, let's start the mess.

### Preparation

Just type `stack new OurBot new-template` into your terminal and then modify `stack.yaml`.
Insert the following lines in it
```
# stack.yaml
packages:
- .
- location:
    git: https://github.com/fizruk/telegram-bot-simple
    commit: c1cc6bbba14ca79c897586e2de28433193c0b9fd
  extra-dep: true

```
Which means that we will use that git repo (on that specified commit) on our program.
As for the reason, I failed to compile [`telegram-api`](https://hackage.haskell.org/package/telegram-api)
in the server I'm deploying to. 

And then we will modify `package.yaml` to include the repo above as one of our dependencies.

```
# package.yaml
dependencies:
- base >= 4.7 && < 5
- telegram-bot-simple

```
Where `telegram-bot-simple` is the name of the library which we previously included
in our `stack.yaml`.

Let's build it for a giggle by issuing `stack build` at the project's root directory.
It should compile just fine, I guess.

Next, we will create our domain model.

### Database Thingy

We will add a few dependencies in our `package.yaml`.

- `persistent` for talking to database.
- `persistent-postgresql` for talking to postgresql, specifically.
- `persistent-template`, it eases our life a bit. Though prolongs the compile time.
- `esqueleto` it's nice to join the tables in a <del>bar</del> database.
- `text`, I just like `Text`.
- `time`

Then, we will create a file named `Model.hs` in our `src` directory and include it
as one of our source files.

To simplify a bit, we will create two tables named `income` and `expense`.
The `income` table will have `source` (`Text`), `amount` (`Double`), and `when` (`UTCTime`) while `expense`
table will have `towhom` (`Text`), `amount` (`Double`), and `when` (`UTCTime`).

In order to accomplish that, we will edit `Model.hs` as the following
```
-- Model.hs
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Model where

import Data.Text
import Data.Time
import Database.Persist.TH
import Database.Persist.Sql

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Income
      source Text
      amount Double
      when   UTCTime
      deriving Show Eq
    Expense
      towhom Text
      amount Double
      when   UTCTime
      deriving Show Eq
  |]

```
On a second glance, I just realized that we use so many language extensions.
As for the reasons, apart from `QuasiQuotes` and `TemplateHaskell`, I don't really
know and just accept that GHC will fail to compile without them.
But if you insist, you can look at the [docs](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html)
about them.

Anyway, the snippet above, especially the `share` block, defines that we will
create tables name `income` and `expense`, which we have specified before the snippet.

And for the "migration plan", just a fancy way to say about creating and/or dropping
tables and/or database before the program talks to it, we will create a function
named `doMigration` in the same source file.
```
-- Model.hs
doMigration :: SqlPersistT IO ()
doMigration = runMigration migrateAll

```
If you wonder where did `migrateAll` come from, that value comes from the
`share` block above.
You can see `"migrateAll"` there. That's the magic of `Template Haskell`.

#### Read Write

Let's start by adding `monad-logger`, `monad-control`, and `transformers` in our
`package.yaml` with the reason of satisfying constraints of one of our functions.

We then create a source file named `ReadWrite.hs` on our `src` dir.
It will be really messy and we will have a lot of duplicated functions.
For example, a function like this:
```
-- ReadWrite.hs
insertIncome :: Text -> Double -> IO (Key Income)
insertIncome source amount = do
  now <- getCurrentTime
  runDb $ insert $ Income source amount now

insertExpense :: Text -> Double -> IO (Key Expense)
insertExpense towhom amount = do
  now <- getCurrentTime
  runDb $ insert $ Expense towhom amount now

```
To be honest, I don't really know how to reduce the duplication.
But that's another homework I have to solve for the next article, I guess.

Okay, let's import library for `ReadWrite.hs`.
```
-- ReadWrite.hs
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader

import Data.Text
import Data.Time
import Data.Maybe

import Database.Persist.Postgresql (withPostgresqlConn, createPostgresqlPool)
import Database.Esqueleto

```
As you've seen, there are a lot of `Control.Monad` imports.
Those libraries are used as the wrapper of computed data in our functions.
And the reason is the program we write communicates with `RealWorld(tm)` and
thus the data from/to our functions is safe and sound. Heheh.

Plus, don't forget to sprinkle some extensions so GHC can decide what kind of data
we are using in the program.
```
-- ReadWrite.hs
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

```
As usual, I really suggest you to read through GHC's documentation to get the
explanations we need (or want) over what those extension do (or don't).

Finally, we come to the simplest part of the program, read write information
to the database.
```
-- ReadWrite.hs
--| For executing queries.
runDb ::
     (MonadIO m, MonadBaseControl IO m)
  => ReaderT SqlBackend (LoggingT m) a
  -> m a
runDb query = do
  -- Yep, you read that right.
  -- There's password and username hardcoded for db in here.
  let con = "host=localhost port=5432 user=ibnu dbname=bot password=jaran"
  runStderrLoggingT $ withPostgresqlConn con $ \backend -> runReaderT query backend

doingMigration :: (MonadIO m, MonadBaseControl IO m, MonadLogger m) => m ()
doingMigration = do
  let con = "host=localhost port=5432 user=ibnu dbname=bot password=jaran"
  pool <- createPostgresqlPool con 10
  liftIO $ runSqlPool doMigration pool

```
The function above takes a query, which is a read-only access to the database.
"Read-only" what I mean here is, in a sense, a paved road built by government.
You can't modify it, yet you can freely use it.
Also, we have to use an instance of `LoggingT` for this function (by appending `runStderrLoggingT`)
to satisfy the demands from `withPostgresqlConn` so it can execute the query properly.

After we wrote the executor, we will write the functions to read and write from/to database.
The snippet about `insert` above pretty much enough for our need of insert at the moment.
And for querying, we will create a few functions.

```
-- ReadWrite.hs
searchIncomeBySource ::
  (MonadBaseControl IO m, MonadIO m) => Text -> m [Income]
searchIncomeBySource source = do
  incomes <-
    runDb $
    select $
    from $ \inc -> do
      where_ (inc ^. IncomeSource ==. val source)
      limit 10
      orderBy [desc (inc ^. IncomeWhen)]
      return inc
  return $ map entityVal incomes

```
The snippet above queries the database to select from table `income` which satisfy
the condition (`IncomeSource` equals to its argument) but only takes 10 rows and
orders it based on `when` column.

As I've complained above, about too many function duplications, we can create
a function for expense by replacing 4 token.
Just replace `Income` to `Expense` and presto! we have the function we want.

There's a really rough edge on this file, though.
```
totalExpense ::
     ( BaseBackend backend ~ SqlBackend
     , PersistUniqueRead backend
     , PersistQueryRead backend
     , IsPersistBackend backend
     , MonadIO m
     )
  => ReaderT backend m Double
totalExpense = do
  res <- select $ from $ \exp -> return $ joinV $ sum_ (exp ^. ExpenseAmount)
  return $ head $ map (fromJust . unValue) res

balance :: IO Double
balance = do
  inc <- runDb totalIncome
  exp <- runDb totalExpense
  return $ inc - exp

```
The function `totalExpense` above disregards three possibilities:

- The table could be empty, thus the `res` value could be an empty list.
- Because `res` could be an empty list, the function `head` is not safe at all.
- And because `sum_`'s return value is `Maybe a`, the usage of `fromJust` is a really bad.

Or if you love challange, you can modify that function so it returns a default value.
0.0, for example.

The same goes for many other functions we need. 
You can read it [here](https://gitlab.com/ibnuda/Telegram-Bot-Walkthrough/blob/feeb49d60a626047f4689072d29a0b3a06a4558f/src/ReadWrite.hs)
if you want to read the rest.

#### Types and Action

If you read the example from [telegram-bot-simple](https://github.com/fizruk/telegram-bot-simple),
you will know that the approach of this library is pretty similar to [miso](https://github.com/dmjio/miso)'s
or Elm's approach on state management.
And that means we have to define our `model` and `action` where every action on every state (or `model`)
maybe has a different effects on it.

So, let's define our `model` by open `Lib.hs` file on `src` dir.
```
-- Lib.hs
import Model
import ReadWrite

import Control.Applicative ((<|>))
import Data.Maybe
import Data.Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

```
Just standard imports.
Nothing particularly interesting here.

```
-- Lib.hs
data ChatState
  = InsertingIncome
  | InsertingIncomeSavedSource Text
  | InsertingExpense
  | InsertingExpenseSavedToWhom Text
  | SearchingIncome
  | SearchingExpense
  | CheckingBalance
  | Other Text
  | EmptyContent
  deriving (Show, Eq)

```
We defined a sumtype named `ChatState` to model the state of the application.
Because we can't really know the intention of the user when inserting, ah screw it.
Let me show you an example.
```
                            You: /income
Bot: Who gave you the money?
                            You: Mom
Bot: How much is it?
                            You: 420
Bot: Okay, saved!

```
Because the nature of the conversation, we can't really rely on the program's "inteligence".
It's a hard problem in NLP, isn't it?
And we don't even use that.
So, the easiest approach is we hold the state and saved input in the program itself as
part of the `state`.

Same goes for the other functions, we only need to take 1 argument tops, if any, before
we call the corresponding query functions. 
```
-- Lib.hs
data ChatModel =
  ChatModel ChatState
  deriving (Show, Eq)

emptyChatModel :: ChatModel
emptyChatModel = ChatModel EmptyContent

data Action
  = Empty
  | ActHelp
  | ActBalance
  | ActAddInc
  | ActAddExp
  | ActSearchIncome
  | ActSearchExpense
  | ActAddMessage Text
  deriving (Show, Read)

```
On the snippet above, we defined `ChatState` as, well, the the state of the application
plus its content; or `model` in Elm's lingo.
We also defined `Action` that limits what could be done on this program.
And let the `Action` derives from `Show` and `Read` because we need them to be "show-able"
and "read-able".
Perhaps you're guessing where do we get `Action` from.
It comes from the parsed or "read" value of message's text (`Update` data type).
For example, `ActHelp` is an action that will be used to tell the program to
show the user the "help messages" is a parsed from "/help" command which we will
define in a moment.

#### Inner Part of the Program

Next, we will create an "app representation" of our session by adding the following lines.
```
-- Lib.hs
incexpBotApp :: BotApp ChatModel Action
incexpBotApp = BotApp
  { botInitialModel = emptyChatModel
  , botAction = flip updateToAction
  , botHandler = updateHandler
  , botJobs = []
  }

```
The defined structure of the `BotApp` specifies what could be done when given a specific `Update`.
Oh, forgot to tell, the message we (and bot) send is called `Update` in this package.

At the moment, `updateToAction` and `updateHandler` haven't been defined yet.
So take a moment to define those functions.
```
-- Lib.hs
updateToAction :: ChatModel -> Update -> Maybe Action
updateToAction _ = parseUpdate $
  ActHelp <$ command (pack "help") <|>
  ActBalance <$ command (pack "balance") <|>
  ActAddInc <$ command (pack "income") <|>
  ActAddExp <$ command (pack "expense") <|>
  ActSearchIncomes <$  command (pack "incomes") <|>
  ActSearchExpenses <$ command (pack "expense") <|>
  ActMessage <$> plainText <|>
  callbackQueryDataRead

```
This `updateAction` function basically takes any kind of chat models and parses
any update it has given to by reading the message's text (`Update` has a lot of data in it).
If the message's text starts with a slash (`/`) then it will return the appropriate action
after "choosing" (symbolized by `<|>` operator) between the `pack`ed strings.
But when the message's text doesn't start with a slash, the function should regards
the message's text as wrapped `Text` in `ActMessage`.

Next, we will defined `updateHandler`.
In short, it's a function to modify a state when given an action.
But we will create a helper function first.
The function above will send a `String` to the other side by transforming the `String`
to a `Text` then transform it into `ReplyMessage` and then send it using `reply`
```
-- Lib.hs
replyString :: String -> BotM ()
replyString str = reply . toReplyMessage . pack $ str

```
And then create the function in the following snippets.
```
updateHandler :: Action -> Model -> Eff Action ChatModel
updateHandler action model =
  case action of
    Empty -> pure model

```
Whenever a state applied by an `Empty` action, it will return the state unmodified.
And basically a final destination so we will not receive an endless chat message from
this program.
```
    ActHelp ->
      emptyChatModel <# do
        replyString "Help messages"
        pure Empty

```
But when an `ActHelp` action being applied to any state, we will modify the state
to `emptyChatModel` and send "Help messages." to the other side and then returning
an `Empty` action.

Oh, and what that `<#` operator does is, basically, after you send a reply or
do whatever to the other scope of this system, you can return the left side of
this operator.
```
    ActBalance ->
      emptyChatModel <# do
        liftIO balance >>= replyString . show
        pure Empty

```
And when `ActBalance` being applied to any state, we still return an `emptyChatModel`
and then returning an `Empty` action after sending a message of the returned value
of `balance` (the function which returns your balance from database).
About `liftIO`, you can basically say that it is a magic function which makes
some wrapper into another wrapper.
```
    ActAddInc ->
      ChatModel InsertingIncome <# do
        replyString "Who gave you the money?"
        pure Empty

```
When `ActAddInc` being applied to any state, we will return a `ChatModel InsertingIncome`
while replying "Who gave you the money?" to the user which followed by returning
an `Empty` action.

We will continue the "conversation" in a few minutes.
Please bear with me for a few more cases.
```
    ActAddExp ->
      ChatModel InsertingExpense <# do
        replyString "Who did you give it to?"
        pure Empty

```
Basically still the same function as the previous case, actually.
```
    ActSearchIncome ->
      ChatModel SearchingIncome <# do
        replyString "Who are you looking for?"
        pure Empty
    ActSearchExpense ->
      ChatModel SearchingExpense <# do
        replyString "Who are you looking for?"
        pure Empty

```
Still the same functions.
But the state indicates the intention that the state itself is used for looking
expenses.
```
    ActMessage msg -> messageHandler msg model

```
This case specializes when the message which the user has sent doesn't have a leading
slash.
So we will create a specialized function to pattern-match the state with the message
content.
```
messageHandler :: Text -> ChatModel -> Eff Action ChatModel
messageHandler message model =
  case model of

```
To fill the gap between the previous case (`ActMessage msg`) and the return type
of the function (`Eff Action ChatModel`), we created a function that takes a `Text`
(from the `msg` in previous function) and `ChatModel` (which passed by the previous function)
and returns an `Eff Action ChatModel`.

And we pattern-matched the passed `model` with the following cases.
```
    ChatModel InsertingExpense ->
      ChatModel (InsertingExpenseSavedSource message) <# do
        replyString "How much is it?"
        pure Empty

```
When the model indicates we had an intention to insert an expense before, we will
modify the model to have `ChatModel (InsertingExpenseSavedSource message)` and then
return it after we reply to the user while returning an `Empty` action.
```
    ChatModel (InsertingExpenseSavedSource source) ->
      case (readMaybe $ unpack message :: Maybe Double) of
        Nothing -> reasking model
        Just amount ->
          ChatModel EmptyContent <# do
            _ <- liftIO $ insertExpense source amount
            replyString "Ok, saved!"
            pure Empty

```
On the other hand, when the program already has `ChatModel (InsertingExpenseSavedSource source)`
as its state, we will try to read the message as a `Double`.
If we failed to parse the message, we will call a function to ask the user to re-input
the message.
Otherwise, the parsed input will be used, together with `source`, as the input values
for `insertExpense`.
Which followed by sending "Ok, saved!" to the user and returning an `Empty` action.
All of that will be followed by returning an `EmptyContent` as the state of the chat.
```
    ChatModel InsertingIncome ->
      ChatModel (InsertingExpenseSavedSource message) <# do
        replyString "How much is it?"
        pure Empty
    ChatModel (InsertingIncomeSavedSource source) ->
      case (readMaybe $ unpack message :: Maybe Double) of
        Nothing -> reasking model
        Just amount ->
          ChatModel EmptyContent <# do
            _ <- liftIO $ insertIncome source amount
            replyString "Ok, saved!"
            pure Empty

```
The story that used for the above cases basically the same thing as the previous
explanation.
```
    ChatModel SearchingExpense ->
      ChatModel EmptyContent <# do
        expenses <- liftIO $ searchExpenseBySource message
        mapM_ (replyString . show) expenses
        pure Empty
    ChatModel SearchingIncome ->
      ChatModel EmptyContent <# do
        incomes <- liftIO $ searchIncomeBySource message
        mapM_ (replyString . show) incomes
        pure Empty

```
Both the functions above indicate that we had an intention to query the database
about what kind of income/expense from/to a certain entity.
What makes differences here is the `mapM_ (replystring . show) expenses` (or `incomes`).
`mapM_` is a function that iterates a list and then apply each and every element
of the previous list to a function while discards the returned value of the computation.
Pretty neat, ennit? Just like your usual `for` or `while`.
```
    otherwise ->
      model <# do
        reply . toReplyMessage $ helpMessage
        pure Empty

```
And the rest of pattern matching scheme, we will show the `helpMessage`.
Like this.

```
helpMessage :: Text
helpMessage =
  (intercalate $ pack "\n") $
  map
    pack
    [ "/help to show this message."
    , "/balance to show balance."
    , "/income to insert income."
    , "/expense to insert expense."
    , "/incomes to show incomes from a source."
    , "/expenses to show expenses to an entity."
    ]
```
#### Main Function and Telegram Preparation

For the main function, we will have a really simple function.
```
-- Lib.hs
someFunc :: IO ()
someFunc = do
  runStderrLoggingT doingMigration
  token <- Token . pack <$> getEnv "TOKEN_TELEGRAM"
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId incexpBotApp) env

```
Which basically creating tables, reading an env. variable, and then start the bot.

And now, we will create a new token for our bot.
I will not screenshot the conversation between my account and @BotFather.
```
                          me: /newbot
BotFather: What's the name?
                          me: SomethingBot
BotFather: Ok. Here's your token. $TOKEN_TELEGRAM

```
#### Deployment

All right, now you've got your token.
Now, you have to set an public IP facing web server plus tls connection thingy.
I've already set one before using [my own](2018-02-01-ssl-on-ip-address.html) guide.

Let's try to save a few records.
```
                                      Ibnu Aji: /income
Expendable_Bot: Who gave you the money?
                                      Ibnu Aji: Mom
Expendable_Bot: How much is it?
                                      Ibnu Aji: 420
Expendable_Bot: Ok, saved!
                                      Ibnu Aji: /expense
Expendable_Bot: Who did you give it to?
                                      Ibnu Aji: Little Sister
Expendable_Bot: How much is it?
                                      Ibnu Aji: 100
Expendable_Bot: Ok, saved!
                                      Ibnu Aji: /balance
Expendable_Bot: 320.0

```

Okay, we're finished here.
It was a nice experience for me to show one of my friend about how easy it is
to create a telegram bot.

#### Exercises, If You Want

That bot assumes only one account who uses it.
You can see that in the table as there's no `account` or anything that can
be recognized as one.
So, if you want to flex your finger a bit, you can.

1. Default value in `balance`.
2. User management.
