module Day10 where

import qualified Data.Map as Map
import Data.Maybe
import Data.Either
import Data.List
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Lazy
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Lexer (integer)

type BotFactory = Map.Map Id Bot
type Value = Integer

type BotMonad a = MaybeT (Writer [BotResultLog]) a
newtype Id = Id Integer deriving(Eq, Ord, Show)
data GivesTo = BotOutput | Output deriving(Show)
data NextInstruction = NextInstruction GivesTo Id deriving(Show)
data BotState = Free | HoldOne Value | HoldTwo Value Value deriving(Show)
data Bot = Bot {state :: BotState, low ::  NextInstruction, high :: NextInstruction} deriving(Show)
data BotResultLog = BotLog Id Value Value | LogOutput Id Value deriving(Show)

parseInstruction :: Parser NextInstruction
parseInstruction = do
    givesTo <- (BotOutput <$ string "bot ") <|> (Output <$ string "output ")
    botId <- parseId
    return (NextInstruction givesTo botId)
    
parseId = Id <$> integer

parseTransaction = do 
    string "bot "
    botId <- parseId
    string " gives low to "
    lowId <- parseInstruction
    string " and high to "
    highId <- parseInstruction
    return (botId, (Bot Free lowId highId))
    
parseInitialValue = do
    string "value "
    val <- integer
    string " goes to bot "
    botId <- parseId
    return (botId, val)
    
botParser :: Parser (Either (Id, Value) (Id, Bot))
botParser = (Left <$> parseInitialValue) <|> (Right <$>  parseTransaction)

solve = do 
    (inits, trans) <- (partitionEithers . rights . fmap (parse botParser "") . lines) <$> readFile "./day10.txt"
    return $ snd $ runWriter $ runMaybeT $ computations (Map.fromList trans) inits



part1 = find (\(BotLog k a' b') -> a == a' && b == b') <$> solve
    where
        (a, b) = (17, 61)
        
part2 = filter (\x -> 
    case x of
         (LogOutput i x) -> True
         _ -> False) <$> solve
                           
        
computations = foldM (flip computeStep)

computeStep :: (Id, Value) -> BotFactory  -> BotMonad BotFactory
computeStep (k, v) bots = do
    bot <- MaybeT . return $ Map.lookup k bots
    (bot', ins) <- findNextInstructions k v bot
    ins' <- logOutputs ins
    computations (Map.insert k bot' bots) ins'
    
logOutputs :: [(NextInstruction, Value)] -> BotMonad [(Id, Value)]
logOutputs xs = do
    xs' <- mapM logging xs
    return $ mapMaybe keepImportant xs'
    where
    logging :: (NextInstruction, Value) -> BotMonad (NextInstruction, Value)
    logging x@((NextInstruction Output i), v) = tell [LogOutput i v] >> return x
    logging x = return x
    keepImportant ((NextInstruction BotOutput i), v) = Just (i,v)
    keepImportant ((NextInstruction Output i), v) = Nothing
findNextInstructions :: Id -> Value -> Bot -> BotMonad (Bot, [(NextInstruction, Value)])
findNextInstructions k v bot = do 
    bot' <- fmap (\st -> bot {state = st}) $ updateState v (state bot)
    ins <- getNextInstruction k bot'
    return (bot', ins)
    
    
getNextInstruction :: Id -> Bot -> BotMonad [(NextInstruction, Value)]
getNextInstruction k (Bot (HoldTwo a b) l h) = do 
    let (a', b') = if a < b then (a, b) else (b, a)
    tell [BotLog k a' b']
    return [(l, a'), (h, b')]
getNextInstruction _ _ = return []

updateState :: MonadPlus m => Value -> BotState -> m BotState
updateState v st = 
    case st of
         Free -> return $ HoldOne v
         HoldOne v' -> return $ HoldTwo v v'
         HoldTwo _ _ -> mzero



