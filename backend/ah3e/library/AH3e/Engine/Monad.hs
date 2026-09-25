module AH3e.Engine.Monad where

import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Ids
import Data.Map.Strict qualified as Map
import System.Random (StdGen, mkStdGen, random, uniformR)

type GameM = State Game

push :: Message -> GameM ()
push m = pushAll [m]

pushAll :: [Message] -> GameM ()
pushAll ms = #queue %= (ms <>)

pushEnd :: Message -> GameM ()
pushEnd m = #queue %= (<> [m])

logText :: Text -> GameM ()
logText t = #log %= (<> [t])

ask :: PlayerId -> Text -> [Choice] -> GameM ()
ask pid prompt choices = do
  #questions . at pid ?= Question prompt choices
  #questionsAsked += 1

askAll :: Map PlayerId Question -> GameM ()
askAll qs = do
  #questions %= (<> qs)
  #questionsAsked += length qs

withGen :: (StdGen -> (a, StdGen)) -> GameM a
withGen f = do
  s <- use #seed
  let (a, g) = f (mkStdGen s)
      (s', _) = random g
  #seed .= s'
  pure a

randomR :: (Int, Int) -> GameM Int
randomR range = withGen (uniformR range)

rollDie :: GameM Int
rollDie = randomR (1, 6)

shuffle :: [a] -> GameM [a]
shuffle xs = go (length xs) xs
 where
  go 0 _ = pure []
  go n ys = do
    i <- randomR (0, n - 1)
    let (before, rest) = splitAt i ys
    case rest of
      (y : after) -> (y :) <$> go (n - 1) (before <> after)
      [] -> pure []

pickRandom :: [a] -> GameM (Maybe a)
pickRandom [] = pure Nothing
pickRandom xs = Just . (xs !!) <$> randomR (0, length xs - 1)

newCard :: CardCode -> GameM CardId
newCard code = do
  n <- use #nextCardId
  #nextCardId .= n + 1
  let cid = CardId n
  #cards . at cid ?= code
  pure cid

cardCode :: CardId -> GameM CardCode
cardCode cid = uses #cards (fromJustNote ("unknown card " <> show cid) . Map.lookup cid)

getInvestigator :: HasCallStack => InvestigatorId -> GameM Investigator
getInvestigator iid = uses #investigators (fromJustNote ("unknown investigator " <> show iid) . Map.lookup iid)

investigatorL :: InvestigatorId -> Lens' Game Investigator
investigatorL iid = singular (#investigators . ix iid)

getMonster :: HasCallStack => CardId -> GameM Monster
getMonster mid = uses #monsters (fromJustNote ("unknown monster " <> show mid) . Map.lookup mid)

monsterL :: CardId -> Lens' Game Monster
monsterL mid = singular (#monsters . ix mid)

assetL :: CardId -> Lens' Game Asset
assetL cid = singular (#assets . ix cid)

playerOf :: InvestigatorId -> GameM PlayerId
playerOf iid = (.player) <$> getInvestigator iid

leaderPlayer :: GameM PlayerId
leaderPlayer = use #leader

playerOrder :: GameM [PlayerId]
playerOrder = do
  ps <- map (.id) <$> use #players
  l <- use #leader
  let (before, after) = break (== l) ps
  pure (after <> before)

investigatorOfPlayer :: PlayerId -> GameM (Maybe InvestigatorId)
investigatorOfPlayer pid = do
  ps <- use #players
  pure $ join $ listToMaybe [p.investigator | p <- ps, p.id == pid]

investigatorsInPlay :: GameM [Investigator]
investigatorsInPlay = do
  order <- playerOrder
  invs <- use #investigators
  fmap catMaybes $ for order \pid -> do
    miid <- investigatorOfPlayer pid
    pure do
      iid <- miid
      i <- Map.lookup iid invs
      _ <- i.space
      pure i

askLeader :: Text -> [Choice] -> GameM ()
askLeader prompt choices = do
  l <- leaderPlayer
  ask l prompt choices

-- a group decision with one option resolves without asking (rule 004 / 448.4)
chooseGroup :: Text -> [Choice] -> GameM ()
chooseGroup _ [] = pure ()
chooseGroup _ [c] = pushAll c.messages
chooseGroup prompt cs = askLeader prompt cs

chooseFor :: InvestigatorId -> Text -> [Choice] -> GameM ()
chooseFor _ _ [] = pure ()
chooseFor iid prompt cs = do
  pid <- playerOf iid
  ask pid prompt cs
