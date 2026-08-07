{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Scenarios.FortuneAndFolly.Helpers (module Arkham.Scenarios.FortuneAndFolly.Helpers, module X) where

import Arkham.Ability.Types
import Arkham.Capability
import Arkham.Card
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Message.Lifted
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers as X (
  getAlarmLevel,
  getMaxAlarmLevel,
 )
import Arkham.Scenarios.FortuneAndFolly.PlayingCard as X
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window
import Data.Aeson.TH
import Data.Function (on)
import Data.Monoid (First (..))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "fortuneAndFolly" a

data CheckGameIcons = CheckGameIcons
  { target :: Target
  , investigator :: InvestigatorId
  , n :: Int
  , cards :: [EncounterCard]
  , mulligan :: Mulligan
  , setAside :: [EncounterCard]
  {- ^ Cards removed from the hand by a mulligan. They stay set aside (out of the
  encounter discard) until the check resolves, so a mid-check reshuffle can't
  pull them back into the encounter deck.
  -}
  }
  deriving stock (Eq, Show)

data Mulligan = CanMulligan Int | NoMulligan
  deriving stock (Eq, Show)

decrementMulligan :: Mulligan -> Mulligan
decrementMulligan (CanMulligan n) | n > 1 = CanMulligan (n - 1)
decrementMulligan _ = NoMulligan

foldMap (deriveJSON defaultOptions) [''Mulligan]
deriveToJSON defaultOptions ''CheckGameIcons

-- Hand written so that @setAside@ stays optional: 'CheckGameIcons' is serialized
-- into the persisted message queue, and games parked mid-check were saved before
-- the field existed.
instance FromJSON CheckGameIcons where
  parseJSON = withObject "CheckGameIcons" \o -> do
    target <- o .: "target"
    investigator <- o .: "investigator"
    n <- o .: "n"
    cards <- o .: "cards"
    mulligan <- o .: "mulligan"
    setAside <- o .:? "setAside" .!= []
    pure $ CheckGameIcons {target, investigator, n, cards, mulligan, setAside}

{- | All cards this check is holding: the ones checked so far plus anything the
investigator has set aside with a mulligan.
-}
checkedCards :: CheckGameIcons -> [EncounterCard]
checkedCards params = params.cards <> params.setAside

checkGameIcons
  :: (Targetable target, ReverseQueue m) => target -> InvestigatorId -> Mulligan -> Int -> m ()
checkGameIcons (toTarget -> target) iid mulligan n =
  scenarioSpecific "checkGameIcons"
    $ toJSON
    $ CheckGameIcons
      { cards = []
      , investigator = iid
      , mulligan
      , n
      , setAside = []
      , target
      }

sameRank :: (HasGame m, HasCardDef a) => Int -> [a] -> m Bool
sameRank n cards = do
  playingCards <- mapMaybeM toPlayingCard cards
  let rankGroups = groupBy (\a b -> a.rank == b.rank) (sortBy (compare `on` (.rank)) playingCards)
  pure $ any (\grp -> length grp >= n) rankGroups

allSameSuit :: (HasCardDef a, HasGame m) => [a] -> m Bool
allSameSuit cards =
  mapMaybeM toPlayingCard cards <&> \case
    [] -> False
    (x : xs) -> all (\pc -> pc.suit == x.suit) xs

allSameRank :: (HasCardDef a, HasGame m) => [a] -> m Bool
allSameRank cards =
  mapMaybeM toPlayingCard cards <&> \case
    [] -> False
    (x : xs) -> all (\pc -> pc.rank == x.rank) xs

sequential :: (HasGame m, HasCardDef a) => [a] -> m Bool
sequential cards = do
  playingCards <- mapMaybeM toPlayingCard cards
  let sortedRanks = sort $ map rankValue playingCards
  -- fewer than two ranks is vacuously "in a row", which would be a free win
  pure $ length sortedRanks > 1 && and (zipWith (\a b -> b == a + 1) sortedRanks (drop 1 sortedRanks))

toPlayingCard :: (HasCardDef a, HasGame m) => a -> m (Maybe PlayingCard)
toPlayingCard a = do
  mods <- getModifiers (CardCodeTarget $ toCardCode cardDef)
  let mpc :: Maybe PlayingCard =
        getFirst
          $ fold [First (maybeResult @PlayingCard pc) | ScenarioModifierValue "setPlayingCard" pc <- mods]
  pure $ mpc <|> toPlayingCardPure a
 where
  cardDef = toCardDef a

winGame
  :: (HasGameLogger m, ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
winGame iid (toSource -> source) n = do
  sendUI "confetti"
  whenM (can.gain.resources iid) do
    abilityModifier (AbilityRef source 1) (AbilitySource source 1) iid (ScenarioModifier "gotResources")
    gainResources iid source n

raiseAlarmLevel :: (Sourceable source, ReverseQueue m) => source -> [InvestigatorId] -> m ()
raiseAlarmLevel source iids = do
  valids <- iids & filterM \iid -> (< 10) <$> getAlarmLevel iid
  for_ valids \iid -> placeTokens source iid AlarmLevel 1
  unless (null valids) do
    checkWindows $ mkAfter <$> map IncreasedAlarmLevel valids
{-# INLINE raiseAlarmLevel #-}

raiseAlarmLevelOf :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
raiseAlarmLevelOf source iid = raiseAlarmLevel source [iid]
{-# INLINE raiseAlarmLevelOf #-}

{- | "An investigator's alarm level cannot be reduced below 1 or raised above 10."
That floor is Fortune and Folly's alone, which is why this shadows the Dark Side
of the Moon helper of the same name instead of re-exporting it.
-}
reduceAlarmLevelBy :: (Sourceable source, ReverseQueue m) => Int -> source -> InvestigatorId -> m ()
reduceAlarmLevelBy n (toSource -> source) iid = do
  current <- getAlarmLevel iid
  let n' = min n (max 0 (current - 1))
  when (n' > 0) $ removeTokens source iid AlarmLevel n'
{-# INLINE reduceAlarmLevelBy #-}

reduceAlarmLevel :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
reduceAlarmLevel source = reduceAlarmLevelBy 1 source
{-# INLINE reduceAlarmLevel #-}
