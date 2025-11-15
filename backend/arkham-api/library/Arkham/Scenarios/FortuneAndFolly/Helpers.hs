{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Scenarios.FortuneAndFolly.Helpers (module Arkham.Scenarios.FortuneAndFolly.Helpers, module X) where

import Arkham.Scenarios.FortuneAndFolly.PlayingCard as X
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
  reduceAlarmLevel,
  reduceAlarmLevelBy,
 )
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
  }
  deriving stock (Eq, Show)

data Mulligan = CanMulligan Int | NoMulligan
  deriving stock (Eq, Show)

decrementMulligan :: Mulligan -> Mulligan
decrementMulligan (CanMulligan n) | n > 1 = CanMulligan (n -1)
decrementMulligan _ = NoMulligan

foldMap (deriveJSON defaultOptions) [''Mulligan, ''CheckGameIcons]

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
  pure $ and $ zipWith (\a b -> b == a + 1) sortedRanks (drop 1 sortedRanks)

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
