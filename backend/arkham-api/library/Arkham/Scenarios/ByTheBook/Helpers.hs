module Arkham.Scenarios.ByTheBook.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Enemy.Helpers (cancelEnemyDefeat)
import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Card (ConvertToCard (..), getVictoryPoints)
import Arkham.Helpers.Modifiers (getModifiers)
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Message.Lifted hiding (cancelEnemyDefeat)
import Arkham.Modifier
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Source
import Arkham.Tracing
import Arkham.Trait (Trait (Cultist))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window
import Arkham.Xp

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = standaloneI18n "byTheBook" a

rolandBanks :: InvestigatorMatcher
rolandBanks = InvestigatorWithTitle "Roland Banks"

victoryDisplayCultists :: ExtendedCardMatcher
victoryDisplayCultists = VictoryDisplayCardMatch $ basic $ CardWithTrait Cultist <> #enemy

wouldBeDefeatedEnemy :: [Window] -> EnemyId
wouldBeDefeatedEnemy = \case
  ((windowType -> Window.EnemyWouldBeDefeated eid) : _) -> eid
  (_ : rest) -> wouldBeDefeatedEnemy rest
  [] -> error "Unexpected window for wouldBeDefeatedEnemy"

healCultistInsteadOfDefeat :: (ReverseQueue m, Sourceable source) => source -> EnemyId -> m ()
healCultistInsteadOfDefeat source eid = do
  cancelEnemyDefeat eid
  damage <- field EnemyDamage eid
  health <- fieldJust EnemyHealth eid
  let excess = damage - health + 1
  when (excess > 0) $ healDamage eid source excess

spawnMrGrey :: ReverseQueue m => m ()
spawnMrGrey =
  selectOne rolandBanks >>= \case
    Just roland -> createEnemyCard_ Enemies.mrGrey roland
    Nothing -> do
      lead <- getLead
      createEnemyCard_ Enemies.mrGrey (locationWithInvestigator lead)

hasDeckCard :: (HasGame m, Tracing m) => InvestigatorId -> CardDef -> m Bool
hasDeckCard iid def =
  selectAny $ basic (cardIs def) <> OwnedBy (IncludeEliminated $ InvestigatorWithId iid)

swapCampaignCard :: ReverseQueue m => InvestigatorId -> CardDef -> CardDef -> m ()
swapCampaignCard iid old new = do
  push $ RemoveCampaignCardFromDeck iid old
  addCampaignCardToDeck iid DoNotShuffleIn new

gainByTheBookXp :: forall m. ReverseQueue m => Source -> m (Int, Int)
gainByTheBookXp source = do
  victoryDisplay <- scenarioField ScenarioVictoryDisplay
  enemyEntries <- toEntries $ filterCards (CardWithType EnemyType) victoryDisplay
  inPlayLocations <- select $ RevealedLocation <> LocationWithoutClues
  locationEntries <-
    (<>)
      <$> toEntries (filterCards (CardWithType LocationType) victoryDisplay)
      <*> toEntries inPlayLocations
  mRoland <- selectOne $ IncludeEliminated rolandBanks
  investigators <- select InvestigatorCanGainXp
  details <- for investigators \iid -> do
    let entries = if Just iid == mRoland then enemyEntries else locationEntries
    mods <- getModifiers iid
    let total = foldl' applyModifier (sum $ map snd entries) mods
    let xpEntries =
          [InvestigatorGainXp iid (XpDetail XpFromVictoryDisplay name n) | (name, n) <- entries]
    pure (iid, total, xpEntries <> mapMaybe (modifierEntry iid) mods)
  push $ ReportXp $ XpBreakdown $ concatMap (\(_, _, entries) -> entries) details
  for_ details \(iid, total, _) -> push $ GainXP iid source total
  pure (sum $ map snd enemyEntries, sum $ map snd locationEntries)
 where
  applyModifier n (XPModifier _ m) = max 0 (n + m)
  applyModifier n _ = n
  modifierEntry iid (XPModifier lbl m)
    | m > 0 = Just $ InvestigatorGainXp iid (XpDetail XpFromCardEffect lbl m)
    | m < 0 = Just $ InvestigatorLoseXp iid (XpDetail XpFromCardEffect lbl (abs m))
  modifierEntry _ _ = Nothing
  toEntries :: ConvertToCard c => [c] -> m [(Text, Int)]
  toEntries = mapMaybeM \c -> do
    card <- RevealedCard <$> convertToCard c
    (toTitle card,) <$$> getVictoryPoints c
