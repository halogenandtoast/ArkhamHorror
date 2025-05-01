module Arkham.Event.Events.OnTheHunt (onTheHunt) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), getAdditionalSearchTargets)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Spawn
import Arkham.Strategy
import Arkham.Zone

newtype OnTheHunt = OnTheHunt EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheHunt :: EventCard OnTheHunt
onTheHunt = event OnTheHunt Cards.onTheHunt

instance RunMessage OnTheHunt where
  runMessage msg e@(OnTheHunt attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      don't $ DoDrawCards iid
      search iid attrs EncounterDeckTarget [(FromTopOfDeck 9, ShuffleBackIn)] #any (defer attrs IsDraw)
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      withI18n $ prompt_ iid "noCardsFound"
      afterSearch $ drawEncounterCard iid attrs
      pure e
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      withI18n $ prompt_ iid "noCardsFound"
      afterSearch $ drawEncounterCard iid attrs
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      let enemyCards = filter (`cardMatch` EnemyType) $ onlyEncounterCards cards
      if notNull enemyCards
        then chooseNM iid (min (length enemyCards) (1 + additionalTargets)) do
          targets enemyCards \card -> do
            searchModifiers
              attrs
              card
              [ForceSpawn (SpawnEngagedWith $ InvestigatorWithId iid), IgnoreRevelation]
            push $ InvestigatorDrewEncounterCard iid card
        else do
          withI18n $ prompt_ iid "noEnemiesFound"
          afterSearch $ drawEncounterCard iid attrs
      pure e
    _ -> OnTheHunt <$> liftRunMessage msg attrs
