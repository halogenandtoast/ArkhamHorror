module Arkham.Event.Events.OnTheHunt (onTheHunt, OnTheHunt (..)) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), getAdditionalSearchTargets)
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
      prompt_ iid "No Cards were found."
      afterSearch $ drawEncounterCard iid attrs
      pure e
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      prompt_ iid "No Cards were found."
      afterSearch $ drawEncounterCard iid attrs
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      additionalTargets <- getAdditionalSearchTargets iid
      let enemyCards = filter (`cardMatch` EnemyType) $ onlyEncounterCards cards
      if (notNull enemyCards)
        then chooseNM iid (min (length enemyCards) (1 + additionalTargets)) do
          targets enemyCards \card -> do
            searchModifier attrs card (ForceSpawn (SpawnEngagedWith $ InvestigatorWithId iid))
            push $ InvestigatorDrewEncounterCard iid card
        else do
          prompt_ iid "No enemies were found."
          afterSearch $ drawEncounterCard iid attrs
      pure e
    _ -> OnTheHunt <$> liftRunMessage msg attrs
