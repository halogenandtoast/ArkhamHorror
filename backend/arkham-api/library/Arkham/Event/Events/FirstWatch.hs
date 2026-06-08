module Arkham.Event.Events.FirstWatch (firstWatch) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query (getInvestigators, getPlayerCount)
import Arkham.Strategy

newtype FirstWatchMetadata = FirstWatchMetadata {firstWatchPairings :: [(InvestigatorId, EncounterCard)]}
  deriving newtype (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype FirstWatch = FirstWatch (EventAttrs `With` FirstWatchMetadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstWatch :: EventCard FirstWatch
firstWatch = event (FirstWatch . (`with` FirstWatchMetadata [])) Cards.firstWatch

instance RunMessage FirstWatch where
  runMessage msg e@(FirstWatch (attrs `With` meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      don't AllDrawEncounterCard
      playerCount <- getPlayerCount
      lookAt iid attrs EncounterDeckTarget [(FromTopOfDeck playerCount, PutBack)] #any (defer attrs IsNotDraw)
      pure e
    UseCardAbilityChoice iid (isSource attrs -> True) 1 (EncounterCardMetadata card) [] _ -> do
      investigators <- getInvestigators
      let assignedInvestigators = map fst (firstWatchPairings meta)
      let remainingInvestigators = nub $ iid : (investigators \\ assignedInvestigators)
      chooseTargetM iid remainingInvestigators \iid' ->
        push $ UseCardAbilityChoice iid' (toSource attrs) 2 (EncounterCardMetadata card) [] NoPayment
      pure e
    UseCardAbilityChoice iid (isSource attrs -> True) 2 (EncounterCardMetadata card) _ _ -> do
      pure
        $ FirstWatch (attrs `with` meta {firstWatchPairings = (iid, card) : firstWatchPairings meta})
    SearchFound _ (isTarget attrs -> True) _ (onlyEncounterCards -> cards) | notNull cards -> do
      focusCards cards do
        chooseOneAtATimeM attrs.owner do
          targets cards \card -> do
            push $ UseCardAbilityChoice attrs.owner (toSource attrs) 1 (EncounterCardMetadata card) [] NoPayment
      doStep 2 msg
      pure e
    DoStep 2 (SearchFound _ (isTarget attrs -> True) _ _) -> do
      -- FAQ v2.5: cards dealt by First Watch are no longer in the encounter deck
      for_ (firstWatchPairings meta) (obtainCard . snd)
      for_ (firstWatchPairings meta) (uncurry drawCard)
      pure e
    _ -> FirstWatch . (`with` meta) <$> liftRunMessage msg attrs
