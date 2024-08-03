module Arkham.Event.Cards.OnTheLamAdvanced where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Game.Helpers (getCanMoveToMatchingLocations)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Movement

newtype OnTheLamAdvanced = OnTheLamAdvanced EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLamAdvanced :: EventCard OnTheLamAdvanced
onTheLamAdvanced = event OnTheLamAdvanced Cards.onTheLamAdvanced

instance RunMessage OnTheLamAdvanced where
  runMessage msg e@(OnTheLamAdvanced attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      roundModifier eid iid (CannotBeAttackedBy NonEliteEnemy)
      createCardEffect Cards.onTheLamAdvanced Nothing attrs iid
      pure e
    _ -> OnTheLamAdvanced <$> liftRunMessage msg attrs

newtype OnTheLamAdvancedEffect = OnTheLamAdvancedEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLamAdvancedEffect :: EffectArgs -> OnTheLamAdvancedEffect
onTheLamAdvancedEffect = cardEffect OnTheLamAdvancedEffect Cards.onTheLamAdvanced

instance HasAbilities OnTheLamAdvancedEffect where
  getAbilities (OnTheLamAdvancedEffect a) = case a.target of
    InvestigatorTarget iid ->
      [ displayAsAction
          $ restrictedAbility a 1 (youExist $ InvestigatorWithId iid)
          $ ConstantReaction
            "Disengage from each engaged enemy and move up to 2 locations away (On the Lam)"
            (RoundEnds #when)
            Free
      ]
    _ -> []

instance RunMessage OnTheLamAdvancedEffect where
  runMessage msg e@(OnTheLamAdvancedEffect attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <-
        getCanMoveToMatchingLocations iid attrs
          $ LocationWithAccessiblePath attrs.source 2 (InvestigatorWithId iid) Anywhere
      enemies <- select $ enemyEngagedWith iid
      traverse_ (push . DisengageEnemy iid) enemies
      chooseOrRunOneM iid do
        for_ locations \location -> do
          targeting location do
            push $ MoveTo $ (move attrs iid location) {moveMeans = Towards}
      disableReturn e
    EndRound -> disableReturn e
    _ -> OnTheLamAdvancedEffect <$> liftRunMessage msg attrs
