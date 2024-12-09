module Arkham.Event.Events.VirescentRot (virescentRot, VirescentRot (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype VirescentRot = VirescentRot EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

virescentRot :: EventCard VirescentRot
virescentRot = event VirescentRot Cards.virescentRot

instance HasModifiersFor VirescentRot where
  getModifiersFor (VirescentRot a) = case a.attachedTo of
    Just target -> modified_ a target [CannotMove]
    _ -> pure mempty

instance HasAbilities VirescentRot where
  getAbilities (VirescentRot a) =
    [ restrictedAbility a 1 ControlsThis $ forced $ EnemyLeavesPlay #when $ EnemyWithAttachedEvent (be a)
    ]

instance RunMessage VirescentRot where
  runMessage msg e@(VirescentRot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ PlaceInBonded iid (toCard attrs)
      pure e
    _ -> VirescentRot <$> liftRunMessage msg attrs
