module Arkham.Event.Cards.AbyssalRot (abyssalRot, AbyssalRot (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher

newtype AbyssalRot = AbyssalRot EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalRot :: EventCard AbyssalRot
abyssalRot = event AbyssalRot Cards.abyssalRot

instance HasModifiersFor AbyssalRot where
  getModifiersFor target (AbyssalRot a) | target `elem` a.attachedTo = do
    modified a [CannotAttack]
  getModifiersFor _ _ = pure []

instance HasAbilities AbyssalRot where
  getAbilities (AbyssalRot a) =
    [ restrictedAbility a 1 ControlsThis $ forced $ EnemyLeavesPlay #when $ EnemyWithAttachedEvent (be a)
    ]

instance RunMessage AbyssalRot where
  runMessage msg e@(AbyssalRot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ PlaceInBonded iid (toCard attrs)
      pure e
    _ -> AbyssalRot <$> liftRunMessage msg attrs
