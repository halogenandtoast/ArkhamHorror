module Arkham.Event.Events.PutrescentRot (putrescentRot, PutrescentRot (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher

newtype PutrescentRot = PutrescentRot EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

putrescentRot :: EventCard PutrescentRot
putrescentRot = event PutrescentRot Cards.putrescentRot

instance HasModifiersFor PutrescentRot where
  getModifiersFor (PutrescentRot a) = case a.attachedTo of
    Just target -> modified_ a target [EnemyFight (-1), EnemyEvade (-1)]
    _ -> pure mempty

instance HasAbilities PutrescentRot where
  getAbilities (PutrescentRot a) =
    [ restrictedAbility a 1 ControlsThis $ forced $ EnemyLeavesPlay #when $ EnemyWithAttachedEvent (be a)
    ]

instance RunMessage PutrescentRot where
  runMessage msg e@(PutrescentRot attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ PlaceInBonded iid (toCard attrs)
      pure e
    _ -> PutrescentRot <$> liftRunMessage msg attrs
