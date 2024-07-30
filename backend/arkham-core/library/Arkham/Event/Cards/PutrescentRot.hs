module Arkham.Event.Cards.PutrescentRot (putrescentRot, PutrescentRot (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modified)
import Arkham.Matcher

newtype PutrescentRot = PutrescentRot EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

putrescentRot :: EventCard PutrescentRot
putrescentRot = event PutrescentRot Cards.putrescentRot

instance HasModifiersFor PutrescentRot where
  getModifiersFor target (PutrescentRot a) | target `elem` a.attachedTo = do
    modified a [EnemyFight (-1), EnemyEvade (-1)]
  getModifiersFor _ _ = pure []

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
