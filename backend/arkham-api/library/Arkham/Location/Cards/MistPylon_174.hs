module Arkham.Location.Cards.MistPylon_174 (mistPylon_174) where

import Arkham.Ability
import Arkham.Constants
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Scenarios.TheHeartOfMadness.Pylon

newtype MistPylon_174 = MistPylon_174 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable, Targetable)

mistPylon_174 :: LocationCard MistPylon_174
mistPylon_174 = location MistPylon_174 Cards.mistPylon_174 1 (PerPlayer 4)

instance HasModifiersFor MistPylon_174 where
  getModifiersFor (MistPylon_174 a) = do
    collapsed <- (a.token #damage >=) <$> getPlayerCountValue (PerPlayer 4)
    modifySelfWhen a a.revealed $ CanBeAttackedAsIfEnemy : [ScenarioModifier "collapsed" | collapsed]

instance HasAbilities MistPylon_174 where
  getAbilities (MistPylon_174 a) =
    extendRevealed1 a
      $ basicAbility
      $ restricted a AbilityAttack Here
      $ ActionAbility [#fight] (ActionCost 1)

instance RunMessage MistPylon_174 where
  runMessage msg l@(MistPylon_174 _attrs) = runQueueT $ case msg of
    _ -> lift $ pylonRunner msg l
