module Arkham.Location.Cards.MistPylon_177 (mistPylon_177) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Matcher
import Arkham.Scenarios.TheHeartOfMadness.Pylon

newtype MistPylon_177 = MistPylon_177 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable, Targetable)

mistPylon_177 :: LocationCard MistPylon_177
mistPylon_177 =
  locationWith
    MistPylon_177
    Cards.mistPylon_177
    3
    (PerPlayer 2)
    (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor MistPylon_177 where
  getModifiersFor (MistPylon_177 a) = do
    collapsed <- (a.token #damage >=) <$> getPlayerCountValue (PerPlayer 2)
    modifySelfWhen a a.revealed $ CanBeAttackedAsIfEnemy : [ScenarioModifier "collapsed" | collapsed]
    modifySelectWhen
      a
      (notNull $ locationSeals a)
      (InvestigatorAt $ LocationWithTitle "Mist-Pylon")
      [CancelOneDamageOrHorror]

instance HasAbilities MistPylon_177 where
  getAbilities = pylonAbilities SealB

instance RunMessage MistPylon_177 where
  runMessage = pylonRunner SealB
