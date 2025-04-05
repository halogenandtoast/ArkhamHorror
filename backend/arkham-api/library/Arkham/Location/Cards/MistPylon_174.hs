module Arkham.Location.Cards.MistPylon_174 (mistPylon_174) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen, modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Matcher
import Arkham.Scenarios.TheHeartOfMadness.Pylon

newtype MistPylon_174 = MistPylon_174 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable, Targetable)

mistPylon_174 :: LocationCard MistPylon_174
mistPylon_174 =
  locationWith
    MistPylon_174
    Cards.mistPylon_174
    1
    (PerPlayer 4)
    (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor MistPylon_174 where
  getModifiersFor (MistPylon_174 a) = do
    collapsed <- (a.token #damage >=) <$> getPlayerCountValue (PerPlayer 4)
    modifySelfWhen a a.revealed $ CanBeAttackedAsIfEnemy : [ScenarioModifier "collapsed" | collapsed]
    modifySelectWhen a (notNull $ locationSeals a) (LocationWithTitle "Mist-Pylon") [DamageTaken 1]

instance HasAbilities MistPylon_174 where
  getAbilities = pylonAbilities SealD

instance RunMessage MistPylon_174 where
  runMessage = pylonRunner SealD
