module Arkham.Location.Cards.MistPylon_175 (mistPylon_175) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen, modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Matcher
import Arkham.Scenarios.TheHeartOfMadness.Pylon

newtype MistPylon_175 = MistPylon_175 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable, Targetable)

mistPylon_175 :: LocationCard MistPylon_175
mistPylon_175 =
  locationWith
    MistPylon_175
    Cards.mistPylon_175
    5
    (PerPlayer 1)
    (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor MistPylon_175 where
  getModifiersFor (MistPylon_175 a) = do
    collapsed <- (a.token #damage >=) <$> getPlayerCountValue (PerPlayer 1)
    modifySelfWhen a a.revealed $ CanBeAttackedAsIfEnemy : [ScenarioModifier "collapsed" | collapsed]
    modifySelectWhen a (notNull $ locationSeals a) (LocationWithTitle "Mist-Pylon") [ShroudModifier (-2)]

instance HasAbilities MistPylon_175 where
  getAbilities = pylonAbilities SealE

instance RunMessage MistPylon_175 where
  runMessage = pylonRunner SealE
