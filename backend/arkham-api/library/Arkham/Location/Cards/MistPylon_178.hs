module Arkham.Location.Cards.MistPylon_178 (mistPylon_178) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen, modifySelectWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Matcher
import Arkham.Scenarios.TheHeartOfMadness.Pylon

newtype MistPylon_178 = MistPylon_178 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable, Targetable)

mistPylon_178 :: LocationCard MistPylon_178
mistPylon_178 =
  locationWith
    MistPylon_178
    Cards.mistPylon_178
    2
    (PerPlayer 3)
    (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor MistPylon_178 where
  getModifiersFor (MistPylon_178 a) = do
    collapsed <- (a.token #damage >=) <$> getPlayerCountValue (PerPlayer 3)
    modifySelfWhen a a.revealed $ CanBeAttackedAsIfEnemy : [ScenarioModifier "collapsed" | collapsed]
    modifySelectWhen a (notNull $ locationSeals a) (EnemyAt $ LocationWithTitle "Mist-Pylon") [EnemyFight (-2), EnemyEvade (-2)]

instance HasAbilities MistPylon_178 where
  getAbilities = pylonAbilities SealA

instance RunMessage MistPylon_178 where
  runMessage = pylonRunner SealA
