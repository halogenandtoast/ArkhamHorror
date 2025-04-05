module Arkham.Location.Cards.MistPylon_176 (mistPylon_176) where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen, modifySelfWhen)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (choose)
import Arkham.Matcher
import Arkham.Scenarios.TheHeartOfMadness.Pylon

newtype MistPylon_176 = MistPylon_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, Sourceable, Targetable)

mistPylon_176 :: LocationCard MistPylon_176
mistPylon_176 =
  locationWith
    MistPylon_176
    Cards.mistPylon_176
    4
    (PerPlayer 2)
    (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasModifiersFor MistPylon_176 where
  getModifiersFor (MistPylon_176 a) = do
    collapsed <- (a.token #damage >=) <$> getPlayerCountValue (PerPlayer 2)
    modifySelfWhen a a.revealed $ CanBeAttackedAsIfEnemy : [ScenarioModifier "collapsed" | collapsed]
    modifySelectWhen
      a
      (notNull $ locationSeals a)
      (InvestigatorAt $ LocationWithTitle "Mist-Pylon")
      [SkillModifier skind 1 | skind <- [minBound ..]]

instance HasAbilities MistPylon_176 where
  getAbilities = pylonAbilities SealC

instance RunMessage MistPylon_176 where
  runMessage = pylonRunner SealC
