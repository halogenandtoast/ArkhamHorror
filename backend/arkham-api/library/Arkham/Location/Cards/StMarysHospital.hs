module Arkham.Location.Cards.StMarysHospital (stMarysHospital) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype StMarysHospital = StMarysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospital :: LocationCard StMarysHospital
stMarysHospital = location StMarysHospital Cards.stMarysHospital 2 (PerPlayer 1)

instance HasAbilities StMarysHospital where
  getAbilities (StMarysHospital x) =
    extendRevealed1 x
      $ playerLimit PerGame
      $ restricted x 1 (Here <> exists (HealableInvestigator (toSource x) #damage You)) doubleActionAbility

instance RunMessage StMarysHospital where
  runMessage msg l@(StMarysHospital attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      healDamage iid (attrs.ability 1) 3
      pure l
    _ -> StMarysHospital <$> liftRunMessage msg attrs
