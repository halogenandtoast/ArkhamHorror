module Arkham.Location.Cards.AlkalineRailA (alkalineRailA) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Resident))

newtype AlkalineRailA = AlkalineRailA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alkalineRailA :: LocationCard AlkalineRailA
alkalineRailA = location AlkalineRailA Cards.alkalineRailA 3 (PerPlayer 3)

instance HasAbilities AlkalineRailA where
  getAbilities (AlkalineRailA a) =
    extendRevealed1 a
      $ restricted a 1 (oneOf [Here, exists $ assetAt a <> withTrait Resident])
      $ forced
      $ RoundEnds #when

instance RunMessage AlkalineRailA where
  runMessage msg l@(AlkalineRailA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) (assignDamageTo (attrs.ability 1) 1)
      selectEach (assetAt attrs <> withTrait Resident) \a -> dealAssetDirectDamage a (attrs.ability 1) 1
      pure l
    _ -> AlkalineRailA <$> liftRunMessage msg attrs
