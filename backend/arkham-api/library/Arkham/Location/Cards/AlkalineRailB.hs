module Arkham.Location.Cards.AlkalineRailB (alkalineRailB) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Resident))

newtype AlkalineRailB = AlkalineRailB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alkalineRailB :: LocationCard AlkalineRailB
alkalineRailB = symbolLabel $ location AlkalineRailB Cards.alkalineRailB 3 (PerPlayer 3)

instance HasAbilities AlkalineRailB where
  getAbilities (AlkalineRailB a) =
    extendRevealed1 a
      $ restricted a 1 (oneOf [Here, exists $ assetAt a <> withTrait Resident])
      $ forced
      $ RoundEnds #when

instance RunMessage AlkalineRailB where
  runMessage msg l@(AlkalineRailB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt attrs) (assignDamageTo (attrs.ability 1) 1)
      selectEach (assetAt attrs <> withTrait Resident) \a -> dealAssetDirectDamage a (attrs.ability 1) 1
      pure l
    _ -> AlkalineRailB <$> liftRunMessage msg attrs
