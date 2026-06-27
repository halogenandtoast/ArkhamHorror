module Arkham.Location.Cards.SecretPassageRelicsOfThePast (secretPassageRelicsOfThePast) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SecretPassageRelicsOfThePast = SecretPassageRelicsOfThePast LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretPassageRelicsOfThePast :: LocationCard SecretPassageRelicsOfThePast
secretPassageRelicsOfThePast =
  symbolLabel
    $ location SecretPassageRelicsOfThePast Cards.secretPassageRelicsOfThePast 5 (PerPlayer 1)
    & setConnectsTo (setFromList [LeftOf, RightOf])

instance HasAbilities SecretPassageRelicsOfThePast where
  getAbilities (SecretPassageRelicsOfThePast a) =
    extend
      a
      [ restricted a 1 (exists $ SetAsideCardMatch $ cardIs Cards.innerChamber)
          $ forced
          $ PutLocationIntoPlay #after Anyone (be a)
      , restricted a 2 (HasSupply Compass) $ forced $ Enters #after You (be a)
      ]

instance RunMessage SecretPassageRelicsOfThePast where
  runMessage msg l@(SecretPassageRelicsOfThePast attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeSetAsideLocation_ Cards.innerChamber
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discoverAt NotInvestigate iid (attrs.ability 2) 1 attrs
      pure l
    _ -> SecretPassageRelicsOfThePast <$> liftRunMessage msg attrs
