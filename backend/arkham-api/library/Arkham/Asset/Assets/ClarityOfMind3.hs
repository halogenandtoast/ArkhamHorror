module Arkham.Asset.Assets.ClarityOfMind3 (clarityOfMind3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ClarityOfMind3 = ClarityOfMind3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clarityOfMind3 :: AssetCard ClarityOfMind3
clarityOfMind3 = asset ClarityOfMind3 Cards.clarityOfMind3

instance HasAbilities ClarityOfMind3 where
  getAbilities (ClarityOfMind3 a) =
    [ controlled a 1 (exists $ HealableInvestigator (a.ability 1) #horror $ colocatedWithMatch You)
        $ actionAbilityWithCost (assetUseCost a Charge 1)
    ]

instance RunMessage ClarityOfMind3 where
  runMessage msg a@(ClarityOfMind3 attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      doStep 2 msg
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror (colocatedWith iid)
      chooseOrRunOneM iid do
        targets investigators \x -> do
          healHorror x (attrs.ability 1) 1
          doStep (n - 1) msg'
      pure a
    _ -> ClarityOfMind3 <$> liftRunMessage msg attrs
