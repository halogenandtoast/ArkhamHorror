module Arkham.Asset.Assets.ClarityOfMind (clarityOfMind) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ClarityOfMind = ClarityOfMind AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clarityOfMind :: AssetCard ClarityOfMind
clarityOfMind = asset ClarityOfMind Cards.clarityOfMind

instance HasAbilities ClarityOfMind where
  getAbilities (ClarityOfMind a) =
    [ controlled a 1 (exists $ HealableInvestigator (a.ability 1) #horror $ colocatedWithMatch You)
        $ actionAbilityWithCost (assetUseCost a Charge 1)
    ]

instance RunMessage ClarityOfMind where
  runMessage msg a@(ClarityOfMind attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror (colocatedWith iid)
      chooseOrRunOneM iid do
        targets investigators \x -> healHorror x (attrs.ability 1) 1
      pure a
    _ -> ClarityOfMind <$> liftRunMessage msg attrs
