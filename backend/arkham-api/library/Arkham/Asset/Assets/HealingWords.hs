module Arkham.Asset.Assets.HealingWords (healingWords) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Token

newtype HealingWords = HealingWords AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

healingWords :: AssetCard HealingWords
healingWords = asset HealingWords Cards.healingWords

instance HasAbilities HealingWords where
  getAbilities (HealingWords a) =
    [ controlled a 1 (exists $ HealableInvestigator (a.ability 1) #damage $ colocatedWithMatch You)
        $ actionAbilityWithCost
        $ assetUseCost a Charge 1
    ]

instance RunMessage HealingWords where
  runMessage msg a@(HealingWords attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      damageInvestigators <- select $ HealableInvestigator (attrs.ability 1) #damage $ colocatedWith iid
      chooseOrRunOneM iid $ targets damageInvestigators $ healDamageOn (attrs.ability 1) 1
      pure a
    _ -> HealingWords <$> liftRunMessage msg attrs
