module Arkham.Asset.Assets.AzizaSteerHasABoneToPick (azizaSteerHasABoneToPick) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AzizaSteerHasABoneToPick = AzizaSteerHasABoneToPick AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

azizaSteerHasABoneToPick :: AssetCard AzizaSteerHasABoneToPick
azizaSteerHasABoneToPick = ally AzizaSteerHasABoneToPick Cards.azizaSteerHasABoneToPick (4, 4)

instance HasAbilities AzizaSteerHasABoneToPick where
  getAbilities (AzizaSteerHasABoneToPick a) =
    [ controlled a 1 (exists $ oneOf [HealableAsset (a.ability 1) dt (be a) | dt <- [#damage, #horror]])
        $ freeReaction
        $ AssetDealtDamageOrHorror #after (SourceIsEnemyAttack AnyEnemy) (be a)
    ]

instance RunMessage AzizaSteerHasABoneToPick where
  runMessage msg a@(AzizaSteerHasABoneToPick attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      exhaustThis attrs
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "healDamage" $ healDamage attrs (attrs.ability 1) 1
        countVar 1 $ labeled "healHorror" $ healHorror attrs (attrs.ability 1) 1
      pure a
    _ -> AzizaSteerHasABoneToPick <$> liftRunMessage msg attrs
