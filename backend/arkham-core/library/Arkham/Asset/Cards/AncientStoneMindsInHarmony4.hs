module Arkham.Asset.Cards.AncientStoneMindsInHarmony4 (
  ancientStoneMindsInHarmony4,
  AncientStoneMindsInHarmony4 (..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype AncientStoneMindsInHarmony4 = AncientStoneMindsInHarmony4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStoneMindsInHarmony4 :: AssetCard AncientStoneMindsInHarmony4
ancientStoneMindsInHarmony4 = asset AncientStoneMindsInHarmony4 Cards.ancientStoneMindsInHarmony4

instance HasAbilities AncientStoneMindsInHarmony4 where
  getAbilities (AncientStoneMindsInHarmony4 a) =
    [ controlledAbility
        a
        1
        ( oneOf
            [ exists (InvestigatorAt YourLocation <> InvestigatorWithAnyHorror)
            , exists (AssetAt YourLocation <> AssetWithHorror)
            ]
        )
        $ ReactionAbility (DrawsCards #when You AnyValue) (DynamicUseCost (be a) Secret DrawnCardsValue)
    ]

instance RunMessage AncientStoneMindsInHarmony4 where
  runMessage msg a@(AncientStoneMindsInHarmony4 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> amount) -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid
      assets <- select $ HealableAsset (toSource attrs) #horror $ AssetAt (locationWithInvestigator iid)

      chooseOrRunOne
        iid
        [ TargetLabel target [HealHorror target (toSource attrs) amount]
        | target <- map toTarget assets <> map toTarget investigators
        ]
      pure a
    _ -> AncientStoneMindsInHarmony4 <$> liftRunMessage msg attrs
