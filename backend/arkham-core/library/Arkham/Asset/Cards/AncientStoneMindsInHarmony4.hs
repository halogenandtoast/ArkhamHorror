module Arkham.Asset.Cards.AncientStoneMindsInHarmony4 (
  ancientStoneMindsInHarmony4,
  AncientStoneMindsInHarmony4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Timing qualified as Timing

newtype AncientStoneMindsInHarmony4 = AncientStoneMindsInHarmony4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStoneMindsInHarmony4 :: AssetCard AncientStoneMindsInHarmony4
ancientStoneMindsInHarmony4 =
  asset AncientStoneMindsInHarmony4 Cards.ancientStoneMindsInHarmony4

instance HasAbilities AncientStoneMindsInHarmony4 where
  getAbilities (AncientStoneMindsInHarmony4 a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> AnyCriterion
              [ InvestigatorExists
                  (InvestigatorAt YourLocation <> InvestigatorWithAnyHorror)
              , AssetExists (AssetAt YourLocation <> AssetWithHorror)
              ]
        )
        $ ReactionAbility
          (DrawsCards Timing.When You AnyValue)
          (DynamicUseCost (AssetWithId $ toId a) Secret DrawnCardsValue)
    ]

getAmount :: Payment -> Int
getAmount (UsesPayment n) = n
getAmount (Payments ps) = sum $ map getAmount ps
getAmount _ = 0

instance RunMessage AncientStoneMindsInHarmony4 where
  runMessage msg a@(AncientStoneMindsInHarmony4 attrs) = case msg of
    InvestigatorPlayedAsset _ aid | aid == toId attrs -> do
      n <- getRecordCount YouHaveIdentifiedTheStone
      AncientStoneMindsInHarmony4
        <$> runMessage
          msg
          (attrs {assetPrintedUses = Uses Secret (Static n), assetUses = singletonMap Secret n})
    UseCardAbility iid (isSource attrs -> True) 1 _ (getAmount -> amount) -> do
      investigatorsWithHealMessage <-
        getInvestigatorsWithHealHorror attrs amount $ colocatedWith iid

      assets <-
        selectMap AssetTarget
          $ HealableAsset (toSource attrs) HorrorType
          $ AssetAt (locationWithInvestigator iid)

      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ TargetLabel target [HealHorror target (toSource attrs) amount]
          | target <- assets
          ]
        <> [ targetLabel target [healHorror]
           | (target, healHorror) <- investigatorsWithHealMessage
           ]
      pure a
    _ -> AncientStoneMindsInHarmony4 <$> runMessage msg attrs
