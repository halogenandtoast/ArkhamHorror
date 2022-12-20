module Arkham.Asset.Cards.AncientStoneMindsInHarmony4
  ( ancientStoneMindsInHarmony4
  , AncientStoneMindsInHarmony4(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Target
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
          (ControlsThis <> AnyCriterion
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
        <$> runMessage msg (attrs { assetUses = Uses Secret n })
    UseCardAbility iid (isSource attrs -> True) 1 _ (getAmount -> amount) -> do
      investigators <-
        selectListMap InvestigatorTarget
        $ colocatedWith iid
        <> InvestigatorWithAnyHorror
      assets <-
        selectListMap AssetTarget
        $ AssetAt (locationWithInvestigator iid)
        <> AssetWithHorror
      push
        $ chooseOrRunOne iid
        $ [ TargetLabel target [HealHorror target (toSource attrs) amount]
          | target <- investigators <> assets
          ]
      pure a
    _ -> AncientStoneMindsInHarmony4 <$> runMessage msg attrs
