module Arkham.Asset.Cards.Kerosene1
  ( kerosene1
  , Kerosene1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher

newtype Kerosene1 = Kerosene1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kerosene1 :: AssetCard Kerosene1
kerosene1 = assetWith Kerosene1 Cards.kerosene1 (discardWhenNoUsesL .~ True)

instance HasAbilities Kerosene1 where
  getAbilities (Kerosene1 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis
          <> LocationExists
               (LocationOfThis <> LocationWithDefeatedEnemyThisRound)
          <> AnyCriterion
               [ InvestigatorExists
                 (HealableInvestigator HorrorType
                 $ InvestigatorAt YourLocation
                 )
               , AssetExists (HealableAsset HorrorType $ AssetAt YourLocation)
               ]
          )
        $ ActionAbility Nothing
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Supply 1
    ]

instance RunMessage Kerosene1 where
  runMessage msg a@(Kerosene1 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      totalInvestigatorHorror <- getSum <$> selectAgg
        Sum
        InvestigatorHorror
        (colocatedWith iid <> InvestigatorWithAnyHorror)
      totalAssetHorror <- getSum <$> selectAgg
        Sum
        AssetHorror
        (AssetAt (locationWithInvestigator iid) <> AssetWithHorror)

      let maxHorror = min 2 (totalInvestigatorHorror + totalAssetHorror)

      push $ chooseAmounts
        iid
        "Choose amount of horror to heal"
        (MaxAmountTarget maxHorror)
        [("Horror", (0, maxHorror))]
        (toTarget attrs)
      pure a
    ResolveAmounts iid (getChoiceAmount "Horror" -> n) (isTarget attrs -> True)
      -> do
        pushAll $ replicate n $ UseCardAbilityChoice
          iid
          (toSource attrs)
          1
          NoAbilityMetadata
          []
          NoPayment
        pure a

    UseCardAbilityChoice iid (isSource attrs -> True) 1 _ _ _ -> do
      investigators <-
        selectTargets $ HealableInvestigator HorrorType $ colocatedWith iid
      assets <- selectTargets $ HealableAsset HorrorType $ AssetAt
        (locationWithInvestigator iid)
      push $ chooseOne
        iid
        [ TargetLabel target [HealHorror target (toSource attrs) 1]
        | target <- investigators <> assets
        ]
      pure a
    _ -> Kerosene1 <$> runMessage msg attrs
