module Arkham.Asset.Assets.Kerosene1 (kerosene1, Kerosene1 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Damage
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher

newtype Kerosene1 = Kerosene1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kerosene1 :: AssetCard Kerosene1
kerosene1 = assetWith Kerosene1 Cards.kerosene1 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities Kerosene1 where
  getAbilities (Kerosene1 a) =
    [ controlled
        a
        1
        ( exists
            (locationWithAsset a <> LocationWithDefeatedEnemyThisRound)
            <> oneOf
              [ exists $ HealableInvestigator (a.ability 1) #horror $ colocatedWithMatch You
              , exists
                  $ HealableAsset (toSource a) #horror
                  $ AssetAt YourLocation
                  <> #ally
                  <> AssetControlledBy (affectsOthers Anyone)
              ]
        )
        $ actionAbilityWithCost (exhaust a <> assetUseCost a Supply 1)
    ]

instance RunMessage Kerosene1 where
  runMessage msg a@(Kerosene1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      -- When a source owned by the investigator can heal at full (e.g. Soul
      -- Sanctification), horror can be "healed" even when there is none to heal,
      -- so the amount must not be capped by the horror actually present.
      mods <- getModifiers iid
      canHealAtFull <-
        anyM (sourceMatches source) [sm | CanHealAtFull sm dt <- mods, dt == HorrorType]
      maxHorror <-
        if canHealAtFull
          then pure 2
          else do
            totalInvestigatorHorror <-
              getSum
                <$> selectAgg Sum InvestigatorHorror (HealableInvestigator source #horror $ colocatedWith iid)
            totalAssetHorror <-
              getSum
                <$> selectAgg
                  Sum
                  AssetHorror
                  ( HealableAsset source #horror
                      $ at_ (locationWithInvestigator iid)
                      <> #ally
                      <> AssetControlledBy (affectsOthersKnown iid Anyone)
                  )
            pure $ min 2 (totalInvestigatorHorror + totalAssetHorror)

      chooseAmounts
        iid
        "Choose amount of horror to heal"
        (MaxAmountTarget maxHorror)
        [("Horror", (0, maxHorror))]
        (toTarget attrs)
      pure a
    ResolveAmounts iid (getChoiceAmount "Horror" -> n) (isTarget attrs -> True) -> do
      pushAll
        $ replicate n
        $ UseCardAbilityChoice iid (toSource attrs) 1 NoAbilityMetadata [] NoPayment
      pure a
    UseCardAbilityChoice iid (isSource attrs -> True) 1 _ _ _ -> do
      investigators <- selectTargets $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid

      assets <-
        selectTargets
          $ HealableAsset (attrs.ability 1) HorrorType
          $ AssetAt (locationWithInvestigator iid)
          <> #ally
          <> AssetControlledBy (affectsOthersKnown iid Anyone)

      chooseOne iid
        $ [ TargetLabel target [HealHorror target (toSource attrs) 1]
          | target <- assets <> investigators
          ]
      pure a
    _ -> Kerosene1 <$> liftRunMessage msg attrs
