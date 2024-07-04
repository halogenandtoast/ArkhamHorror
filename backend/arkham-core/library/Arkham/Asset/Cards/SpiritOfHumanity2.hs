module Arkham.Asset.Cards.SpiritOfHumanity2 (spiritOfHumanity2, SpiritOfHumanity2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag (getRemainingBlessTokens)
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Matcher

newtype SpiritOfHumanity2 = SpiritOfHumanity2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritOfHumanity2 :: AssetCard SpiritOfHumanity2
spiritOfHumanity2 = asset SpiritOfHumanity2 Cards.spiritOfHumanity2

instance HasAbilities SpiritOfHumanity2 where
  getAbilities (SpiritOfHumanity2 x) =
    let
      criteria = exists $ oneOf $ map healable [#horror, #damage]
      healable hType = HealableInvestigator (toAbilitySource x 1) hType You
     in
      [ controlledAbility x 1 HasRemainingBlessTokens
          $ FastAbility
          $ exhaust x
          <> HorrorCost (toAbilitySource x 1) YouTarget 1
          <> DamageCost (toAbilitySource x 1) YouTarget 1
      , controlledAbility x 1 (HasRemainingCurseTokens <> criteria)
          $ FastAbility
          $ exhaust x
          <> AddCurseTokenCost 2
      ]

instance RunMessage SpiritOfHumanity2 where
  runMessage msg a@(SpiritOfHumanity2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- min 2 <$> getRemainingBlessTokens
      replicateM_ n $ addChaosToken #bless
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      canHealDamage <- canHaveDamageHealed source iid
      canHealHorror <- canHaveHorrorHealed source iid
      pushAll
        $ [ HealDamage (InvestigatorTarget iid) source 1
          | canHealDamage
          ]
        <> [ HealHorror (InvestigatorTarget iid) source 1
           | canHealHorror
           ]
      pure a
    _ -> SpiritOfHumanity2 <$> liftRunMessage msg attrs
