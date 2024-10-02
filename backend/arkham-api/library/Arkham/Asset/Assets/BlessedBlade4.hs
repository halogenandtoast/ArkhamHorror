module Arkham.Asset.Assets.BlessedBlade4 (blessedBlade4, BlessedBlade4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Cost
import Arkham.Helpers.SkillTest
import Arkham.Modifier

newtype BlessedBlade4 = BlessedBlade4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessedBlade4 :: AssetCard BlessedBlade4
blessedBlade4 = asset BlessedBlade4 Cards.blessedBlade4

instance HasAbilities BlessedBlade4 where
  getAbilities (BlessedBlade4 attrs) = [restrictedAbility attrs 1 ControlsThis fightAction_]

instance RunMessage BlessedBlade4 where
  runMessage msg a@(BlessedBlade4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
      skillTestModifier sid (attrs.ability 1) sid ReturnBlessedToChaosBag
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    BeforeRevealChaosTokens -> do
      void $ runMaybeT $ do
        st <- MaybeT getSkillTest
        guard $ isAbilitySource attrs 1 st.source
        liftGuardM $ getCanAffordCost st.investigator st.source [] [] (exhaust attrs)
        blessTokens <- lift $ min 2 <$> getRemainingBlessTokens
        guard $ blessTokens > 0
        lift
          $ chooseOne
            st.investigator
            [ Label ("Exhaust Blessed Blade (4) to add " <> pluralize blessTokens "{bless} token")
                $ Exhaust (toTarget attrs)
                : replicate blessTokens (AddChaosToken #bless)
            , Label "Do not exhaust" []
            ]
      pure a
    _ -> BlessedBlade4 <$> liftRunMessage msg attrs
