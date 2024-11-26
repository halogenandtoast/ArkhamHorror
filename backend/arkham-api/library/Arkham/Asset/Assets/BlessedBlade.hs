module Arkham.Asset.Assets.BlessedBlade (blessedBlade, BlessedBlade (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Cost
import Arkham.Helpers.SkillTest
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Modifier

newtype BlessedBlade = BlessedBlade AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blessedBlade :: AssetCard BlessedBlade
blessedBlade = asset BlessedBlade Cards.blessedBlade

instance HasAbilities BlessedBlade where
  getAbilities (BlessedBlade attrs) = [controlled attrs 1 (thisIs attrs AssetReady) fightAction_]

instance RunMessage BlessedBlade where
  runMessage msg a@(BlessedBlade attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifier sid source iid $ SkillModifier #combat 1
      onRevealChaosTokenEffect sid (oneOf [#bless, #eldersign]) attrs attrs do
        skillTestModifier sid source iid $ DamageDealt 1
      chooseFightEnemy sid iid source
      pure a
    BeforeRevealChaosTokens -> do
      void $ runMaybeT $ do
        st <- MaybeT getSkillTest
        guard $ isAbilitySource attrs 1 st.source
        liftGuardM $ getCanAffordCost st.investigator st.source [] [] (exhaust attrs)
        liftGuardM $ (> 0) <$> getRemainingBlessTokens
        lift
          $ chooseOne
            st.investigator
            [ Label
                "Exhaust Blessed Blade to add 1 {bless} token"
                [Exhaust (toTarget attrs), AddChaosToken #bless]
            , Label "Do not exhaust" []
            ]
      pure a
    _ -> BlessedBlade <$> liftRunMessage msg attrs
