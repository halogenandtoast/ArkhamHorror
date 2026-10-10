module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.ThePeacockCenterOfAttention (
  thePeacockCenterOfAttention,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty), maybeModified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestParleySuccess)
import Arkham.Matcher
import Arkham.Projection

newtype ThePeacockCenterOfAttention = ThePeacockCenterOfAttention AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePeacockCenterOfAttention :: AssetCard ThePeacockCenterOfAttention
thePeacockCenterOfAttention = asset ThePeacockCenterOfAttention Cards.thePeacockCenterOfAttention

instance HasAbilities ThePeacockCenterOfAttention where
  -- "[action]: Parley. Test [combat] (4)."
  getAbilities (ThePeacockCenterOfAttention a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance HasModifiersFor ThePeacockCenterOfAttention where
  -- "This test gets -1 difficulty for each asset you control with a printed cost
  -- of 4 or more (to a minimum of 1)." The printed difficulty is 4, so the
  -- reduction stops at 3. 'AssetCost' is the printed cost.
  getModifiersFor (ThePeacockCenterOfAttention a) =
    getSkillTest >>= traverse_ \st -> maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      costs <- lift $ traverse (field AssetCost) =<< select (assetControlledBy st.investigator)
      let n = min 3 (count (>= 4) costs)
      guard $ n > 0
      pure [Difficulty (-n)]

instance RunMessage ThePeacockCenterOfAttention where
  runMessage msg a@(ThePeacockCenterOfAttention attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #combat (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      guestParleySuccess attrs 1 iid
      pure a
    -- Act 1b flips every guest; the Victim face is a different card def.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.thePeacockCenterOfAffliction
      pure a
    _ -> ThePeacockCenterOfAttention <$> liftRunMessage msg attrs
