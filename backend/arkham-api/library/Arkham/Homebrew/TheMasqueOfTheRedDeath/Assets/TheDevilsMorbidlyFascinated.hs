module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheDevilsMorbidlyFascinated (
  theDevilsMorbidlyFascinated,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty), maybeModified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestParleySuccess)
import Arkham.Investigator.Types (Field (InvestigatorHorror))
import Arkham.Projection

newtype TheDevilsMorbidlyFascinated = TheDevilsMorbidlyFascinated AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theDevilsMorbidlyFascinated :: AssetCard TheDevilsMorbidlyFascinated
theDevilsMorbidlyFascinated = asset TheDevilsMorbidlyFascinated Cards.theDevilsMorbidlyFascinated

instance HasAbilities TheDevilsMorbidlyFascinated where
  -- "[action]: Parley. Test [willpower] (4)."
  getAbilities (TheDevilsMorbidlyFascinated a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance HasModifiersFor TheDevilsMorbidlyFascinated where
  -- "This test gets -1 difficulty for every 2 horror on your investigator (to a
  -- minimum of 1)." The printed difficulty is 4, so the reduction stops at 3.
  getModifiersFor (TheDevilsMorbidlyFascinated a) =
    getSkillTest >>= traverse_ \st -> maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      horror <- lift $ field InvestigatorHorror st.investigator
      let n = min 3 (horror `div` 2)
      guard $ n > 0
      pure [Difficulty (-n)]

instance RunMessage TheDevilsMorbidlyFascinated where
  runMessage msg a@(TheDevilsMorbidlyFascinated attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      guestParleySuccess attrs 1 iid
      pure a
    -- Act 1b flips every guest; the Victim face is a different card def.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theDevilsMorbidlyFated
      pure a
    _ -> TheDevilsMorbidlyFascinated <$> liftRunMessage msg attrs
