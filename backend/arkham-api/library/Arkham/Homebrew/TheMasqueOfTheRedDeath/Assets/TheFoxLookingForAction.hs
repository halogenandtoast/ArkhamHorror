module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheFoxLookingForAction (
  theFoxLookingForAction,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameEnv (getHistoryField, getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty), maybeModified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.History (HistoryField (HistoryPlayedCards))
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestParleySuccess)

newtype TheFoxLookingForAction = TheFoxLookingForAction AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFoxLookingForAction :: AssetCard TheFoxLookingForAction
theFoxLookingForAction = asset TheFoxLookingForAction Cards.theFoxLookingForAction

instance HasAbilities TheFoxLookingForAction where
  -- "[action]: Parley. Test [agility] (4)."
  getAbilities (TheFoxLookingForAction a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance HasModifiersFor TheFoxLookingForAction where
  -- "This test gets -1 difficulty for each card you have played this round (to a
  -- minimum of 1)." The printed difficulty is 4, so the reduction stops at 3.
  getModifiersFor (TheFoxLookingForAction a) =
    getSkillTest >>= traverse_ \st -> maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      played <- lift $ getHistoryField #round st.investigator HistoryPlayedCards
      let n = min 3 (length played)
      guard $ n > 0
      pure [Difficulty (-n)]

instance RunMessage TheFoxLookingForAction where
  runMessage msg a@(TheFoxLookingForAction attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      guestParleySuccess attrs 1 iid
      pure a
    -- Act 1b flips every guest; the Victim face is a different card def.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theFoxLookingForRelease
      pure a
    _ -> TheFoxLookingForAction <$> liftRunMessage msg attrs
