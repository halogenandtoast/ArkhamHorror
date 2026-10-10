module Arkham.Homebrew.TheMasqueOfTheRedDeath.Assets.TheRavenNotEasilyImpressed (
  theRavenNotEasilyImpressed,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.GameEnv (getSkillTest)
import Arkham.Helpers.Modifiers (ModifierType (Difficulty), maybeModified_)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (guestParleySuccess)

newtype TheRavenNotEasilyImpressed = TheRavenNotEasilyImpressed AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRavenNotEasilyImpressed :: AssetCard TheRavenNotEasilyImpressed
theRavenNotEasilyImpressed = asset TheRavenNotEasilyImpressed Cards.theRavenNotEasilyImpressed

{- | "each card that you have committed to skill tests this round"

Nothing in the engine records commits past the end of the test they were made
to, so the Raven keeps its own tally -- one entry per card committed, cleared at
the end of the round. The investigator is part of the entry because the clause
is "that *you* have committed" and any seat may parley.
-}
committedThisRound :: AssetAttrs -> InvestigatorId -> Int
committedThisRound attrs iid = count (== iid) $ getAssetMetaDefault [] attrs

instance HasAbilities TheRavenNotEasilyImpressed where
  -- "[action]: Parley. Test [agility] (4)."
  getAbilities (TheRavenNotEasilyImpressed a) =
    [skillTestAbility $ restricted a 1 OnSameLocation parleyAction_]

instance HasModifiersFor TheRavenNotEasilyImpressed where
  -- "This test gets -1 difficulty for each card that you have committed to skill
  -- tests this round (to a minimum of 1)." The printed difficulty is 4, so the
  -- reduction stops at 3. Read continuously, so a card committed to this very
  -- test counts, which is what "this round" means.
  getModifiersFor (TheRavenNotEasilyImpressed a) =
    getSkillTest >>= traverse_ \st -> maybeModified_ a (SkillTestTarget st.id) do
      guard $ isAbilitySource a 1 st.source
      let n = min 3 (committedThisRound a st.investigator)
      guard $ n > 0
      pure [Difficulty (-n)]

instance RunMessage TheRavenNotEasilyImpressed where
  runMessage msg a@(TheRavenNotEasilyImpressed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #agility (Fixed 4)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      guestParleySuccess attrs 1 iid
      pure a
    -- Act 1b flips every guest; the Victim face is a different card def.
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theRavenNotEasilyDisposed
      pure a
    Do (CommitCard iid _) -> do
      attrs' <- liftRunMessage msg attrs
      pure . TheRavenNotEasilyImpressed $ overMeta (<>) [iid] attrs'
    EndRound -> do
      attrs' <- liftRunMessage msg attrs
      pure . TheRavenNotEasilyImpressed $ attrs' & setMeta ([] :: [InvestigatorId])
    _ -> TheRavenNotEasilyImpressed <$> liftRunMessage msg attrs
