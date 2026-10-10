module Arkham.Homebrew.AgesUnwound.Treacheries.RealityUndone (realityUndone) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype RealityUndone = RealityUndone TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityUndone :: TreacheryCard RealityUndone
realityUndone = treacheryWith RealityUndone Cards.realityUndone (setMeta @[Int] [])

{- | "Each investigator tests [willpower] (3). Each investigator who fails must
choose a different option: Take 3 damage. / Take 3 horror. / Discard an asset you
control. / Draw the top two cards of the encounter deck."

The options are shared across the table, so the ones already taken live in the
treachery's meta and are withheld from the next investigator's prompt.
-}
instance RunMessage RealityUndone where
  runMessage msg t@(RealityUndone attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      let taken = toResult @[Int] attrs.meta
      let remaining = filter (`notElem` taken) [0 .. 3]
      hasAssets <- selectAny $ DiscardableAsset <> AssetNonStory <> assetControlledBy iid
      let pick n = push $ HandleAbilityOption iid (toSource attrs) n
      when (notNull remaining) $ chooseOneM iid $ withI18n do
        when (0 `elem` remaining) $ countVar 3 $ labeled "takeDamage" do
          pick 0
          assignDamage iid attrs 3
        when (1 `elem` remaining) $ countVar 3 $ labeled "takeHorror" do
          pick 1
          assignHorror iid attrs 3
        when (2 `elem` remaining) $ countVar 1 $ labeledValidate hasAssets "discardAssets" do
          pick 2
          chooseAndDiscardAssetMatching iid attrs AssetNonStory
        when (3 `elem` remaining) $ countVar 2 $ labeled "drawTopCardOfEncounterDeck" do
          pick 3
          drawEncounterCards iid attrs 2
      pure t
    HandleAbilityOption _ (isSource attrs -> True) n ->
      pure $ RealityUndone $ setMeta (n : toResult @[Int] attrs.meta) attrs
    _ -> RealityUndone <$> liftRunMessage msg attrs
