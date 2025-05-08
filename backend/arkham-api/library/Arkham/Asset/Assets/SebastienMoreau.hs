module Arkham.Asset.Assets.SebastienMoreau (sebastienMoreau) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype SebastienMoreau = SebastienMoreau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sebastienMoreau :: AssetCard SebastienMoreau
sebastienMoreau = asset SebastienMoreau Cards.sebastienMoreau

instance HasAbilities SebastienMoreau where
  getAbilities (SebastienMoreau a) =
    [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
    , groupLimit PerGame
        $ restricted a 2 (not_ $ exists Story.sickeningReality_68)
        $ forced
        $ LastClueRemovedFromAsset #when (be a)
    ]

instance RunMessage SebastienMoreau where
  runMessage msg a@(SebastienMoreau attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      modifiers <- getModifiers iid
      when (attrs.token #clue > 0 && CannotTakeControlOfClues `notElem` modifiers) do
        moveTokens (attrs.ability 1) attrs iid #clue 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Story.theFirstShow
      pure a
    _ -> SebastienMoreau <$> liftRunMessage msg attrs
