module Arkham.Asset.Assets.ConstanceDumaine (constanceDumaine) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype ConstanceDumaine = ConstanceDumaine AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

constanceDumaine :: AssetCard ConstanceDumaine
constanceDumaine = asset ConstanceDumaine Cards.constanceDumaine

instance HasAbilities ConstanceDumaine where
  getAbilities (ConstanceDumaine a) =
    [ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_
    , groupLimit PerGame
        $ restricted a 2 (not_ $ exists Story.sickeningReality_65)
        $ forced
        $ LastClueRemovedFromAsset #when (be a)
    ]

instance RunMessage ConstanceDumaine where
  runMessage msg a@(ConstanceDumaine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      modifiers <- getModifiers iid
      when (attrs.token #clue > 0 && CannotTakeControlOfClues `notElem` modifiers) do
        moveTokens (attrs.ability 1) attrs iid #clue 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Story.engramsOath
      pure a
    _ -> ConstanceDumaine <$> liftRunMessage msg attrs
