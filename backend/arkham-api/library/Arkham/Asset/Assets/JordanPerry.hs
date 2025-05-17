module Arkham.Asset.Assets.JordanPerry (jordanPerry) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype JordanPerry = JordanPerry AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jordanPerry :: AssetCard JordanPerry
jordanPerry = asset JordanPerry Cards.jordanPerry

instance HasAbilities JordanPerry where
  getAbilities (JordanPerry a) =
    [ skillTestAbility
        $ restricted a 1 (OnSameLocation <> youExist (InvestigatorWithResources $ atLeast 10)) parleyAction_
    , groupLimit PerGame
        $ restricted a 2 (not_ $ exists Story.sickeningReality_66)
        $ forced
        $ LastClueRemovedFromAsset #when (be a)
    ]

instance RunMessage JordanPerry where
  runMessage msg a@(JordanPerry attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      modifiers <- getModifiers iid
      when (attrs.token #clue > 0 && CannotTakeControlOfClues `notElem` modifiers) do
        moveTokens (attrs.ability 1) attrs iid #clue 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Story.lagneauPerdu
      pure a
    _ -> JordanPerry <$> liftRunMessage msg attrs
