module Arkham.Asset.Assets.IshimaruHaruko (ishimaruHaruko) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype IshimaruHaruko = IshimaruHaruko AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ishimaruHaruko :: AssetCard IshimaruHaruko
ishimaruHaruko = asset IshimaruHaruko Cards.ishimaruHaruko

instance HasAbilities IshimaruHaruko where
  getAbilities (IshimaruHaruko a) =
    [ skillTestAbility
        $ restricted a 1 (OnSameLocation <> youExist (HandWith (LengthIs $ atLeast 6))) parleyAction_
    , groupLimit PerGame
        $ restricted a 2 (not_ $ exists Story.sickeningReality_67)
        $ forced
        $ LastClueRemovedFromAsset #when (be a)
    ]

instance RunMessage IshimaruHaruko where
  runMessage msg a@(IshimaruHaruko attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #willpower (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      modifiers <- getModifiers iid
      when (attrs.token #clue > 0 && CannotTakeControlOfClues `notElem` modifiers) do
        moveTokens (attrs.ability 1) attrs iid #clue 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Story.thePattern
      pure a
    _ -> IshimaruHaruko <$> liftRunMessage msg attrs
