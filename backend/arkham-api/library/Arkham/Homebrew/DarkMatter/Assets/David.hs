module Arkham.Homebrew.DarkMatter.Assets.David (david) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (boogeymanAboveOrBelow)

newtype David = David AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

david :: AssetCard David
david = asset David Cards.david

instance HasAbilities David where
  getAbilities (David a) =
    [skillTestAbility $ restricted a 1 (OnSameLocation <> boogeymanAboveOrBelow a.id) parleyAction_]

instance RunMessage David where
  runMessage msg a@(David attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #combat (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      readStory iid attrs Stories.reintegrated_063
      pure a
    _ -> David <$> liftRunMessage msg attrs
