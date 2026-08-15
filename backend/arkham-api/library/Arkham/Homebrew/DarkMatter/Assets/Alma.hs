module Arkham.Homebrew.DarkMatter.Assets.Alma (alma) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (boogeymanAboveOrBelow)

newtype Alma = Alma AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alma :: AssetCard Alma
alma = asset Alma Cards.alma

instance HasAbilities Alma where
  getAbilities (Alma a) =
    [skillTestAbility $ restricted a 1 (OnSameLocation <> boogeymanAboveOrBelow a.id) parleyAction_]

instance RunMessage Alma where
  runMessage msg a@(Alma attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      readStory iid attrs Stories.reintegrated_062
      pure a
    _ -> Alma <$> liftRunMessage msg attrs
