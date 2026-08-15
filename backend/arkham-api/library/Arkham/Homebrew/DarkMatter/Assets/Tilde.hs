module Arkham.Homebrew.DarkMatter.Assets.Tilde (tilde) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (boogeymanAboveOrBelow)

newtype Tilde = Tilde AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tilde :: AssetCard Tilde
tilde = asset Tilde Cards.tilde

instance HasAbilities Tilde where
  getAbilities (Tilde a) =
    [skillTestAbility $ restricted a 1 (OnSameLocation <> boogeymanAboveOrBelow a.id) parleyAction_]

instance RunMessage Tilde where
  runMessage msg a@(Tilde attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      readStory iid attrs Stories.reintegrated_064
      pure a
    _ -> Tilde <$> liftRunMessage msg attrs
