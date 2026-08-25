module Arkham.Homebrew.CircusExMortis.Stories.DeepInTheDark (deepInTheDark) where

import Arkham.Ability
import Arkham.GameValue (GameValue (..))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokensAt)
import Arkham.Matcher
import Arkham.Story.Import.Lifted

newtype DeepInTheDark = DeepInTheDark StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Starts Kidnapped Citizen (b) side up; flips to the story side and back.
deepInTheDark :: StoryCard DeepInTheDark
deepInTheDark = storyWith DeepInTheDark Cards.deepInTheDark (flippedL .~ True)

instance HasAbilities DeepInTheDark where
  getAbilities (DeepInTheDark attrs)
    | attrs.flipped =
        [restricted attrs 1 OnSameLocation $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)]
    | otherwise = [restricted attrs 2 OnSameLocation actionAbility]

instance RunMessage DeepInTheDark where
  runMessage msg (DeepInTheDark attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure . DeepInTheDark $ attrs & flippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      mLoc <- getLocationOf attrs.placement
      for_ mLoc \loc -> do
        n <- perPlayer 1
        sealed <- length <$> getSealedMoonTokensAt loc
        let required = max 0 (n - sealed)
        withCost iid (GroupClueCost (Static required) (LocationWithId loc)) do
          addToVictory iid attrs
      pure . DeepInTheDark $ attrs & flippedL .~ True
    _ -> DeepInTheDark <$> liftRunMessage msg attrs
