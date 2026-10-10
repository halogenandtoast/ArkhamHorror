module Arkham.Homebrew.AgesUnwound.Stories.FavorsForFavors (favorsForFavors) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask, takeControlOfSetAsideRewardAsset)
import Arkham.Message.Lifted.Log (record)
import Arkham.Story.Import.Lifted

newtype FavorsForFavors = FavorsForFavors StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Contacting the Lodge/ (@:ages-unwound:148@).
favorsForFavors :: StoryCard FavorsForFavors
favorsForFavors = story FavorsForFavors Cards.favorsForFavors

{- | "In your Campaign Log, record that /you have advanced the schemes of the
Silver Twilight Lodge./ Put the set-aside Ionian Pendant into play under your
control. For the remainder of this scenario, it does not take up an accessory
slot. Complete The Devil You Know. Remove this card from the game."

"This card" is Contacting the Lodge, the other side of this one.
-}
instance RunMessage FavorsForFavors where
  runMessage msg s@(FavorsForFavors attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      record YouHaveAdvancedTheSchemesOfTheSilverTwilightLodge
      takeControlOfSetAsideRewardAsset attrs iid Assets.ionianPendant #accessory
      completeTask Treacheries.theDevilYouKnow
      for_ (storyOtherSide attrs) removeFromGame
      pure s
    _ -> FavorsForFavors <$> liftRunMessage msg attrs
