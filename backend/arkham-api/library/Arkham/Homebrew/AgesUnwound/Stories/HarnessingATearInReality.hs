module Arkham.Homebrew.AgesUnwound.Stories.HarnessingATearInReality (harnessingATearInReality) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTask, takeControlOfSetAsideRewardAsset)
import Arkham.Story.Import.Lifted

newtype HarnessingATearInReality = HarnessingATearInReality StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of /Scheme of the Myriad/ (@:ages-unwound:152@).
harnessingATearInReality :: StoryCard HarnessingATearInReality
harnessingATearInReality = story HarnessingATearInReality Cards.harnessingATearInReality

{- | "Put the set-aside Forestall Fate asset into play under your control. For
the remainder of this scenario, it does not take up an arcane slot. Complete Up
To Something. Remove this card from the game."
-}
instance RunMessage HarnessingATearInReality where
  runMessage msg s@(HarnessingATearInReality attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      takeControlOfSetAsideRewardAsset attrs iid Assets.forestallFate #arcane
      completeTask Treacheries.upToSomething
      for_ (storyOtherSide attrs) removeFromGame
      pure s
    _ -> HarnessingATearInReality <$> liftRunMessage msg attrs
