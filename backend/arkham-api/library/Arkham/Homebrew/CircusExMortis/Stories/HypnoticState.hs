module Arkham.Homebrew.CircusExMortis.Stories.HypnoticState (hypnoticState) where

import Arkham.Ability
import Arkham.GameValue (GameValue (..))
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Question (AmountTarget (MaxAmountTarget))
import Arkham.Story.Import.Lifted

newtype HypnoticState = HypnoticState StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Starts Kidnapped Citizen (b) side up; flips to the story side and back.
hypnoticState :: StoryCard HypnoticState
hypnoticState = storyWith HypnoticState Cards.hypnoticState (flippedL .~ True)

instance HasAbilities HypnoticState where
  getAbilities (HypnoticState attrs)
    | attrs.flipped =
        [restricted attrs 1 OnSameLocation $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)]
    | otherwise = [restricted attrs 2 OnSameLocation actionAbility]

instance RunMessage HypnoticState where
  runMessage msg (HypnoticState attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure . HypnoticState $ attrs & flippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- perPlayer 1
      chooseAmounts
        iid
        (campaignI18n $ scope "hypnoticState" $ "$" <> labelKey "chooseHorror")
        (MaxAmountTarget n)
        [("Horror", (0, n))]
        attrs
      pure (HypnoticState attrs)
    ResolveAmounts iid (getChoiceAmount "Horror" -> hor) (isTarget attrs -> True) -> do
      mLoc <- getLocationOf attrs.placement
      for_ mLoc \loc -> do
        n <- perPlayer 1
        when (hor > 0) $ assignHorror iid attrs hor
        let required = max 0 (n - hor)
        withCost iid (GroupClueCost (Static required) (LocationWithId loc)) do
          addToVictory iid attrs
      pure . HypnoticState $ attrs & flippedL .~ True
    _ -> HypnoticState <$> liftRunMessage msg attrs
