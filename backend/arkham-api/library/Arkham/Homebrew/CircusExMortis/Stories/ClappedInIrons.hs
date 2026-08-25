module Arkham.Homebrew.CircusExMortis.Stories.ClappedInIrons (clappedInIrons) where

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

newtype ClappedInIrons = ClappedInIrons StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Starts Kidnapped Citizen (b) side up; flips to the story side and back.
clappedInIrons :: StoryCard ClappedInIrons
clappedInIrons = storyWith ClappedInIrons Cards.clappedInIrons (flippedL .~ True)

instance HasAbilities ClappedInIrons where
  getAbilities (ClappedInIrons attrs)
    | attrs.flipped =
        [restricted attrs 1 OnSameLocation $ freeTrigger (GroupClueCost (PerPlayer 1) YourLocation)]
    | otherwise = [restricted attrs 2 OnSameLocation actionAbility]

instance RunMessage ClappedInIrons where
  runMessage msg (ClappedInIrons attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      pure . ClappedInIrons $ attrs & flippedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- perPlayer 1
      chooseAmounts
        iid
        (campaignI18n $ scope "clappedInIrons" $ "$" <> labelKey "chooseDamage")
        (MaxAmountTarget n)
        [("Damage", (0, n))]
        attrs
      pure (ClappedInIrons attrs)
    ResolveAmounts iid (getChoiceAmount "Damage" -> dmg) (isTarget attrs -> True) -> do
      mLoc <- getLocationOf attrs.placement
      for_ mLoc \loc -> do
        n <- perPlayer 1
        when (dmg > 0) $ assignDamage iid attrs dmg
        let required = max 0 (n - dmg)
        withCost iid (GroupClueCost (Static required) (LocationWithId loc)) do
          addToVictory iid attrs
      pure . ClappedInIrons $ attrs & flippedL .~ True
    _ -> ClappedInIrons <$> liftRunMessage msg attrs
