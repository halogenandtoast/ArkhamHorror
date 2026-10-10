module Arkham.Homebrew.AgesUnwound.Stories.AThousandAvenuesOfAttack (
  aThousandAvenuesOfAttack,
) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

newtype AThousandAvenuesOfAttack = AThousandAvenuesOfAttack StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The reverse of the @:ages-unwound:196@ printing of /Today, a Thousand Times/.
aThousandAvenuesOfAttack :: StoryCard AThousandAvenuesOfAttack
aThousandAvenuesOfAttack = story AThousandAvenuesOfAttack Cards.aThousandAvenuesOfAttack

{- | "In player order, each investigator draws the top three cards of the
encounter deck, one at a time. Remove Today, a Thousand Times from the game."

"One at a time" is what the queue does for free: each draw is its own message and
resolves before the next, so a card that defeats an investigator is felt by the
draws that follow.
-}
instance RunMessage AThousandAvenuesOfAttack where
  runMessage msg s@(AThousandAvenuesOfAttack attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      eachInvestigator \iid -> replicateM_ 3 $ drawEncounterCard iid attrs
      for_ (storyOtherSide attrs) removeFromGame
      removeFromGame attrs
      pure s
    _ -> AThousandAvenuesOfAttack <$> liftRunMessage msg attrs
