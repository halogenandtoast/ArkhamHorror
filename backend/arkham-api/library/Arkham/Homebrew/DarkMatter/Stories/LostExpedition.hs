module Arkham.Homebrew.DarkMatter.Stories.LostExpedition (lostExpedition) where

import Arkham.Card (toCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

newtype LostExpedition = LostExpedition StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostExpedition :: StoryCard LostExpedition
lostExpedition = story LostExpedition Cards.lostExpedition

{- | "Regardless of if this is the first time you read this: Advance the current
act and set this card aside, out of play." (The first-time branch is flavour
only.)
-}
instance RunMessage LostExpedition where
  runMessage msg s@(LostExpedition attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      advanceCurrentAct attrs
      push $ StoryMessage $ RemoveStory attrs.id
      push $ SetAsideCards [toCard attrs]
      pure s
    _ -> LostExpedition <$> liftRunMessage msg attrs
