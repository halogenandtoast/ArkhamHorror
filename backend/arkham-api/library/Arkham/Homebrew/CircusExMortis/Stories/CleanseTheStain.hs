module Arkham.Homebrew.CircusExMortis.Stories.CleanseTheStain (cleanseTheStain) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.CardDefs.Stories qualified as Cards
import Arkham.Story.Import.Lifted

{- | Destiny "stain" (:206). "Flip this card over and put it into play with 6 resources on
it" -- the back is the Defiled Woods location (:206b). Those 6 resources are stocked by the
location's own builder rather than pushed from here: a location's enters-play windows
resolve ahead of anything this handler queues behind the placement, and Defiled Woods'
Forced fires "if there are no resources on" it, which an empty window would see.
'otherSideIs' makes the def single-sided, so the location face enters play already revealed
and no 'RevealLocation' ever fires for it.
-}
newtype CleanseTheStain = CleanseTheStain StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cleanseTheStain :: StoryCard CleanseTheStain
cleanseTheStain = story CleanseTheStain Cards.cleanseTheStain

instance RunMessage CleanseTheStain where
  runMessage msg s@(CleanseTheStain attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      placeLocation_ Locations.defiledWoods
      pure s
    _ -> CleanseTheStain <$> liftRunMessage msg attrs
