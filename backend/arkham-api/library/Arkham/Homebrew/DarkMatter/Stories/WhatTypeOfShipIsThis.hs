module Arkham.Homebrew.DarkMatter.Stories.WhatTypeOfShipIsThis (whatTypeOfShipIsThis) where

import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories, campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Import.Lifted

newtype WhatTypeOfShipIsThis = WhatTypeOfShipIsThis StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatTypeOfShipIsThis :: StoryCard WhatTypeOfShipIsThis
whatTypeOfShipIsThis = story WhatTypeOfShipIsThis Cards.whatTypeOfShipIsThis

{- | "Each investigator at your location adds 1 tally mark next to their
'Memories'. You must either (choose one):
- Place 1 doom on the current agenda and add this card to the victory display.
  This effect may cause the current agenda to advance.
- Remove this card from the game."
-}
instance RunMessage WhatTypeOfShipIsThis where
  runMessage msg s@(WhatTypeOfShipIsThis attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      colocated <- select $ colocatedWith iid
      for_ colocated (`addMemories` 1)
      chooseOneM iid $ campaignI18n do
        labeled' "whatTypeOfShipIsThis.placeDoom" do
          placeDoomOnAgendaAndCheckAdvance 1
          addToVictory iid attrs
        labeled' "whatTypeOfShipIsThis.removeFromGame" $ push $ RemoveFromGame (toTarget attrs)
      pure s
    _ -> WhatTypeOfShipIsThis <$> liftRunMessage msg attrs
