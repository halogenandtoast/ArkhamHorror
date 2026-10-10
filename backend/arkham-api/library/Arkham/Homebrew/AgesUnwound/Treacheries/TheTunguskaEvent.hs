module Arkham.Homebrew.AgesUnwound.Treacheries.TheTunguskaEvent (theTunguskaEvent) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype TheTunguskaEvent = TheTunguskaEvent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Winter's back puts this into play next to the act deck. It prints no
__Revelation__, so there is nothing to resolve when it arrives.
-}
theTunguskaEvent :: TreacheryCard TheTunguskaEvent
theTunguskaEvent = treachery TheTunguskaEvent Cards.theTunguskaEvent

{- | "[reaction] At the end of the round, investigators at Tunguska spend
2[per_investigator] clues, as a group: Flip this card over and resolve its text."

/Colour Out of Space/ ends "For the remainder of the game, it cannot be flipped
over again", which is the meta flag: once the ability has resolved the criterion
is 'Never'. The Task itself stays in play -- /Window of Opportunity/ is what
completes it.
-}
instance HasAbilities TheTunguskaEvent where
  getAbilities (TheTunguskaEvent a) =
    [ restricted a 1 (if flipped a then Never else youExist (at_ atTunguska))
        $ triggered (RoundEnds #when)
        $ GroupClueCost (PerPlayer 2) atTunguska
    ]
   where
    atTunguska = locationIs Locations.tunguska

flipped :: TreacheryAttrs -> Bool
flipped a = toResultDefault False a.meta

instance RunMessage TheTunguskaEvent where
  runMessage msg (TheTunguskaEvent attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.colourOutOfSpace
      pure . TheTunguskaEvent $ setMeta True attrs
    _ -> TheTunguskaEvent <$> liftRunMessage msg attrs
