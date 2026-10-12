module Arkham.Homebrew.AgainstTheWendigo.Stories.TheKnowledgeOfTheCold (
  theKnowledgeOfTheCold,
) where

import Arkham.Card (toCard)
import Arkham.Helpers.Location (connectBothWays)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers (riverLocation)
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Message.Lifted.Placement (place)
import Arkham.Placement
import Arkham.Projection
import Arkham.Story.Import.Lifted

newtype TheKnowledgeOfTheCold = TheKnowledgeOfTheCold StoryAttrs
  deriving anyclass (IsStory, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Part one attaches the story to the Temple of Ithaqua, which is what grants
the Temple its horror abilities (see
'Arkham.Homebrew.AgainstTheWendigo.Locations.TempleOfIthaqua'). Part two fires
once three horror are on the Temple: "Flip this card, put it into play in place
of Temple of Ithaqua... Put Temple of Ithaqua in the victory display with the
horrors that were on it."
-}
theKnowledgeOfTheCold :: StoryCard TheKnowledgeOfTheCold
theKnowledgeOfTheCold = persistStory $ story TheKnowledgeOfTheCold Cards.theKnowledgeOfTheCold

instance RunMessage TheKnowledgeOfTheCold where
  runMessage msg s@(TheKnowledgeOfTheCold attrs) = runQueueT $ case msg of
    {- 'Global', not attached to the Temple: a story's abilities only run while it
    is in play somewhere the engine will ask it, and an attachment is not that --
    nor is it laid out on the board. The Temple reads it by existence rather than
    by placement ('knowledgeIsBeingRead'), so Global serves both. -}
    ResolveThisStory _ (is attrs -> True) | attrs.placement /= Global -> do
      push $ StoryMessage $ PlaceStory (toCard attrs) Global
      pure s
    {- The second part, asked for by the reaction the Temple gains from part one:
    "{reaction} If this location has 3 horrors on it, an investigator on this
    location reads the second part of The Knowledge of the Cold." Resolving the
    aside card again is what reads it, the way the fates work. -}
    ResolveThisStory _ (is attrs -> True) -> do
      lead <- getLead
      -- The story is Ithaqua's own card, so it leaves before Ithaqua arrives.
      removeStory attrs
      selectForMaybeM (locationIs Locations.templeOfIthaqua) \temple -> do
        {- Ithaqua is "put into play in place of" the Temple, so it takes over the
        Temple's slot on the map and the Temple's connection to the River location
        directly to its East or West.

        "...and place the investigators and disengaged enemies who were in Temple
        of Ithaqua." They move onto Ithaqua BEFORE the Temple leaves play: an
        investigator whose location is removed from under them is eliminated,
        which is what used to happen here. Engaged enemies travel with the
        investigator they are engaged with, so only the disengaged ones are
        placed. -}
        slot <- field LocationPosition temple
        rivers <- select $ riverLocation <> connectedFrom (LocationWithId temple)
        investigators <- select $ InvestigatorAt (LocationWithId temple)
        enemies <- select $ enemyAt temple <> UnengagedEnemy
        {- "...put it into play in place of Temple of Ithaqua": into the Temple's own
        cell, so it inherits the Temple's place on the map. Its abilities are all
        on its revealed side, so it arrives face up -- and by `unsafeReveal`,
        since `reveal` cannot see a location that is only queued to be placed
        (see SylviasFateV1). -}
        ithaqua <- maybe placeLocationCard placeLocationCardInGrid slot Locations.ithaqua
        unsafeReveal ithaqua
        for_ rivers (connectBothWays ithaqua)
        for_ investigators (`place` AtLocation ithaqua)
        for_ enemies (`place` AtLocation ithaqua)
        -- "Put Temple of Ithaqua in the victory display with the horrors that
        -- were on it."
        addToVictory lead temple
        push $ RemoveLocation temple
      record YouAreTheCustodianOfIthaquasKnowledge
      pure s
    _ -> TheKnowledgeOfTheCold <$> liftRunMessage msg attrs
