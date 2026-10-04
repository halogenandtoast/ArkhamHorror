module Arkham.Homebrew.AgainstTheWendigo.Stories.TheKnowledgeOfTheCold (
  theKnowledgeOfTheCold,
) where

import Arkham.Ability
import Arkham.Card (toCard)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)
import Arkham.Placement
import Arkham.Story.Import.Lifted

newtype TheKnowledgeOfTheCold = TheKnowledgeOfTheCold StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
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

instance HasAbilities TheKnowledgeOfTheCold where
  getAbilities (TheKnowledgeOfTheCold a) =
    [ restricted
        a
        1
        (exists $ locationIs Locations.templeOfIthaqua <> LocationWithHorror (atLeast 3))
        $ forced
        $ PlacedToken #after AnySource (LocationTargetMatches $ locationIs Locations.templeOfIthaqua) #horror
    ]

instance RunMessage TheKnowledgeOfTheCold where
  runMessage msg s@(TheKnowledgeOfTheCold attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      selectForMaybeM (locationIs Locations.templeOfIthaqua) \temple ->
        push $ StoryMessage $ PlaceStory (toCard attrs) (AttachedToLocation temple)
      pure s
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      selectForMaybeM (locationIs Locations.templeOfIthaqua) \temple -> do
        addToVictory lead temple
        push $ RemoveLocation temple
      -- Ithaqua's abilities are all on its revealed side, so it arrives face up.
      reveal =<< placeLocationCard Locations.ithaqua
      record YouAreTheCustodianOfIthaquasKnowledge
      removeStory attrs
      pure s
    _ -> TheKnowledgeOfTheCold <$> liftRunMessage msg attrs
