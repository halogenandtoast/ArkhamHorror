module Arkham.Homebrew.AgesUnwound.Locations.FrontGates_169 (frontGates_169) where

import Arkham.Ability
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (act1c)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FrontGates_169 = FrontGates_169 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frontGates_169 :: LocationCard FrontGates_169
frontGates_169 = symbolLabel $ location FrontGates_169 Cards.frontGates_169 4 (PerPlayer 1)

-- | "a copy of Preserve Causality or You Must Not Be Seen"
theParadoxes :: CardMatcher
theParadoxes = mapOneOf cardIs [Treacheries.preserveCausality, Treacheries.youMustNotBeSeen]

{- | "Forced - After you reveal Front Gates, if act 1c is in play: Reveal cards
from the encounter deck until a copy of Preserve Causality or You Must Not Be
Seen is revealed. Draw it. Shuffle the encounter deck."

Both named treacheries surge while only one act deck is in play, so the "if act
1c is in play" clause is what makes this a real threat rather than a free
encounter card.
-}
instance HasAbilities FrontGates_169 where
  getAbilities (FrontGates_169 a) =
    extendRevealed1 a
      $ restricted a 1 (ActExists act1c)
      $ forced
      $ RevealLocation #after You (be a)

instance RunMessage FrontGates_169 where
  runMessage msg l@(FrontGates_169 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      {- "Reveal cards ... until a copy ... is revealed. Draw it." -- the
      *topmost* copy, so this is not 'FindAndDrawEncounterCard' (which offers a
      free choice among every match) nor 'discardUntilFirst' (which discards the
      cards revealed on the way). The cards passed over go back into the deck,
      which is what the printed shuffle is for; the engine never reveals them, so
      the shuffle below only keeps the deck honest if something else peeked. -}
      deck <- getEncounterDeck
      for_ (find (`cardMatch` theParadoxes) deck.cards) \card -> do
        obtainCard card
        push $ InvestigatorDrewEncounterCard iid card
      push $ ShuffleDeck Deck.EncounterDeck
      pure l
    _ -> FrontGates_169 <$> liftRunMessage msg attrs
