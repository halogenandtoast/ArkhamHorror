{- | Beach -- a discount on the cards that find clues.

The discount is a [reaction] on the play window, which is where the cost is
still open: the active cost is built from 'getCostForCard' only once
@InitiatePlayCard@ runs, after @PlayCard #when@. 'CanReduceCostOf' is the other
half of the pattern (On Your Own (3), Rod of Animalism (1)) -- without it an
event the reaction would make affordable is never offered as playable.
-}
module Arkham.Homebrew.AndThenThereWereNone.Locations.Beach (beach) where

import Arkham.Ability
import Arkham.Card.CardDef (discoversCluesTag)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Location.Import.Lifted hiding (PlayCard)
import Arkham.Matcher

newtype Beach = Beach LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beach :: LocationCard Beach
beach = location Beach Cards.beach 2 (PerPlayer 3)

{- | "an event that investigates or discovers clues"

The two halves read off two different fields of the card def. @CardWithAction
\#investigate@ is the def's own Investigate action (Seeking Answers, Deciphered
Reality, Pilfer), and whatever that investigation discovers comes with it;
'discoversCluesTag' is the rest, the events that find clues some other way
(Working a Hunch, Drawn to the Flame, Evidence!, Scene of the Crime). Together
they are the whole group -- see the tag's haddock for what it does and does not
claim.
-}
investigatingEvent :: CardMatcher
investigatingEvent = #event <> oneOf [CardWithAction #investigate, CardTaggedWith discoversCluesTag]

instance HasModifiersFor Beach where
  getModifiersFor (Beach a) = whenRevealed a do
    modifySelect a (investigatorAt a.id) [CanReduceCostOf investigatingEvent 1]

instance HasAbilities Beach where
  getAbilities (Beach a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "beach.cost"
      $ restricted a 1 Here
      $ freeReaction (PlayCard #when You $ basic investigatingEvent)

instance RunMessage Beach where
  runMessage msg l@(Beach attrs) = runQueueT $ case msg of
    -- "That event gets -1 cost."
    UseCardAbility _ (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      reduceCostOf (attrs.ability 1) card 1
      pure l
    _ -> Beach <$> liftRunMessage msg attrs
