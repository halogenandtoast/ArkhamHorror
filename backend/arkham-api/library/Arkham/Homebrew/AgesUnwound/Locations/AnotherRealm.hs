module Arkham.Homebrew.AgesUnwound.Locations.AnotherRealm (anotherRealm) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Story (readStory)
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype AnotherRealm = AnotherRealm LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Strange Portal/ puts this into play. It enters on its location side with
its clue on it -- the other face is the /Destination/ story -- so it is revealed
from the start and flippable, as the Dim Carcosa locations are.
-}
anotherRealm :: LocationCard AnotherRealm
anotherRealm =
  locationWith AnotherRealm Cards.anotherRealm 4 (PerPlayer 1)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

{- | "__Forced__ - When an investigator at Another Realm draws an enemy: Discard
it. That investigator draws the top card of the encounter deck. /
[action] If there are no clues on Another Realm: Flip this card over and resolve
its text."
-}
instance HasAbilities AnotherRealm where
  getAbilities (AnotherRealm a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ DrawCard #when (investigatorAt a.id) (basic #enemy) EncounterDeck
      , restricted a 2 (Here <> NoCluesOnThis) actionAbility
      ]

instance RunMessage AnotherRealm where
  runMessage msg l@(AnotherRealm attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      quietCancelCardDraw card
      case card of
        EncounterCard ec -> push $ AddToEncounterDiscard ec
        _ -> pure ()
      drawEncounterCard iid (attrs.ability 1)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.destination
      pure . AnotherRealm $ attrs & canBeFlippedL .~ False
    _ -> AnotherRealm <$> liftRunMessage msg attrs
