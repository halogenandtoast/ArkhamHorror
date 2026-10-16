{- | Meltwater Stream -- the water gives him away, so a bad read can be taken
again.

The trigger is the window 'drawInvisibleCard' raises, which names the deck the
card came off; the replacement draw comes from that same deck rather than from
the card's trait.

"Cancel that card's effects" is the read itself: the story entity is never
created, so nothing on the card resolves -- including the line that would have
shuffled it back, which is why this ability shuffles it back itself. The second
draw waits for a 'doStep' so that it reads the deck with the cancelled card
already shuffled into it, as printed.
-}
module Arkham.Homebrew.TheInvisibleMan.Locations.MeltwaterStream (meltwaterStream) where

import Arkham.Ability
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Location.Import.Lifted

newtype MeltwaterStream = MeltwaterStream LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

meltwaterStream :: LocationCard MeltwaterStream
meltwaterStream = location MeltwaterStream Cards.meltwaterStream 3 (PerPlayer 2)

instance HasAbilities MeltwaterStream where
  getAbilities (MeltwaterStream a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "meltwaterStream.redraw"
      $ groupLimit PerRound
      $ restricted a 1 Here
      $ freeReaction drewInvisibleCard

instance RunMessage MeltwaterStream where
  runMessage msg l@(MeltwaterStream attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (invisibleCardDrawn -> Just (card, key)) _ -> do
      allMatchingDon't (isInvisibleCardRead card)
      shuffleBackInto key card
      doStep 1 msg
      pure l
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 1 (invisibleCardDrawn -> Just (_, key)) _) -> do
      drawInvisibleCard key iid
      pure l
    _ -> MeltwaterStream <$> liftRunMessage msg attrs
