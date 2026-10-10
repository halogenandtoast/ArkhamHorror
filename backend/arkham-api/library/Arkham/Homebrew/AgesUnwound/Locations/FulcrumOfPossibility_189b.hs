module Arkham.Homebrew.AgesUnwound.Locations.FulcrumOfPossibility_189b (
  fulcrumOfPossibility_189b,
) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype FulcrumOfPossibility_189b = FulcrumOfPossibility_189b LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Revelation__ - Put Fulcrum of Possibility into play."

The reverse of @:ages-unwound:189@. Act 4 flips a location from beneath the
agenda deck and resolves its text, and this text is "put it into play" -- so the
corrupted place is simply placed, revealed, with its own higher shroud and its
clue back on it. There is no revelation handler to write: the card never becomes
a treachery, and placing it /is/ resolving it.
-}
fulcrumOfPossibility_189b :: LocationCard FulcrumOfPossibility_189b
fulcrumOfPossibility_189b =
  symbolLabel
    $ locationWith FulcrumOfPossibility_189b Cards.fulcrumOfPossibility_189b 5 (PerPlayer 1)
    $ revealedL
    .~ True

{- | "__Forced__ - At the end of the round, if there are any clues on this
location: In player order, each investigator draws the top card of the encounter
deck."
-}
instance HasAbilities FulcrumOfPossibility_189b where
  getAbilities (FulcrumOfPossibility_189b a) =
    extendRevealed1 a
      $ restricted a 1 (thisExists a LocationWithAnyClues)
      $ forced
      $ RoundEnds #when

instance RunMessage FulcrumOfPossibility_189b where
  runMessage msg l@(FulcrumOfPossibility_189b attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> FulcrumOfPossibility_189b <$> liftRunMessage msg attrs
