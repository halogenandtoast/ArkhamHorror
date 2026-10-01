module Arkham.Homebrew.CircusExMortis.Locations.MossyGlen (mossyGlen) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MossyGlen = MossyGlen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mossyGlen :: LocationCard MossyGlen
mossyGlen = location MossyGlen Cards.mossyGlen 3 (PerPlayer 2)

{- | "After you release a ☾ token at Mossy Glen: Replenish 1 clue on Mossy Glen."

'ChaosTokenReleased' names the card the token was sealed on, so @You@ reads "a ☾ token
sealed on your own investigator card" -- which is where this scenario's ☾ tokens live --
and 'Here' supplies the "at Mossy Glen". 'LocationNotAtClueLimit' keeps the reaction from
being offered when the replenish could place nothing.
-}
instance HasAbilities MossyGlen where
  getAbilities (MossyGlen a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationNotAtClueLimit)
      $ freeReaction
      $ ChaosTokenReleased #after You moonToken

instance RunMessage MossyGlen where
  runMessage msg l@(MossyGlen attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ PlaceCluesUpToClueValue attrs.id (attrs.ability 1) 1
      pure l
    _ -> MossyGlen <$> liftRunMessage msg attrs
