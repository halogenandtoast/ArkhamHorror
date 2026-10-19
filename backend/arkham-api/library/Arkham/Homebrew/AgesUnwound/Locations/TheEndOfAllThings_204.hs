module Arkham.Homebrew.AgesUnwound.Locations.TheEndOfAllThings_204 (
  theEndOfAllThings_204,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (AdditionalCostToEnter), modifySelf)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (revealedL)
import Arkham.Matcher

newtype TheEndOfAllThings_204 = TheEndOfAllThings_204 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEndOfAllThings_204 :: LocationCard TheEndOfAllThings_204
theEndOfAllThings_204 =
  symbolLabel
    $ locationWith TheEndOfAllThings_204 Cards.theEndOfAllThings_204 3 (PerPlayer 2)
    $ revealedL
    .~ True

{- | "As an additional cost to move to The End of All Things, exile a non-weakness
asset you control."

Said where it belongs, as the location's own 'AdditionalCostToEnter'. The move
pays it, so an investigator with nothing to exile cannot afford the step at all
and a move that does not happen spends nothing -- neither of which the old
shape got right: it gated entry with a @CannotEnter@ and then charged the asset
on arrival, after the move was already irreversible.
-}
instance HasModifiersFor TheEndOfAllThings_204 where
  getModifiersFor (TheEndOfAllThings_204 a) =
    modifySelf a [AdditionalCostToEnter $ ExileAssetCost exileableAsset]

{- | "[action] Exile a non-weakness asset you control: Gain 2 clues from the token
pool. (Limit once per round.)"
-}
instance HasAbilities TheEndOfAllThings_204 where
  getAbilities (TheEndOfAllThings_204 a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (You <> HasMatchingAsset exileableAsset)) actionAbility

instance RunMessage TheEndOfAllThings_204 where
  runMessage msg l@(TheEndOfAllThings_204 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndExileAsset iid
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> TheEndOfAllThings_204 <$> liftRunMessage msg attrs
