module Arkham.Homebrew.AgesUnwound.Locations.TheEndOfAllThings_204 (
  theEndOfAllThings_204,
) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (CannotEnter), modified_)
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

The /gate/ is a modifier: an investigator with nothing to exile cannot come here
at all. The payment itself is the 'SilentForcedAbility' below.

TODO(ages-unwound): the engine has no @ExileAssetCost AssetMatcher@ -- the sibling
of 'Arkham.Cost.DiscardAssetCost' -- and 'Arkham.Cost.ExileCost' names one fixed
target, so this cannot be hung on 'Arkham.Modifier.AdditionalCostToEnter' where it
belongs. Splitting it into "cannot enter without one" plus "exile one on arrival"
keeps both halves true; what it loses is the cost's timing, so a move that is
cancelled after the fact has still spent the asset.
-}
instance HasModifiersFor TheEndOfAllThings_204 where
  getModifiersFor (TheEndOfAllThings_204 a) = do
    cannotPay <- select $ not_ (HasMatchingAsset exileableAsset)
    for_ cannotPay \iid -> modified_ a iid [CannotEnter a.id]

{- | "[action] Exile a non-weakness asset you control: Gain 2 clues from the token
pool. (Limit once per round.)"

Ability 2 is the arrival payment described above -- a 'SilentForcedAbility',
because the card prints no __Forced__ of its own and the hook exists only to
charge a cost the engine cannot express as one.
-}
instance HasAbilities TheEndOfAllThings_204 where
  getAbilities (TheEndOfAllThings_204 a) =
    extendRevealed
      a
      [ groupLimit PerRound
          $ restricted a 1 (Here <> exists (You <> HasMatchingAsset exileableAsset)) actionAbility
      , mkAbility a 2 $ SilentForcedAbility $ Enters #after You (be a)
      ]

instance RunMessage TheEndOfAllThings_204 where
  runMessage msg l@(TheEndOfAllThings_204 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndExileAsset iid
      gainClues iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseAndExileAsset iid
      pure l
    _ -> TheEndOfAllThings_204 <$> liftRunMessage msg attrs
