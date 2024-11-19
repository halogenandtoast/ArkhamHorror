module Arkham.Location.Cards.PrecariousIceSheet (precariousIceSheet, PrecariousIceSheet (..)) where

import Arkham.Ability
import Arkham.Helpers.Window (cardDrawn)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Hazard))

newtype PrecariousIceSheet = PrecariousIceSheet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

precariousIceSheet :: LocationCard PrecariousIceSheet
precariousIceSheet = symbolLabel $ location PrecariousIceSheet Cards.precariousIceSheet 4 (PerPlayer 1)

instance HasAbilities PrecariousIceSheet where
  getAbilities (PrecariousIceSheet attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 Here
      $ ReactionAbility
        (DrawCard #when You (CanCancelRevelationEffect $ basic $ #treachery <> withTrait Hazard) AnyDeck)
        (AddFrostTokenCost 1)

instance RunMessage PrecariousIceSheet where
  runMessage msg l@(PrecariousIceSheet attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      cancelCardEffects attrs card
      pure l
    _ -> PrecariousIceSheet <$> liftRunMessage msg attrs
