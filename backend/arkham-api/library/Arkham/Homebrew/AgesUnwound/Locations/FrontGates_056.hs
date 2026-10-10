module Arkham.Homebrew.AgesUnwound.Locations.FrontGates_056 (frontGates_056) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Terror))

newtype FrontGates_056 = FrontGates_056 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Scenario III's starting location.
frontGates_056 :: LocationCard FrontGates_056
frontGates_056 = symbolLabel $ location FrontGates_056 Cards.frontGates_056 4 (PerPlayer 1)

{- | "[action][action]: Discard a [[Terror]] treachery in your threat area."

Two actions, so 'doubleActionAbility': the printed icons are the whole cost.
-}
instance HasAbilities FrontGates_056 where
  getAbilities (FrontGates_056 a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> exists (TreacheryWithTrait Terror <> TreacheryInThreatAreaOf You))
        doubleActionAbility

instance RunMessage FrontGates_056 where
  runMessage msg l@(FrontGates_056 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      terrors <- select $ TreacheryWithTrait Terror <> treacheryInThreatAreaOf iid
      chooseTargetM iid terrors $ toDiscardBy iid (attrs.ability 1)
      pure l
    _ -> FrontGates_056 <$> liftRunMessage msg attrs
