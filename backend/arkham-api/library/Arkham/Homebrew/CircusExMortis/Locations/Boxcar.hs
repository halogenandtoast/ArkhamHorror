module Arkham.Homebrew.CircusExMortis.Locations.Boxcar (boxcar) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Boxcar = Boxcar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boxcar :: LocationCard Boxcar
boxcar = location Boxcar Cards.boxcar 2 (Static 1)

instance HasAbilities Boxcar where
  getAbilities (Boxcar a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction (PlayEvent #after You EventTargetsInvestigator)

instance RunMessage Boxcar where
  runMessage msg (Boxcar attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      drawCards iid (attrs.ability 1) 1
      pure $ Boxcar attrs
    _ -> Boxcar <$> liftRunMessage msg attrs
