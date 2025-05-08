module Arkham.Location.Cards.Ballroom (ballroom) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Ballroom = Ballroom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ballroom :: LocationCard Ballroom
ballroom = location Ballroom Cards.ballroom 4 (Static 0)

instance HasAbilities Ballroom where
  getAbilities (Ballroom a) =
    extendRevealed1 a
      $ groupLimit PerPhase
      $ restricted a 1 (Here <> youExist can.gain.resources)
      $ freeReaction
      $ PerformAction #after You #parley

instance RunMessage Ballroom where
  runMessage msg l@(Ballroom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 2
      pure l
    _ -> Ballroom <$> liftRunMessage msg attrs
