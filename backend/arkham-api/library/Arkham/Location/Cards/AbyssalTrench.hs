module Arkham.Location.Cards.AbyssalTrench (abyssalTrench) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AbyssalTrench = AbyssalTrench LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalTrench :: LocationCard AbyssalTrench
abyssalTrench = location AbyssalTrench Cards.abyssalTrench 0 (Static 1)

-- TODO: abilities

instance RunMessage AbyssalTrench where
  runMessage msg (AbyssalTrench attrs) = runQueueT $ AbyssalTrench <$> liftRunMessage msg attrs
