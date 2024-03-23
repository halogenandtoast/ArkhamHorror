module Arkham.Treachery.Cards.TheSpinnerInDarkness
  ( theSpinnerInDarkness
  , TheSpinnerInDarkness(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheSpinnerInDarkness = TheSpinnerInDarkness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSpinnerInDarkness :: TreacheryCard TheSpinnerInDarkness
theSpinnerInDarkness = treachery TheSpinnerInDarkness Cards.theSpinnerInDarkness

instance RunMessage TheSpinnerInDarkness where
  runMessage msg t@(TheSpinnerInDarkness attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> TheSpinnerInDarkness <$> lift (runMessage msg attrs)
