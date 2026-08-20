module Arkham.Treachery.Cards.TheDreamEaters.PointOfNoReturn.FalseAwakening (falseAwakening) where

import Arkham.Treachery.CardDefs.TheDreamEaters.PointOfNoReturn qualified as Cards
import Arkham.Treachery.Cards.TheDreamEaters.DarkSideOfTheMoon.FalseAwakening qualified as Base
import Arkham.Treachery.Import.Lifted

newtype FalseAwakening = FalseAwakening Base.FalseAwakening
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities FalseAwakening where
  getAbilities (FalseAwakening inner) = getAbilities inner

falseAwakening :: TreacheryCard FalseAwakening
falseAwakening =
  treachery
    (FalseAwakening . Base.FalseAwakening)
    Cards.falseAwakening

instance RunMessage FalseAwakening where
  runMessage msg (FalseAwakening inner) =
    FalseAwakening <$> runMessage msg inner
