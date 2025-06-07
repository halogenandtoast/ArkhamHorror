module Arkham.Location.Cards.OvergrownRuins (overgrownRuins) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Treacheries

newtype OvergrownRuins = OvergrownRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overgrownRuins :: LocationCard OvergrownRuins
overgrownRuins = symbolLabel $ location OvergrownRuins Cards.overgrownRuins 5 (PerPlayer 1)

instance HasAbilities OvergrownRuins where
  getAbilities (OvergrownRuins a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ treacheryIs Treacheries.poisoned <> TreacheryInThreatAreaOf You)
      $ forced
      $ Enters #after You (be a)

instance RunMessage OvergrownRuins where
  runMessage msg l@(OvergrownRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      setActions iid (attrs.ability 1) 0
      endYourTurn iid
      pure l
    _ -> OvergrownRuins <$> liftRunMessage msg attrs
