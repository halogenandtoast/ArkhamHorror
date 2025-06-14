module Arkham.Location.Cards.TempleRuins (templeRuins) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype TempleRuins = TempleRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

templeRuins :: LocationCard TempleRuins
templeRuins = symbolLabel $ location TempleRuins Cards.templeRuins 4 (Static 0)

instance HasAbilities TempleRuins where
  getAbilities (TempleRuins a) =
    extendRevealed1 a $ restricted a 1 Here $ ActionAbility [#explore] $ ActionCost 2

instance RunMessage TempleRuins where
  runMessage msg l@(TempleRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Explore iid (attrs.ability 1) $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> TempleRuins <$> liftRunMessage msg attrs
