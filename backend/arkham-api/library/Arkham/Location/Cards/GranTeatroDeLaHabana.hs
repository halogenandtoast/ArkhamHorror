module Arkham.Location.Cards.GranTeatroDeLaHabana (granTeatroDeLaHabana) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier

newtype GranTeatroDeLaHabana = GranTeatroDeLaHabana LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

granTeatroDeLaHabana :: LocationCard GranTeatroDeLaHabana
granTeatroDeLaHabana = symbolLabel $ location GranTeatroDeLaHabana Cards.granTeatroDeLaHabana 4 (PerPlayer 1)

instance HasAbilities GranTeatroDeLaHabana where
  getAbilities (GranTeatroDeLaHabana a) =
    extendRevealed1 a
      $ playerLimit PerTurn
      $ restricted a 1 (Here <> DuringTurn You)
      $ FastAbility (ResourceCost 2)

instance RunMessage GranTeatroDeLaHabana where
  runMessage msg l@(GranTeatroDeLaHabana attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifier iid (attrs.ability 1) attrs (ShroudModifier (-2))
      pure l
    _ -> GranTeatroDeLaHabana <$> liftRunMessage msg attrs
