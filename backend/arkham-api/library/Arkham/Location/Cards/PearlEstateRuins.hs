module Arkham.Location.Cards.PearlEstateRuins (pearlEstateRuins) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheSilentHeath.Helpers

newtype PearlEstateRuins = PearlEstateRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pearlEstateRuins :: LocationCard PearlEstateRuins
pearlEstateRuins =
  symbolLabel $ locationWith PearlEstateRuins Cards.pearlEstateRuins 3 (Static 1) connectsToAdjacent

instance HasAbilities PearlEstateRuins where
  getAbilities (PearlEstateRuins a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "pearlEstateRuins.resign"
      $ restricted a 1 Here
      $ ActionAbility #resign Nothing (ActionCost 1)

instance RunMessage PearlEstateRuins where
  runMessage msg l@(PearlEstateRuins attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (assetControlledBy iid <> AssetWithTitle "Crystal Remains") (addToVictory iid)
      resign iid
      pure l
    _ -> PearlEstateRuins <$> liftRunMessage msg attrs
