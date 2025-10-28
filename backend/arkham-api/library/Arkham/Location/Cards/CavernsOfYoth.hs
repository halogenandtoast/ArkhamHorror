module Arkham.Location.Cards.CavernsOfYoth (cavernsOfYoth) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype CavernsOfYoth = CavernsOfYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernsOfYoth :: LocationCard CavernsOfYoth
cavernsOfYoth = symbolLabel $ location CavernsOfYoth Cards.cavernsOfYoth 1 (PerPlayer 1)

instance HasAbilities CavernsOfYoth where
  getAbilities (CavernsOfYoth a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage CavernsOfYoth where
  runMessage msg l@(CavernsOfYoth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getCurrentDepth
      placeClues (attrs.ability 1) attrs n
      pure l
    _ -> CavernsOfYoth <$> liftRunMessage msg attrs
