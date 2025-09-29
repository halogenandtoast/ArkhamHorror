module Arkham.Location.Cards.SanctumDoorwayHoldingCells (sanctumDoorwayHoldingCells) where

import Arkham.Ability
import Arkham.Card
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SanctumDoorwayHoldingCells = SanctumDoorwayHoldingCells LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanctumDoorwayHoldingCells :: LocationCard SanctumDoorwayHoldingCells
sanctumDoorwayHoldingCells = location SanctumDoorwayHoldingCells Cards.sanctumDoorwayHoldingCells 1 (PerPlayer 1)

instance HasAbilities SanctumDoorwayHoldingCells where
  getAbilities (SanctumDoorwayHoldingCells a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)

instance RunMessage SanctumDoorwayHoldingCells where
  runMessage msg l@(SanctumDoorwayHoldingCells attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      findEncounterCard iid attrs $ card_ $ #enemy <> #cultist
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyAt_ card attrs
      pure l
    _ -> SanctumDoorwayHoldingCells <$> liftRunMessage msg attrs
