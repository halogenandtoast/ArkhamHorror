module Arkham.Location.Cards.EastAntechamber (eastAntechamber) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Projection
import Arkham.Trait (Trait (Lift))

newtype EastAntechamber = EastAntechamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eastAntechamber :: LocationCard EastAntechamber
eastAntechamber = location EastAntechamber Cards.eastAntechamber 3 (Static 1)

instance HasAbilities EastAntechamber where
  getAbilities (EastAntechamber a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> DuringTurn You)
      $ FastAbility
      $ GroupClueCost (PerPlayer clues) (be a)
   where
    clues = case a.floodLevel of
      Just PartiallyFlooded -> 2
      Just FullyFlooded -> 2
      _ -> 1

instance RunMessage EastAntechamber where
  runMessage msg l@(EastAntechamber attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      mlift <- selectOne $ LocationWithTrait Lift
      for_ mlift \greatLift -> do
        pos <- fieldJust LocationPosition greatLift
        -- "down once" = toward level 1 (lower grid row); the lift can't go below
        -- level 1 (grid row 0). Repositioning the location card carries all of
        -- its attached cards/tokens/investigators with it.
        when (pos.row > 0) $ push $ PlaceGrid (GridLocation (updatePosition pos GridDown) greatLift)
      pure l
    _ -> EastAntechamber <$> liftRunMessage msg attrs
