module Arkham.Homebrew.CircusExMortis.Acts.NoFreeRides (noFreeRides) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Acts.ArrivingAt
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.CircusExMortis.NowArriving (Arrival (..))
import Arkham.Matcher
import Arkham.Trait

newtype NoFreeRides = NoFreeRides ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noFreeRides :: ActCard NoFreeRides
noFreeRides = act (2, A) NoFreeRides Cards.noFreeRides Nothing

instance HasAbilities NoFreeRides where
  getAbilities = actAbilities1 \a ->
    restricted a 1 (notExists $ EnemyWithTrait DarkYoung) $ Objective $ forced $ RoundEnds #when

instance RunMessage NoFreeRides where
  runMessage msg a@(NoFreeRides attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      arrivingAt attrs ArrivingAtStLouis (selectJust $ locationIs Locations.locomotiveEngine)
      advanceActDeck attrs
      pure a
    _ -> NoFreeRides <$> liftRunMessage msg attrs
