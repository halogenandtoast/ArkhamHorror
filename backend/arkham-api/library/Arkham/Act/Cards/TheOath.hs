module Arkham.Act.Cards.TheOath (theOath) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.EchoesOfThePast.Helpers
import Arkham.Trait

newtype TheOath = TheOath ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOath :: ActCard TheOath
theOath =
  act
    (3, A)
    TheOath
    Cards.theOath
    (Just $ GroupClueCost (PerPlayer 3) (locationIs Locations.hiddenLibrary))

instance HasModifiersFor TheOath where
  getModifiersFor (TheOath attrs) = do
    modifySelect
      attrs
      Anywhere
      [ConnectedToWhen (LocationWithTrait Passageway) (LocationWithTrait Passageway)]

instance RunMessage TheOath where
  runMessage msg a@(TheOath attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> scenarioI18n do
      leadChooseOneM do
        labeled' "theOath.r1" $ push R1
        labeled' "theOath.r2" $ push R2
      pure a
    _ -> TheOath <$> liftRunMessage msg attrs
