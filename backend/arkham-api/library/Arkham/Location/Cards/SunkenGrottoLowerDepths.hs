module Arkham.Location.Cards.SunkenGrottoLowerDepths (
  sunkenGrottoLowerDepths,
  SunkenGrottoLowerDepths (..),
)
where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype SunkenGrottoLowerDepths = SunkenGrottoLowerDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoLowerDepths :: LocationCard SunkenGrottoLowerDepths
sunkenGrottoLowerDepths = location SunkenGrottoLowerDepths Cards.sunkenGrottoLowerDepths 0 (Static 0)

instance HasAbilities SunkenGrottoLowerDepths where
  getAbilities (SunkenGrottoLowerDepths a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ LocationInRow (-2) <> UnrevealedLocation)
      $ FastAbility (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage SunkenGrottoLowerDepths where
  runMessage msg l@(SunkenGrottoLowerDepths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationInRow (-2) <> UnrevealedLocation
      chooseOrRunOneM iid do
        targets locations $ lookAtRevealed iid (attrs.ability 1)
      pure l
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> SunkenGrottoLowerDepths <$> liftRunMessage msg attrs
