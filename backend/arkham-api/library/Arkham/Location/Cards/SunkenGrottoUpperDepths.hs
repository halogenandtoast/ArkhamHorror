module Arkham.Location.Cards.SunkenGrottoUpperDepths (
  sunkenGrottoUpperDepths,
  SunkenGrottoUpperDepths (..),
)
where

import Arkham.Ability
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype SunkenGrottoUpperDepths = SunkenGrottoUpperDepths LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoUpperDepths :: LocationCard SunkenGrottoUpperDepths
sunkenGrottoUpperDepths = location SunkenGrottoUpperDepths Cards.sunkenGrottoUpperDepths 0 (Static 0)

instance HasModifiersFor SunkenGrottoUpperDepths where
  getModifiersFor (InvestigatorTarget iid) (SunkenGrottoUpperDepths attrs) = do
    hasBlueKey <- iid <=~> InvestigatorWithKey BlueKey
    toModifiers attrs [CannotEnter (toId attrs) | not attrs.revealed && not hasBlueKey]
  getModifiersFor _ _ = pure []

instance HasAbilities SunkenGrottoUpperDepths where
  getAbilities (SunkenGrottoUpperDepths a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ LocationInRow (-1) <> UnrevealedLocation)
      $ FastAbility (GroupClueCost (PerPlayer 1) (be a))

instance RunMessage SunkenGrottoUpperDepths where
  runMessage msg l@(SunkenGrottoUpperDepths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationInRow (-1) <> UnrevealedLocation
      chooseOrRunOneM iid do
        targets locations $ lookAtRevealed iid (attrs.ability 1)
      pure l
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> SunkenGrottoUpperDepths <$> liftRunMessage msg attrs
