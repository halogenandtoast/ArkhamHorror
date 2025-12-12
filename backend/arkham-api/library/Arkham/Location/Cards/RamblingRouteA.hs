module Arkham.Location.Cards.RamblingRouteA (ramblingRouteA) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype RamblingRouteA = RamblingRouteA LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteA :: LocationCard RamblingRouteA
ramblingRouteA = symbolLabel $ locationWith RamblingRouteA Cards.ramblingRouteA 2 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor RamblingRouteA where
  getModifiersFor (RamblingRouteA a) = do
    for_ a.position \pos ->
      modifySelect
        a
        (mapOneOf (ConcealedCardWithPlacement . InPosition) $ adjacentPositions pos)
        [EnemyFight 2, EnemyEvade 2]

instance HasAbilities RamblingRouteA where
  getAbilities (RamblingRouteA a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (OneOfDistanceCost LocationWithConcealedCard $ ResourceCost 1)

instance RunMessage RamblingRouteA where
  runMessage msg l@(RamblingRouteA attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> exposedInShadows iid attrs $ assignDamage iid (attrs.ability (-1)) 1
          MiddlePosition -> pure ()
          RightPosition -> exposedInShadows iid attrs $ assignHorror iid (attrs.ability (-1)) 1
      pure l
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalResourcePayment -> n) -> do
      if n == 0
        then push $ HandleTargetChoice iid (attrs.ability 1) (LocationTarget attrs.id)
        else do
          ls <- select $ LocationWithDistanceFrom n (be attrs) LocationWithConcealedCard
          chooseTargetM iid ls $ handleTarget iid (attrs.ability 1)
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      cs <- field LocationConcealedCards lid
      chooseTargetM iid cs $ revealConcealed iid (attrs.ability 1)
      pure l
    _ -> RamblingRouteA <$> liftRunMessage msg attrs
