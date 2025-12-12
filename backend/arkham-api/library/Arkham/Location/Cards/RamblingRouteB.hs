module Arkham.Location.Cards.RamblingRouteB (ramblingRouteB) where

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

newtype RamblingRouteB = RamblingRouteB LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteB :: LocationCard RamblingRouteB
ramblingRouteB = symbolLabel $ locationWith RamblingRouteB Cards.ramblingRouteB 2 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor RamblingRouteB where
  getModifiersFor (RamblingRouteB a) = do
    for_ a.position \pos ->
      modifySelect
        a
        (mapOneOf (ConcealedCardWithPlacement . InPosition) $ adjacentPositions pos)
        [EnemyFight 2, EnemyEvade 2]

instance HasAbilities RamblingRouteB where
  getAbilities (RamblingRouteB a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (OneOfDistanceCost LocationWithConcealedCard $ ResourceCost 1)

instance RunMessage RamblingRouteB where
  runMessage msg l@(RamblingRouteB attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> do
            whenMatch iid InvestigatorWithAnyResources do
              exposedInShadows iid attrs $ loseResources iid (attrs.ability (-1)) 2
          MiddlePosition -> do
            whenMatch iid InvestigatorWithAnyActionsRemaining do
              exposedInShadows iid attrs $ loseActions iid (attrs.ability (-1)) 1
          RightPosition -> pure ()
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
    _ -> RamblingRouteB <$> liftRunMessage msg attrs
