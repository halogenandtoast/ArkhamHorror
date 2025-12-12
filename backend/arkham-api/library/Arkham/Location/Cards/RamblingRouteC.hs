module Arkham.Location.Cards.RamblingRouteC (ramblingRouteC) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype RamblingRouteC = RamblingRouteC LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ramblingRouteC :: LocationCard RamblingRouteC
ramblingRouteC = symbolLabel $ locationWith RamblingRouteC Cards.ramblingRouteC 2 (PerPlayer 1) connectsToAdjacent

instance HasModifiersFor RamblingRouteC where
  getModifiersFor (RamblingRouteC a) = do
    for_ a.position \pos ->
      modifySelect
        a
        (mapOneOf (ConcealedCardWithPlacement . InPosition) $ adjacentPositions pos)
        [EnemyFight 2, EnemyEvade 2]

instance HasAbilities RamblingRouteC where
  getAbilities (RamblingRouteC a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 Here
      $ actionAbilityWithCost (OneOfDistanceCost LocationWithConcealedCard $ ResourceCost 1)

instance RunMessage RamblingRouteC where
  runMessage msg l@(RamblingRouteC attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> pure ()
          MiddlePosition -> do
            cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
            unless (null cards) do
              exposedInShadows iid attrs $ chooseTargetM iid cards $ hollow iid
          RightPosition -> do
            assets <- selectWithField AssetCard $ assetControlledBy iid <> AssetCanLeavePlayByNormalMeans
            unless (null assets) do
              exposedInShadows iid attrs do
                chooseOneM iid $ for assets \(aid, card) -> targeting aid $ hollow iid card
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
    _ -> RamblingRouteC <$> liftRunMessage msg attrs
