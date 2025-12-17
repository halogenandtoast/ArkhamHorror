module Arkham.Location.Cards.CliffsOfInsanity (cliffsOfInsanity) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement

newtype CliffsOfInsanity = CliffsOfInsanity LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cliffsOfInsanity :: LocationCard CliffsOfInsanity
cliffsOfInsanity =
  symbolLabel
    $ locationWith CliffsOfInsanity Cards.cliffsOfInsanity 3 (PerPlayer 2) connectsToAdjacent

instance HasAbilities CliffsOfInsanity where
  getAbilities (CliffsOfInsanity a) =
    extendRevealed1 a
      $ restricted a 1 (exists $ mapOneOf (ConcealedCardWithPlacement . InPosition) positions)
      $ forced
      $ RevealLocation #after You (be a)
   where
    positions = maybe [] adjacentPositions a.position

instance RunMessage CliffsOfInsanity where
  runMessage msg l@(CliffsOfInsanity attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> exposedInShadows iid attrs $ assignDamage iid (attrs.ability (-1)) 1
          MiddlePosition -> do
            cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
            unless (null cards) do
              exposedInShadows iid attrs $ chooseTargetM iid cards $ hollow iid
          RightPosition -> exposedInShadows iid attrs $ assignHorror iid (attrs.ability (-1)) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.position \pos -> do
        cs <- select $ mapOneOf (ConcealedCardWithPlacement . InPosition) (adjacentPositions pos)
        cliffPositions <- catMaybes <$> selectField LocationPosition (locationIs Cards.cliffsOfInsanity)
        otherPositions <-
          catMaybes <$> selectField LocationPosition (not_ $ locationIs Cards.cliffsOfInsanity)

        invalidPositions <-
          concatMap adjacentPositions
            . catMaybes
            <$> selectField LocationPosition (LocationWithModifier (ScenarioModifier "noCityOfRemnants"))
        let adjacentToCliffs = concatMap adjacentPositions cliffPositions
        let adjacentToOthers = concatMap adjacentPositions otherPositions
        let targetPositions :: [Pos] =
              nub adjacentToOthers
                \\ nub (adjacentToCliffs <> cliffPositions <> otherPositions <> invalidPositions)
        scenarioSpecific "distributeConcealedLocations" (iid, cs, targetPositions, targetPositions)
      pure l
    _ -> CliffsOfInsanity <$> liftRunMessage msg attrs
