module Arkham.Agenda.Cards.EverShiftingWalls (everShiftingWalls) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelectMapM)
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Projection

newtype EverShiftingWalls = EverShiftingWalls AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

everShiftingWalls :: AgendaCard EverShiftingWalls
everShiftingWalls = agenda (3, A) EverShiftingWalls Cards.everShiftingWalls (Static 8)

instance HasModifiersFor EverShiftingWalls where
  getModifiersFor (EverShiftingWalls a) = do
    healthModifier <- perPlayer 2
    modifySelect a (enemyIs Enemies.theInescapable) [HealthModifier healthModifier]

    -- "When The Inescapable moves via its hunter keyword, each location is
    -- considered to be connected to each adjacent location." In this scenario
    -- The Inescapable only moves via its hunter keyword, and there is no flag to
    -- distinguish a hunter move from other movement during a modifier query, so
    -- we gate on it currently moving (mirrors DrownedShanty's deep-one-move
    -- connection). A location's grid neighbors are its adjacent positions.
    inescapableMoving <- selectAny (MovingEnemy <> enemyIs Enemies.theInescapable)
    when inescapableMoving do
      modifySelectMapM a Anywhere \loc -> do
        mpos <- field LocationPosition loc
        pure case mpos of
          Nothing -> []
          Just pos ->
            [ ConnectedToWhen
                (LocationWithId loc)
                (mapOneOf LocationInPosition pos.adjacents)
            ]

instance HasAbilities EverShiftingWalls where
  getAbilities (EverShiftingWalls a) =
    [ restricted a 1 (youExist $ at_ FullyFloodedLocation)
        $ forced
        $ TurnBegins #when You
    ]

instance RunMessage EverShiftingWalls where
  runMessage msg a@(EverShiftingWalls attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      struggleForAir attrs iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach (not_ ResignedInvestigator) \iid -> do
        sufferPhysicalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> EverShiftingWalls <$> liftRunMessage msg attrs
