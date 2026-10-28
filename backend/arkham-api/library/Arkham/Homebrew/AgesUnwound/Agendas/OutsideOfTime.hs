module Arkham.Homebrew.AgesUnwound.Agendas.OutsideOfTime (outsideOfTime) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.ForMovement
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getCanMoveToMatchingLocations)
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (enemyMoveTo, moveTo)

newtype OutsideOfTime = OutsideOfTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outsideOfTime :: AgendaCard OutsideOfTime
outsideOfTime = agenda (1, A) OutsideOfTime Cards.outsideOfTime (Static 3)

instance HasAbilities OutsideOfTime where
  getAbilities (OutsideOfTime a) =
    [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage OutsideOfTime where
  runMessage msg a@(OutsideOfTime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      locations <- select Anywhere
      chosen <- take 2 <$> shuffle locations
      for_ chosen \lid -> placeClues (attrs.ability 1) lid n
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      locationCount <- selectCount Anywhere
      if locationCount <= 2
        then push R1
        else do
          getRandomLocation unwarded >>= traverse_ \lid -> do
            selectEach (InvestigatorAt $ LocationWithId lid) \iid -> do
              destinations <-
                getCanMoveToMatchingLocations iid attrs (ConnectedFrom ForMovement $ LocationWithId lid)
              unless (null destinations) $ chooseTargetM iid destinations (moveTo attrs iid)
            connected <- select $ ConnectedFrom NotForMovement (LocationWithId lid)
            for_ (nonEmpty connected) \destinations ->
              selectEach (EnemyAt $ LocationWithId lid) \eid -> do
                destination <- sample destinations
                enemyMoveTo attrs eid destination
            placeBeneathAgendaDeck lid

          stillUnwarded <- selectAny $ Anywhere <> unwarded
          if stillUnwarded
            then revertAgenda attrs
            else do
              advanceAgendaDeck attrs
              whenM ((== 2) <$> getCurrentActStep)
                $ selectEach AnyAct \act' ->
                  push $ AdvanceAct act' (toSource attrs) #other
      pure a
    _ -> OutsideOfTime <$> liftRunMessage msg attrs
