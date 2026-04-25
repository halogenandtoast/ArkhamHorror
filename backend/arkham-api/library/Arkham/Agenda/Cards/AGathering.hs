module Arkham.Agenda.Cards.AGathering (aGathering) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Helpers.Modifiers (modifyEach)
import Arkham.Modifier (ModifierType (CannotBeDamaged))
import Arkham.Treachery.Cards qualified as Treacheries

newtype AGathering = AGathering AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aGathering :: AgendaCard AGathering
aGathering = agenda (1, A) AGathering Cards.aGathering (Static 6)

instance HasModifiersFor AGathering where
  getModifiersFor (AGathering attrs) = do
    enemies <- select $ EnemyAt $ LocationWithTreachery (treacheryIs Treacheries.fire1)
    modifyEach attrs enemies [CannotBeDamaged]

instance HasAbilities AGathering where
  getAbilities _ = []

instance RunMessage AGathering where
  runMessage msg a@(AGathering attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      mServant <- selectOne $ enemyIs Enemies.servantOfFlameAWillingSacrifice
      case mServant of
        Just eid -> do
          undergroundCistern <- selectJust $ locationIs Locations.undergroundCistern
          push $ EnemyMove eid undergroundCistern
        Nothing -> do
          servantCards <- getSetAsideCardsMatching $ CardWithTitle "Servant of Flame"
          case listToMaybe servantCards of
            Just card -> do
              undergroundCistern <- selectJust $ locationIs Locations.undergroundCistern
              createEnemyAt_ card undergroundCistern
            Nothing -> pure ()

      iids <- select UneliminatedInvestigator
      for_ iids \iid -> do
        sid <- getRandom
        chooseOneM iid do
          labeled "Test {willpower} (4)" $ beginSkillTest sid iid attrs iid #willpower (Fixed 4)
          labeled "Automatically fail" $ directHorror iid attrs 1

      advanceAgendaDeck attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directHorror iid attrs 1
      pure a
    _ -> AGathering <$> liftRunMessage msg attrs
