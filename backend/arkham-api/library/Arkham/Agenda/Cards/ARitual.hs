module Arkham.Agenda.Cards.ARitual (aRitual) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card (flipCard, replaceCard)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Modifiers (modifyEach)
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Modifier (ModifierType (CannotBeDamaged))
import Arkham.Projection (field)
import Arkham.Treachery.Cards qualified as Treacheries

newtype ARitual = ARitual AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aRitual :: AgendaCard ARitual
aRitual = agenda (2, A) ARitual Cards.aRitual (Static 10)

instance HasModifiersFor ARitual where
  getModifiersFor (ARitual attrs) = do
    enemies <- select $ EnemyAt $ LocationWithTreachery (treacheryIs Treacheries.fire1)
    modifyEach attrs enemies [CannotBeDamaged]

instance HasAbilities ARitual where
  getAbilities _ = []

instance RunMessage ARitual where
  runMessage msg a@(ARitual attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      step <- getCurrentActStep
      if step == 1
        then do
          undergroundCistern <- selectJust $ locationIs Locations.undergroundCistern
          reveal undergroundCistern
          elokoss <- flipCard <$> getSetAsideCard Enemies.elokossFaintEmbers
          replaceCard elokoss.id elokoss
          createEnemyAt_ elokoss undergroundCistern
        else do
          selectOne (enemyIs Enemies.elokossFaintEmbers) >>= \case
            Just eid -> do
              card <- field EnemyCard eid
              push $ ReplaceEnemy eid (flipCard card) Swap
            Nothing -> do
              elokoss <- getSetAsideCard Enemies.elokossMotherOfFlame
              undergroundCistern <- selectJust $ locationIs Locations.undergroundCistern
              createEnemyAt_ elokoss undergroundCistern

      advanceAgendaDeck attrs
      removeActDeck
      pure a
    _ -> ARitual <$> liftRunMessage msg attrs
