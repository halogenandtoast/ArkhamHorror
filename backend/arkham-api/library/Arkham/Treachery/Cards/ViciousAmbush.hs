module Arkham.Treachery.Cards.ViciousAmbush (viciousAmbush) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Name (getFormatted)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheMidwinterGala.Helpers
import Arkham.Text
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ViciousAmbush = ViciousAmbush TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

viciousAmbush :: TreacheryCard ViciousAmbush
viciousAmbush = treachery ViciousAmbush Cards.viciousAmbush

instance RunMessage ViciousAmbush where
  runMessage msg t@(ViciousAmbush attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      noMonsters <- selectNone $ EnemyWithTrait Monster
      if noMonsters
        then gainSurge attrs
        else withLocationOf iid \loc -> do
          enemies <- select $ NearestEnemyTo iid $ EnemyWithTrait Monster
          chooseOrRunOneM iid do
            targets enemies \enemy -> do
              ready enemy
              moveUntil enemy loc
              forTarget enemy msg
      pure t
    ForTarget (EnemyTarget enemy) (Revelation iid (isSource attrs -> True)) -> do
      ok <- matches enemy (EnemyAt $ locationWithInvestigator iid)
      if ok
        then do
          enemyEngageInvestigator enemy iid
          initiateEnemyAttack enemy attrs iid
        else do
          formatted <- String <$> getFormatted enemy
          send $ scenarioI18n $ withVar "enemy" formatted $ toI18n "message.viciousAmbush.couldNotMove"
      pure t
    _ -> ViciousAmbush <$> liftRunMessage msg attrs
