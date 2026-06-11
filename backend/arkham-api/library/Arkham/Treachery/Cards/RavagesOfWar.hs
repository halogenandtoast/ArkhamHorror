module Arkham.Treachery.Cards.RavagesOfWar (ravagesOfWar) where

import Arkham.Matcher
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RavagesOfWar = RavagesOfWar TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravagesOfWar :: TreacheryCard RavagesOfWar
ravagesOfWar = treachery RavagesOfWar Cards.ravagesOfWar

instance RunMessage RavagesOfWar where
  runMessage msg t@(RavagesOfWar attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      hunters <- select $ NonEliteEnemy <> HunterEnemy <> ReadyEnemy <> UnengagedEnemy
      for_ hunters \enemy -> push $ ForTarget (toTarget enemy) HuntersMove
      warring <-
        sortEnemiesByFaction =<< select (NonEliteEnemy <> warringEnemy <> ReadyEnemy <> UnengagedEnemy)
      for_ warring \enemy -> push $ ScenarioSpecific "warringMove" (toJSON enemy)
      doStep 1 msg
      pure t
    DoStep 1 (Revelation _ (isSource attrs -> True)) -> do
      engaged <- select $ NonEliteEnemy <> ReadyEnemy <> EnemyIsEngagedWith Anyone
      warringAttackers <-
        filterM (\enemy -> enemy <=~> NonEliteEnemy)
          =<< sortEnemiesByFaction
          =<< getWarringAttackers
      if null engaged && null warringAttackers
        then gainSurge attrs
        else do
          for_ engaged \enemy ->
            selectEach (investigatorEngagedWith enemy) \iid -> initiateEnemyAttack enemy attrs iid
          for_ warringAttackers \enemy ->
            push $ ScenarioSpecific "warringAttack" (toJSON enemy)
      pure t
    _ -> RavagesOfWar <$> liftRunMessage msg attrs
