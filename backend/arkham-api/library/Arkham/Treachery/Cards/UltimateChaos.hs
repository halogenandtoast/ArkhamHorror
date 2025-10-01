module Arkham.Treachery.Cards.UltimateChaos (ultimateChaos) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UltimateChaos = UltimateChaos TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ultimateChaos :: TreacheryCard UltimateChaos
ultimateChaos = treachery UltimateChaos Cards.ultimateChaos

instance RunMessage UltimateChaos where
  runMessage msg t@(UltimateChaos attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    AfterRevelation iid tid | tid == toId attrs -> do
      instances <- select $ treacheryIs Cards.ultimateChaos
      when (length instances >= 3) $ do
        azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
        for_ instances (toDiscard attrs)
        chooseOneM iid $ scenarioI18n do
          labeled' "ultimateChaos.doom" $ placeDoom attrs azathoth 1
          labeled' "ultimateChaos.attack" $ eachInvestigator $ initiateEnemyAttack azathoth attrs
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      when (n >= 3) $ gainSurge attrs
      when (n >= 2) $ assignDamageAndHorror iid attrs 1 1
      attachTreachery attrs azathoth
      pure t
    _ -> UltimateChaos <$> liftRunMessage msg attrs
