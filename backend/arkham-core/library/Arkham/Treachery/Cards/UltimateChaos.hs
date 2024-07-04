module Arkham.Treachery.Cards.UltimateChaos (ultimateChaos, UltimateChaos (..)) where

import Arkham.Attack
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher
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
      revelationSkillTest iid attrs #willpower (Fixed 4)
      pure t
    AfterRevelation iid tid | tid == toId attrs -> do
      instances <- select $ treacheryIs Cards.ultimateChaos
      when (length instances >= 3) $ do
        azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
        investigators <- getInvestigators
        for_ instances (toDiscard attrs)
        chooseOne
          iid
          [ Label "Place 1 Doom on Azathoth" [PlaceDoom (toSource attrs) (toTarget azathoth) 1]
          , Label
              "Azathoth attacks each investigator in player order"
              [toMessage $ enemyAttack azathoth (toSource attrs) investigator | investigator <- investigators]
          ]
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      when (n >= 3) $ gainSurge attrs
      when (n >= 2) $ assignDamageAndHorror iid attrs 1 1
      attachTreachery attrs azathoth
      pure t
    _ -> UltimateChaos <$> liftRunMessage msg attrs
