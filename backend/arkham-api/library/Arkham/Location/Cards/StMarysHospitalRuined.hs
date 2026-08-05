module Arkham.Location.Cards.StMarysHospitalRuined (stMarysHospitalRuined) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheDoomOfArkhamPartII.Helpers (scenarioI18n)
import Arkham.Token (Token (Clue))

newtype StMarysHospitalRuined = StMarysHospitalRuined LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stMarysHospitalRuined :: LocationCard StMarysHospitalRuined
stMarysHospitalRuined =
  location StMarysHospitalRuined Cards.stMarysHospitalRuined 3 (Static 1)

instance HasModifiersFor StMarysHospitalRuined where
  getModifiersFor (StMarysHospitalRuined a) = modifySelf a [CannotBeFullyFlooded]

instance HasAbilities StMarysHospitalRuined where
  getAbilities (StMarysHospitalRuined a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> youExist (InvestigatorWithClues $ atLeast 1)
            <> exists (enemyAt a <> EnemyCanBeDamagedBySource (a.ability 1))
        )
      $ freeReaction (EnemyEnters #after (be a) (enemyIs Enemies.cthulhuAncientEvil))

instance RunMessage StMarysHospitalRuined where
  runMessage msg l@(StMarysHospitalRuined attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      clues <- field InvestigatorClues iid
      scenarioI18n $ chooseAmount' iid "cluesToPlace" "$clues" 1 (min 3 clues) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$clues" -> n) (isTarget attrs -> True) | n > 0 -> do
      moveTokens (attrs.ability 1) iid attrs Clue n
      doStep n msg
      pure l
    DoStep n msg'@(ResolveAmounts iid _ (isTarget attrs -> True)) | n > 0 -> do
      enemies <- select $ enemyAt attrs <> EnemyCanBeDamagedBySource (attrs.ability 1)
      unless (null enemies) do
        chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
        doStep (n - 1) msg'
      pure l
    _ -> StMarysHospitalRuined <$> liftRunMessage msg attrs
