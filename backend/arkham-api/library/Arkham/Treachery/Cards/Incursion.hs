module Arkham.Treachery.Cards.Incursion (incursion) where

import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Mutated))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (Incursion)

newtype Incursion = Incursion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

incursion :: TreacheryCard Incursion
incursion = treacheryWith Incursion Cards.incursion (setMeta @[EnemyId] [])

instance RunMessage Incursion where
  runMessage msg t@(Incursion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      let difficulty = DividedByCalculation (SumCalculation [DoomCountCalculation, Fixed 1]) 2
      revelationSkillTest sid iid attrs #agility difficulty
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n | n > 0 -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      let chosen = toResultDefault @[EnemyId] [] attrs.meta
      farmhouse <- selectJust $ locationIs Locations.theFarmhouse
      enemies <- select $ NearestEnemyToLocation farmhouse (EnemyWithTrait Mutated <> not_ (EnemyOneOf $ map EnemyWithId chosen))
      unless (null enemies) do
        chooseTargetM iid enemies \enemy -> do
          push $ ForTarget (EnemyTarget enemy) msg'
          readyThis enemy
          sendMessage enemy HuntersMove
          -- Attack as if it were the enemy phase: engaged enemies attack their
          -- investigator, and a ready/unengaged enemy at The Farmhouse attacks
          -- The Captives (which reacts to the per-enemy EnemiesAttack below).
          sendMessage enemy (Do EnemiesAttack)
          push $ ForTarget (EnemyTarget enemy) EnemiesAttack
          doStep (n - 1) msg'
      pure t
    ForTarget (EnemyTarget enemy) (FailedThisSkillTest _ (isSource attrs -> True)) -> do
      let chosen = toResultDefault @[EnemyId] [] attrs.meta
      pure $ Incursion $ attrs & setMeta (enemy : chosen)
    _ -> Incursion <$> liftRunMessage msg attrs
