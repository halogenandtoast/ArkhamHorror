module Arkham.Treachery.Cards.ChildrenOfBlood.BloodMoney.SanguineRebirth (sanguineRebirth) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Enemies
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Matcher
import Arkham.Trait (Trait (Cultist))
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SanguineRebirth = SanguineRebirth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanguineRebirth :: TreacheryCard SanguineRebirth
sanguineRebirth = treachery SanguineRebirth Cards.sanguineRebirth

instance HasAbilities SanguineRebirth where
  getAbilities (SanguineRebirth a) = case a.placement of
    AttachedToEnemy eid -> [mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (EnemyWithId eid)]
    _ -> []

instance RunMessage SanguineRebirth where
  runMessage msg t@(SanguineRebirth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid (EnemyWithTrait Cultist)
      when (null enemies) $ findAndDrawEncounterCard iid $ #enemy <> CardWithTrait Cultist
      doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      enemies <- select $ NearestEnemyTo iid (EnemyWithTrait Cultist)
      chooseOrRunOneM iid $ targets enemies $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      case attrs.placement of
        AttachedToEnemy eid -> do
          mlid <- field EnemyLocation eid
          for_ mlid \lid -> do
            spawns <- getSetAsideCardsMatching (cardIs Enemies.spawnOfZburamoarte)
            for_ (take 1 spawns) \card -> createEnemyWith_ card lid createExhausted
        _ -> pure ()
      removeFromGame attrs
      pure t
    _ -> SanguineRebirth <$> liftRunMessage msg attrs
