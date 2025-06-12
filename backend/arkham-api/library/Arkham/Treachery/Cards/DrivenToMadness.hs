module Arkham.Treachery.Cards.DrivenToMadness (drivenToMadness) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Humanoid))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (EnemyEvaded)

newtype DrivenToMadness = DrivenToMadness TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drivenToMadness :: TreacheryCard DrivenToMadness
drivenToMadness = treachery DrivenToMadness Cards.drivenToMadness

instance HasModifiersFor DrivenToMadness where
  getModifiersFor (DrivenToMadness attrs) = case attrs.placement of
    AttachedToEnemy eid -> do
      modified_ attrs eid [EnemyFight 1, HealthModifier 1, EnemyEvade 1, RemoveKeyword Keyword.Aloof]
      modifySelect attrs Anyone [CannotParleyWith $ EnemyWithId eid]
    _ -> pure mempty

instance HasAbilities DrivenToMadness where
  getAbilities (DrivenToMadness attrs) = case attrs.attached of
    Just (EnemyTarget eid) -> [mkAbility attrs 1 $ forced $ EnemyEvaded #after You $ EnemyWithId eid]
    _ -> []

instance RunMessage DrivenToMadness where
  runMessage msg t@(DrivenToMadness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      humanoids <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Humanoid
      if null humanoids
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets humanoids \x -> do
          attachTreachery attrs x
          enemyCheckEngagement x
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (toAbilitySource attrs 1) attrs
      pure t
    _ -> DrivenToMadness <$> liftRunMessage msg attrs
