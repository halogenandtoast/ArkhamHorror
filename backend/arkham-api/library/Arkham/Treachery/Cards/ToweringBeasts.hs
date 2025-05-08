module Arkham.Treachery.Cards.ToweringBeasts (toweringBeasts) where

import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ToweringBeasts = ToweringBeasts TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toweringBeasts :: TreacheryCard ToweringBeasts
toweringBeasts = treachery ToweringBeasts Cards.toweringBeasts

instance HasModifiersFor ToweringBeasts where
  getModifiersFor (ToweringBeasts attrs) = case attrs.placement of
    AttachedToEnemy eid -> modified_ attrs eid [EnemyFight 1, HealthModifier 1]
    _ -> pure mempty

instance RunMessage ToweringBeasts where
  runMessage msg t@(ToweringBeasts attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      broods <- getBroodOfYogSothoth
      unless (null broods) do
        chooseTargetM iid broods \brood -> do
          attachTreachery attrs brood
          withLocationOf iid \lid -> do
            lid' <- selectJust (locationWithEnemy brood)
            when (lid == lid') $ assignDamage iid attrs 1
      pure t
    _ -> ToweringBeasts <$> liftRunMessage msg attrs
