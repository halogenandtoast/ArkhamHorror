module Arkham.Treachery.Cards.AbandonedToMadness (abandonedToMadness) where

import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Possessed))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AbandonedToMadness = AbandonedToMadness TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abandonedToMadness :: TreacheryCard AbandonedToMadness
abandonedToMadness = treachery AbandonedToMadness Cards.abandonedToMadness

instance HasModifiersFor AbandonedToMadness where
  getModifiersFor (AbandonedToMadness a) = case a.placement of
    AttachedToEnemy enemy ->
      getSkillTest >>= \case
        Nothing -> pure ()
        Just st -> maybeModified_ a (SkillTestTarget st.id) do
          enemy' <- hoistMaybe st.target.enemy
          guard $ enemy == enemy'
          liftGuardM $ orM [isParley, isFighting enemy, isEvading enemy]
          pure [Difficulty 1]
    _ -> pure ()

instance RunMessage AbandonedToMadness where
  runMessage msg t@(AbandonedToMadness attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select $ NearestEnemyTo iid $ EnemyWithTrait Possessed
      if null enemies
        then findEncounterCard iid attrs $ CardWithTrait Possessed <> #enemy
        else chooseTargetM iid enemies \enemy -> do
          attachTreachery attrs enemy
          placeDoom attrs enemy 1
      pure t
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      obtainCard card
      enemy <- createEnemy card ()
      attachTreachery attrs enemy
      pure t
    _ -> AbandonedToMadness <$> liftRunMessage msg attrs
