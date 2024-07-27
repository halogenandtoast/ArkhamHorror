module Arkham.Event.Cards.Grift (grift, Grift (..)) where

import Arkham.Attack
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Id
import Arkham.Matcher

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Grift = Grift (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grift :: EventCard Grift
grift = event (Grift . (`with` Meta Nothing)) Cards.grift

instance RunMessage Grift where
  runMessage msg e@(Grift (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ EnemyAt $ locationWithInvestigator iid
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid #agility (Fixed 0)
      pure . Grift $ With attrs (meta {chosenEnemy = Just eid})
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      gainResourcesIfCan iid attrs $ min 6 n
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      for_ (chosenEnemy meta) \eid ->
        push $ EnemyAttack $ enemyAttack eid attrs iid
      pure e
    _ -> Grift . (`with` meta) <$> liftRunMessage msg attrs
