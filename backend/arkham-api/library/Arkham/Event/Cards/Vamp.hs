module Arkham.Event.Cards.Vamp (vamp, Vamp (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Vamp = Vamp (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vamp :: EventCard Vamp
vamp = event (Vamp . (`with` Meta Nothing)) Cards.vamp

instance RunMessage Vamp where
  runMessage msg e@(Vamp (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ EnemyAt (locationWithInvestigator iid) <> canParleyEnemy iid
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      let skills = [#willpower, #intellect, #combat, #agility]
      chooseOne iid [SkillLabel s [ForSkillType s msg] | s <- skills]
      pure . Vamp $ attrs `with` meta {chosenEnemy = Just eid}
    ForSkillType sType (HandleTargetChoice iid (isSource attrs -> True) _) -> do
      sid <- getRandom
      parley sid iid attrs iid sType (Fixed 3)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      for_ (chosenEnemy meta) $ \eid -> do
        getSkillTestSkillTypes >>= traverse_ \case
          SkillWillpower -> removeDoom attrs eid 1
          SkillIntellect -> discoverAtYourLocation NotInvestigate iid attrs 1
          SkillAgility -> do
            automaticallyEvadeEnemy iid eid
            whenM (eid <=~> NonEliteEnemy) do
              locations <- select $ ConnectedFrom (locationWithEnemy eid) <> LocationCanBeEnteredBy eid
              when (notNull locations) do
                chooseOne iid [targetLabel lid [EnemyMove eid lid] | lid <- locations]
          SkillCombat -> nonAttackEnemyDamage attrs 2 eid
      pure e
    _ -> Vamp . (`with` meta) <$> liftRunMessage msg attrs
