module Arkham.Event.Cards.Vamp3 (vamp3, Vamp3 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestSkillTypes)
import Arkham.Id
import Arkham.Matcher
import Arkham.SkillType

newtype Meta = Meta {chosenEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Vamp3 = Vamp3 (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vamp3 :: EventCard Vamp3
vamp3 = event (Vamp3 . (`with` Meta Nothing)) Cards.vamp3

instance RunMessage Vamp3 where
  runMessage msg e@(Vamp3 (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ EnemyAt $ locationWithInvestigator iid
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      let skills = [#willpower, #intellect, #combat, #agility]
      chooseSome iid "Done choosing skills" [SkillLabel s [ForSkillType s msg] | s <- skills]
      pure . Vamp3 $ attrs `with` meta {chosenEnemy = Just eid}
    ForSkillType sType (HandleTargetChoice iid (isSource attrs -> True) _) -> do
      sid <- getRandom
      beginSkillTest sid iid attrs iid sType (Fixed 2)
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      for_ (chosenEnemy meta) $ \eid -> do
        getSkillTestSkillTypes >>= traverse_ \case
          SkillWillpower -> removeDoom attrs eid 1
          SkillIntellect -> discoverAtMatchingLocation NotInvestigate iid attrs (locationWithEnemy eid) 1
          SkillAgility -> do
            automaticallyEvadeEnemy iid eid
            whenM (eid <=~> NonEliteEnemy) do
              locations <- select $ ConnectedFrom (locationWithEnemy eid) <> LocationCanBeEnteredBy eid
              when (notNull locations) do
                chooseOne iid [targetLabel lid [EnemyMove eid lid] | lid <- locations]
          SkillCombat -> nonAttackEnemyDamage attrs 2 eid
      pure e
    _ -> Vamp3 . (`with` meta) <$> liftRunMessage msg attrs
