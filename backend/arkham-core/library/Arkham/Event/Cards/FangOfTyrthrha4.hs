module Arkham.Event.Cards.FangOfTyrthrha4 (fangOfTyrthrha4, FangOfTyrthrha4 (..)) where

import Arkham.Criteria
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Movement
import Arkham.Projection

newtype FangOfTyrthrha4 = FangOfTyrthrha4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fangOfTyrthrha4 :: EventCard FangOfTyrthrha4
fangOfTyrthrha4 = event FangOfTyrthrha4 Cards.fangOfTyrthrha4

instance RunMessage FangOfTyrthrha4 where
  runMessage msg e@(FangOfTyrthrha4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      skillTestModifiers attrs iid [AddSkillToOtherSkill #agility #combat, DamageDealt 3]
      chooseFightEnemyMatch iid attrs
        $ CanFightEnemyWithOverride
        $ CriteriaOverride
        $ enemyExists
        $ EnemyAt RevealedLocation

      pure e
    ChoseEnemy sid iid (isSource attrs -> True) enemy -> do
      enemyLocation <- fieldJust EnemyLocation enemy
      yourLocation <- field InvestigatorLocation iid
      when (Just enemyLocation /= yourLocation) do
        chooseOne
          iid
          [Label "Move to enemy location" [Move $ move attrs iid enemyLocation], Label "Don't move" []]
      pure e
    _ -> FangOfTyrthrha4 <$> liftRunMessage msg attrs
