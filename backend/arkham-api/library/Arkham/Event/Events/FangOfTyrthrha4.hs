module Arkham.Event.Events.FangOfTyrthrha4 (fangOfTyrthrha4) where

import Arkham.Criteria
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Projection

newtype FangOfTyrthrha4 = FangOfTyrthrha4 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fangOfTyrthrha4 :: EventCard FangOfTyrthrha4
fangOfTyrthrha4 = event FangOfTyrthrha4 Cards.fangOfTyrthrha4

instance RunMessage FangOfTyrthrha4 where
  runMessage msg e@(FangOfTyrthrha4 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [AddSkillToOtherSkill #agility #combat, DamageDealt 3]
      chooseFightEnemyMatch sid iid attrs
        $ CanFightEnemyWithOverride
        $ CriteriaOverride
        $ enemyExists
        $ EnemyAt RevealedLocation

      pure e
    ChoseEnemy _sid iid (isSource attrs -> True) enemy -> do
      enemyLocation <- fieldJust EnemyLocation enemy
      yourLocation <- field InvestigatorLocation iid
      when (Just enemyLocation /= yourLocation) do
        chooseOneM iid do
          labeled "Move to enemy location" $ moveTo attrs iid enemyLocation
          labeled "Don't move" nothing
      pure e
    _ -> FangOfTyrthrha4 <$> liftRunMessage msg attrs
