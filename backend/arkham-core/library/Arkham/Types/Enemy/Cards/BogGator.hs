module Arkham.Types.Enemy.Cards.BogGator where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Trait

newtype BogGator = BogGator EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bogGator :: EnemyId -> BogGator
bogGator uuid =
  BogGator
    $ baseAttrs uuid "81022"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 2)
    . (healthL .~ Static 2)
    . (evadeL .~ 2)
    . (preyL .~ LowestSkill SkillAgility)

instance HasSet Trait env LocationId => HasModifiersFor env BogGator where
  getModifiersFor _ (EnemyTarget eid) (BogGator a@EnemyAttrs {..})
    | spawned a && eid == enemyId = do
      bayouLocation <- member Bayou <$> getSet enemyLocation
      pure $ toModifiers a $ if bayouLocation
        then [EnemyFight 2, EnemyEvade 2]
        else []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env BogGator where
  getActions i window (BogGator attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env BogGator where
  runMessage msg (BogGator attrs) = BogGator <$> runMessage msg attrs
