module Arkham.Types.Enemy.Cards.ScreechingByakhee where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype ScreechingByakhee = ScreechingByakhee EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

screechingByakhee :: EnemyId -> ScreechingByakhee
screechingByakhee uuid =
  ScreechingByakhee
    $ baseAttrs uuid "01175"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 2)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 3)
    . (preyL .~ LowestRemainingSanity)

instance HasCount RemainingSanity env InvestigatorId => HasModifiersFor env ScreechingByakhee where
  getModifiersFor _ target (ScreechingByakhee attrs) | isTarget attrs target =
    do
      sanities <- map unRemainingSanity
        <$> traverse getCount (setToList $ enemyEngagedInvestigators attrs)
      pure $ toModifiers attrs $ if any (<= 4) sanities
        then [EnemyFight 1, EnemyEvade 1]
        else []
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env ScreechingByakhee where
  getActions i window (ScreechingByakhee attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env ScreechingByakhee where
  runMessage msg (ScreechingByakhee attrs) =
    ScreechingByakhee <$> runMessage msg attrs
