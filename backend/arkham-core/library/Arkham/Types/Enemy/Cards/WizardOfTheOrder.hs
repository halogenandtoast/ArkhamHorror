module Arkham.Types.Enemy.Cards.WizardOfTheOrder
  ( WizardOfTheOrder(..)
  , wizardOfTheOrder
  )
where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype WizardOfTheOrder = WizardOfTheOrder EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wizardOfTheOrder :: EnemyId -> WizardOfTheOrder
wizardOfTheOrder uuid =
  WizardOfTheOrder
    $ baseAttrs uuid "01170"
    $ (healthDamageL .~ 1)
    . (fightL .~ 4)
    . (healthL .~ Static 2)
    . (evadeL .~ 2)

instance HasModifiersFor env WizardOfTheOrder where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env WizardOfTheOrder where
  getActions i window (WizardOfTheOrder attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env WizardOfTheOrder where
  runMessage msg e@(WizardOfTheOrder attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    EndMythos -> pure $ WizardOfTheOrder $ attrs & doomL +~ 1
    _ -> WizardOfTheOrder <$> runMessage msg attrs
