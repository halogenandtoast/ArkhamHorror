{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Enemy.Cards.WizardOfTheOrder
  ( WizardOfTheOrder(..)
  , wizardOfTheOrder
  )
where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype WizardOfTheOrder = WizardOfTheOrder Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wizardOfTheOrder :: EnemyId -> WizardOfTheOrder
wizardOfTheOrder uuid =
  WizardOfTheOrder
    $ baseAttrs uuid "01170"
    $ (healthDamage .~ 1)
    . (fight .~ 4)
    . (health .~ Static 2)
    . (evade .~ 2)

instance HasModifiersFor env WizardOfTheOrder where
  getModifiersFor = noModifiersFor

instance HasModifiers env WizardOfTheOrder where
  getModifiers _ (WizardOfTheOrder Attrs {..}) =
    pure . concat . toList $ enemyModifiers

instance ActionRunner env => HasActions env WizardOfTheOrder where
  getActions i window (WizardOfTheOrder attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env WizardOfTheOrder where
  runMessage msg e@(WizardOfTheOrder attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAtEmptyLocation iid eid
    EndMythos -> pure $ WizardOfTheOrder $ attrs & doom +~ 1
    _ -> WizardOfTheOrder <$> runMessage msg attrs
