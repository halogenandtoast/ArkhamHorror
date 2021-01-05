module Arkham.Types.Enemy.Cards.GhoulFromTheDepths where

import Arkham.Import

import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype GhoulFromTheDepths = GhoulFromTheDepths Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ghoulFromTheDepths :: EnemyId -> GhoulFromTheDepths
ghoulFromTheDepths uuid =
  GhoulFromTheDepths
    $ baseAttrs uuid "50023"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 2)

instance HasModifiersFor env GhoulFromTheDepths where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env GhoulFromTheDepths where
  getActions i window (GhoulFromTheDepths attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env GhoulFromTheDepths where
  runMessage msg e@(GhoulFromTheDepths attrs@Attrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) enemyId (LocationWithTitle "Bathroom")
    _ -> GhoulFromTheDepths <$> runMessage msg attrs
