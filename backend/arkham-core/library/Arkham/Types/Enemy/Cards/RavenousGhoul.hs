module Arkham.Types.Enemy.Cards.RavenousGhoul where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype RavenousGhoul = RavenousGhoul EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ravenousGhoul :: EnemyId -> RavenousGhoul
ravenousGhoul uuid =
  RavenousGhoul
    $ baseAttrs uuid "01161"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)
    . (preyL .~ LowestRemainingHealth)

instance HasModifiersFor env RavenousGhoul where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env RavenousGhoul where
  getActions i window (RavenousGhoul attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env RavenousGhoul where
  runMessage msg (RavenousGhoul attrs) = RavenousGhoul <$> runMessage msg attrs
