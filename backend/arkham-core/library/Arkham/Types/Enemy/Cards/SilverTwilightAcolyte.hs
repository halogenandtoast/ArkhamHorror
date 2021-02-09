module Arkham.Types.Enemy.Cards.SilverTwilightAcolyte where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype SilverTwilightAcolyte = SilverTwilightAcolyte EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silverTwilightAcolyte :: EnemyId -> SilverTwilightAcolyte
silverTwilightAcolyte uuid =
  SilverTwilightAcolyte $ (weaknessBaseAttrs uuid "01102")
    { enemyHealthDamage = 1
    , enemySanityDamage = 0
    , enemyFight = 2
    , enemyHealth = Static 3
    , enemyEvade = 3
    , enemyPrey = SetToBearer
    }

instance HasModifiersFor env SilverTwilightAcolyte where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env SilverTwilightAcolyte where
  getActions i window (SilverTwilightAcolyte attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env SilverTwilightAcolyte where
  runMessage msg (SilverTwilightAcolyte attrs@EnemyAttrs {..}) = case msg of
    EnemyAttack _ eid | eid == enemyId -> do
      unshiftMessage PlaceDoomOnAgenda
      SilverTwilightAcolyte <$> runMessage msg attrs
    _ -> SilverTwilightAcolyte <$> runMessage msg attrs
