module Arkham.Types.Enemy.Cards.StubbornDetective where


import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype StubbornDetective = StubbornDetective EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stubbornDetective :: EnemyId -> StubbornDetective
stubbornDetective uuid = StubbornDetective $ (weaknessBaseAttrs uuid "01102")
  { enemyHealthDamage = 1
  , enemySanityDamage = 0
  , enemyFight = 3
  , enemyHealth = Static 2
  , enemyEvade = 2
  , enemyPrey = SetToBearer
  }

instance HasId LocationId env InvestigatorId => HasModifiersFor env StubbornDetective where
  getModifiersFor _ (InvestigatorTarget iid) (StubbornDetective a@EnemyAttrs {..})
    | spawned a = do
      locationId <- getId @LocationId iid
      pure $ toModifiers a [ Blank | locationId == enemyLocation ]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env StubbornDetective where
  getActions i window (StubbornDetective attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env StubbornDetective where
  runMessage msg (StubbornDetective attrs) =
    StubbornDetective <$> runMessage msg attrs
