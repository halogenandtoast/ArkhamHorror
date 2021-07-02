module Arkham.Types.Enemy.Cards.StubbornDetective where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Prey
import Arkham.Types.Target

newtype StubbornDetective = StubbornDetective EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stubbornDetective :: EnemyCard StubbornDetective
stubbornDetective = enemy StubbornDetective Cards.stubbornDetective
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 0)
  . (fightL .~ 3)
  . (healthL .~ Static 2)
  . (evadeL .~ 2)
  . (preyL .~ SetToBearer)

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
