module Arkham.Types.Enemy.Cards.SalvatoreNeri
  ( salvatoreNeri
  , SalvatoreNeri(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype SalvatoreNeri = SalvatoreNeri EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

salvatoreNeri :: EnemyCard SalvatoreNeri
salvatoreNeri = enemy SalvatoreNeri Cards.salvatoreNeri (0, Static 3, 0) (0, 2)

instance
  ( HasModifiersFor env ()
  , HasSkillValue env InvestigatorId
  )
  => HasModifiersFor env SalvatoreNeri where
  getModifiersFor (InvestigatorSource iid) (EnemyTarget eid) (SalvatoreNeri attrs)
    | eid == toId attrs
    = do
      fightValue <- getSkillValue SkillCombat iid
      evadeValue <- getSkillValue SkillAgility iid
      pure $ toModifiers attrs [EnemyFight fightValue, EnemyEvade evadeValue]
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasActions env => HasActions env SalvatoreNeri where
  getActions i window (SalvatoreNeri attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env SalvatoreNeri where
  runMessage msg (SalvatoreNeri attrs) = SalvatoreNeri <$> runMessage msg attrs
