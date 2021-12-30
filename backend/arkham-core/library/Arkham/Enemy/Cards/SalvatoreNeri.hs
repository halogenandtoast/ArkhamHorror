module Arkham.Enemy.Cards.SalvatoreNeri
  ( salvatoreNeri
  , SalvatoreNeri(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype SalvatoreNeri = SalvatoreNeri EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

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

instance EnemyRunner env => RunMessage env SalvatoreNeri where
  runMessage msg (SalvatoreNeri attrs) = SalvatoreNeri <$> runMessage msg attrs
