module Arkham.Enemy.Cards.WebSpinner
  ( webSpinner
  , WebSpinner(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype WebSpinner = WebSpinner EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

webSpinner :: EnemyCard WebSpinner
webSpinner = enemy WebSpinner Cards.webSpinner (0, Static 1, 0) (0, 0)

instance RunMessage WebSpinner where
  runMessage msg (WebSpinner attrs) =
    WebSpinner <$> runMessage msg attrs
