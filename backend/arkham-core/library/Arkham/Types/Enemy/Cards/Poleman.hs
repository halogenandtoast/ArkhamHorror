module Arkham.Types.Enemy.Cards.Poleman
  ( poleman
  , Poleman(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs

newtype Poleman = Poleman EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poleman :: EnemyCard Poleman
poleman = enemy Poleman Cards.poleman (0, Static 1, 0) (0, 0)

instance HasModifiersFor env Poleman

instance EnemyAttrsHasActions env => HasActions env Poleman where
  getActions i window (Poleman attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env Poleman where
  runMessage msg (Poleman attrs) = Poleman <$> runMessage msg attrs
