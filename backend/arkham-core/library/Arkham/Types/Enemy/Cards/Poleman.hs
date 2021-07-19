module Arkham.Types.Enemy.Cards.Poleman
  ( poleman
  , Poleman(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.AssetMatcher
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.LocationMatcher
import Arkham.Types.Prey

newtype Poleman = Poleman EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poleman :: EnemyCard Poleman
poleman = enemyWith
  Poleman
  Cards.poleman
  (4, Static 4, 2)
  (1, 1)
  ((spawnAtL ?~ LocationWithTitle "Canal-side")
  . (preyL .~ HasMostMatchingAsset (AssetWithTitle "Innocent Reveler"))
  )

instance HasModifiersFor env Poleman

instance EnemyAttrsHasActions env => HasActions env Poleman where
  getActions i window (Poleman attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env Poleman where
  runMessage msg (Poleman attrs) = Poleman <$> runMessage msg attrs
