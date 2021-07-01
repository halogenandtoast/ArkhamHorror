module Arkham.Types.Enemy.Cards.SethBishop
  ( sethBishop
  , SethBishop(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.GameValue

newtype SethBishop = SethBishop EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sethBishop :: EnemyCard SethBishop
sethBishop = enemy SethBishop Cards.sethBishop
  $ (healthDamageL .~ 1)
  . (sanityDamageL .~ 1)
  . (fightL .~ 5)
  . (healthL .~ PerPlayer 3)
  . (evadeL .~ 5)

instance HasModifiersFor env SethBishop where
  getModifiersFor = noModifiersFor

instance EnemyAttrsHasActions env => HasActions env SethBishop where
  getActions i window (SethBishop attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env SethBishop where
  runMessage msg (SethBishop attrs) = SethBishop <$> runMessage msg attrs
