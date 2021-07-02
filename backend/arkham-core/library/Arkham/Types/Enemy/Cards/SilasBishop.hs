module Arkham.Types.Enemy.Cards.SilasBishop
  ( SilasBishop(..)
  , silasBishop
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Modifier

newtype SilasBishop = SilasBishop EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silasBishop :: EnemyCard SilasBishop
silasBishop = enemy SilasBishop Cards.silasBishop
  $ (healthDamageL .~ 2)
  . (sanityDamageL .~ 2)
  . (fightL .~ 3)
  . (healthL .~ PerPlayer 6)
  . (evadeL .~ 7)

instance HasModifiersFor env SilasBishop where
  getModifiersFor _ target (SilasBishop attrs) | isTarget attrs target =
    pure $ toModifiers attrs [CannotMakeAttacksOfOpportunity]
  getModifiersFor _ _ _ = pure []

instance ActionRunner env => HasActions env SilasBishop where
  getActions i window (SilasBishop attrs) = getActions i window attrs

instance (EnemyRunner env) => RunMessage env SilasBishop where
  runMessage msg (SilasBishop attrs) = SilasBishop <$> runMessage msg attrs
