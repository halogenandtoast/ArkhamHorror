module Arkham.Types.Enemy.Cards.SilasBishop
  ( SilasBishop(..)
  , silasBishop
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.Modifier
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers

newtype SilasBishop = SilasBishop EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silasBishop :: EnemyId -> SilasBishop
silasBishop uuid =
  SilasBishop
    $ baseAttrs uuid "02216"
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
