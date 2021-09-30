module Arkham.Types.Effect.Effects.TommyMalloy
  ( TommyMalloy(..)
  , tommyMalloy
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype TommyMalloy = TommyMalloy EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tommyMalloy :: EffectArgs -> TommyMalloy
tommyMalloy = TommyMalloy . uncurry4 (baseAttrs "60103")

instance HasModifiersFor env TommyMalloy where
  getModifiersFor _ target (TommyMalloy attrs) | effectTarget attrs == target =
    pure $ toModifiers attrs [MaxDamageTaken 1]
  getModifiersFor _ _ _ = pure []

isTakeDamage :: EffectAttrs -> Window -> Bool
isTakeDamage attrs window = case effectTarget attrs of
  EnemyTarget eid -> go eid
  _ -> False
 where
  go eid = case windowType window of
    Window.TakeDamage _ _ (EnemyTarget eid') -> eid == eid'
    _ -> False

instance HasQueue env => RunMessage env TommyMalloy where
  runMessage msg e@(TommyMalloy attrs) = case msg of
    CheckWindow _ windows' | any (isTakeDamage attrs) windows' ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> TommyMalloy <$> runMessage msg attrs
