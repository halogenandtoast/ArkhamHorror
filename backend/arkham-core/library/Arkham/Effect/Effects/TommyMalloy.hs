module Arkham.Effect.Effects.TommyMalloy
  ( TommyMalloy(..)
  , tommyMalloy
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype TommyMalloy = TommyMalloy EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tommyMalloy :: EffectArgs -> TommyMalloy
tommyMalloy = TommyMalloy . uncurry4 (baseAttrs "60103")

instance HasModifiersFor TommyMalloy where
  getModifiersFor target (TommyMalloy attrs) | effectTarget attrs == target =
    pure $ toModifiers attrs [MaxDamageTaken 1]
  getModifiersFor _ _ = pure []

isTakeDamage :: EffectAttrs -> Window -> Bool
isTakeDamage attrs window = case effectTarget attrs of
  EnemyTarget eid -> go eid
  _ -> False
 where
  go eid = case windowType window of
    Window.TakeDamage _ _ (EnemyTarget eid') ->
      eid == eid' && windowTiming window == Timing.After
    _ -> False

instance RunMessage TommyMalloy where
  runMessage msg e@(TommyMalloy attrs) = case msg of
    CheckWindow _ windows' | any (isTakeDamage attrs) windows' ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> TommyMalloy <$> runMessage msg attrs
