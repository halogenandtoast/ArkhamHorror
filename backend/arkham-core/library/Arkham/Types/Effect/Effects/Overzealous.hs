module Arkham.Types.Effect.Effects.Overzealous
  ( Overzealous(..)
  , overzealous
  ) where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Overzealous = Overzealous EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overzealous :: EffectArgs -> Overzealous
overzealous = Overzealous . uncurry4 (baseAttrs "03040")

instance HasModifiersFor env Overzealous where
  getModifiersFor _ target (Overzealous a) | effectTarget a == target =
    pure $ toModifiers a [AddKeyword Keyword.Surge]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Overzealous where
  runMessage msg e@(Overzealous attrs) = case msg of
    Discard (TreacheryTarget tid)
      | effectTarget attrs == CardIdTarget (unTreacheryId tid) -> e
      <$ push (DisableEffect $ effectId attrs)
    _ -> Overzealous <$> runMessage msg attrs
