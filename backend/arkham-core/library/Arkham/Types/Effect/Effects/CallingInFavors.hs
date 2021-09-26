module Arkham.Types.Effect.Effects.CallingInFavors
  ( CallingInFavors(..)
  , callingInFavors
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait

newtype CallingInFavors = CallingInFavors EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingInFavors :: EffectArgs -> CallingInFavors
callingInFavors = CallingInFavors . uncurry4 (baseAttrs "03158")

instance HasModifiersFor env CallingInFavors where
  getModifiersFor source (CardIdTarget _) (CallingInFavors attrs)
    | source == effectSource attrs = do
      case effectMetadata attrs of
        Just (EffectInt n) -> pure $ toModifiers
          attrs
          [ReduceCostOf (CardWithType AssetType <> CardWithTrait Ally) n]
        _ -> error "Invalid metadata"
  getModifiersFor _ _ _ = pure []


instance HasQueue env => RunMessage env CallingInFavors where
  runMessage msg e@(CallingInFavors attrs) = case msg of
    Discard (EventTarget eid) | EventSource eid == effectSource attrs ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> CallingInFavors <$> runMessage msg attrs
