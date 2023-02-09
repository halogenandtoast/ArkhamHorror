module Arkham.Effect.Effects.CallingInFavors
  ( CallingInFavors(..)
  , callingInFavors
  ) where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Source
import Arkham.Target
import Arkham.Trait

newtype CallingInFavors = CallingInFavors EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callingInFavors :: EffectArgs -> CallingInFavors
callingInFavors = CallingInFavors . uncurry4 (baseAttrs "03158")

instance HasModifiersFor CallingInFavors where
  getModifiersFor (InvestigatorTarget iid) (CallingInFavors attrs)
    | (InvestigatorSource iid) == effectSource attrs = do
      case effectMetadata attrs of
        Just (EffectInt n) -> pure $ toModifiers
          attrs
          [ReduceCostOf (CardWithType AssetType <> CardWithTrait Ally) n]
        _ -> error "Invalid metadata"
  getModifiersFor _ _ = pure []


instance RunMessage CallingInFavors where
  runMessage msg e@(CallingInFavors attrs) = case msg of
    Discard _ (EventTarget eid) | EventSource eid == effectSource attrs ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> CallingInFavors <$> runMessage msg attrs
