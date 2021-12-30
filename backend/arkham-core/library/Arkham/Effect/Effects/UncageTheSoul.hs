module Arkham.Effect.Effects.UncageTheSoul
  ( UncageTheSoul(..)
  , uncageTheSoul
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Target

newtype UncageTheSoul = UncageTheSoul EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncageTheSoul :: EffectArgs -> UncageTheSoul
uncageTheSoul = UncageTheSoul . uncurry4 (baseAttrs "03033")

instance HasModifiersFor env UncageTheSoul where
  getModifiersFor _ target@(CardIdTarget cid) (UncageTheSoul attrs)
    | effectTarget attrs == target = pure
    $ toModifiers attrs [ReduceCostOf (CardWithId cid) 3]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env UncageTheSoul where
  runMessage msg e@(UncageTheSoul attrs) = case msg of
    ResolvedCard _ card | CardIdTarget (toCardId card) == effectTarget attrs ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> UncageTheSoul <$> runMessage msg attrs
