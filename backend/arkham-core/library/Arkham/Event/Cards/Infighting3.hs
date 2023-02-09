module Arkham.Event.Cards.Infighting3
  ( infighting3
  , Infighting3(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype Infighting3 = Infighting3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infighting3 :: EventCard Infighting3
infighting3 = event Infighting3 Cards.infighting3

-- TODO: We need to add a CancelledOrIgnoredCardOrGameEffect window here, but
-- knowing if this cancelled is tough and I don't think there is any way Diana
-- can actually interract with this card.
instance RunMessage Infighting3 where
  runMessage msg e@(Infighting3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      pushAll
        [ CreateWindowModifierEffect
          EffectPhaseWindow
          (EffectModifiers
          $ toModifiers attrs [CancelAttacksByEnemies NonEliteEnemy]
          )
          (toSource attrs)
          (InvestigatorTarget iid)
        , discard attrs
        ]
      pure e
    _ -> Infighting3 <$> runMessage msg attrs
