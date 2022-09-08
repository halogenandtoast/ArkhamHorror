module Arkham.Event.Cards.Marksmanship2
  ( marksmanship2
  , Marksmanship2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype Marksmanship2 = Marksmanship2 EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship2 :: EventCard Marksmanship2
marksmanship2 = event Marksmanship2 Cards.marksmanship2

instance HasModifiersFor Marksmanship2 where
  getModifiersFor (AbilityTarget iid ability) (Marksmanship2 a)
    | eventOwner a == iid = do
      case abilityAction ability of
        Just Action.Fight -> do
          traits <- sourceTraits (abilitySource ability)
          if any (`elem` traits) [Firearm, Ranged]
            then pure $ toModifiers
              a
              [ ActionAbilityOverride Action.Fight
                $ CriteriaOverride OnSameLocation
                $ AnyCriterion
                    [OnSameLocation, OnLocation $ ConnectedFrom LocationOfThis]
              ]
            else pure []
        _ -> pure []
  getModifiersFor _ _ = pure []


instance RunMessage Marksmanship2 where
  runMessage msg e@(Marksmanship2 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> Marksmanship2 <$> runMessage msg attrs
