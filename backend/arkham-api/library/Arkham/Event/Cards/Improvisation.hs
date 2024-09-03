module Arkham.Event.Cards.Improvisation (improvisation, improvisationEffect, Improvisation (..)) where

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype Improvisation = Improvisation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisation :: EventCard Improvisation
improvisation = event Improvisation Cards.improvisation

switchRole :: PlayerId -> InvestigatorId -> Message
switchRole pid iid = chooseOne pid [Label (tshow role) [SetRole iid role] | role <- [minBound .. maxBound]]

instance RunMessage Improvisation where
  runMessage msg e@(Improvisation attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      let drawing = drawCards iid attrs 1
      player <- getPlayer iid
      pushAll
        [ switchRole player iid
        , createCardEffect Cards.improvisation Nothing attrs iid
        , drawing
        ]
      pure e
    _ -> Improvisation <$> runMessage msg attrs

newtype ImprovisationEffect = ImprovisationEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisationEffect :: EffectArgs -> ImprovisationEffect
improvisationEffect = cardEffect ImprovisationEffect Cards.improvisation

instance HasModifiersFor ImprovisationEffect where
  getModifiersFor target@(InvestigatorTarget iid) (ImprovisationEffect attrs) | attrs.target == target = do
    role <- field InvestigatorClass iid
    pure $ toModifiers attrs [ReduceCostOf (CardWithClass role) 3]
  getModifiersFor _ _ = pure []

instance RunMessage ImprovisationEffect where
  runMessage msg e@(ImprovisationEffect attrs) = case msg of
    CardEnteredPlay iid card | effectTarget attrs == InvestigatorTarget iid -> do
      role <- field InvestigatorClass iid
      pushWhen
        (maybe False (== role) . headMay . toList $ cdClassSymbols (toCardDef card))
        (disable attrs)
      pure e
    EndTurn iid | attrs.target == InvestigatorTarget iid -> do
      push $ disable attrs
      pure e
    _ -> ImprovisationEffect <$> runMessage msg attrs
