module Arkham.Effect.Effects.Improvisation (
  Improvisation (..),
  improvisation,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection

newtype Improvisation = Improvisation EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisation :: EffectArgs -> Improvisation
improvisation = Improvisation . uncurry4 (baseAttrs "03018")

instance HasModifiersFor Improvisation where
  getModifiersFor target@(InvestigatorTarget iid) (Improvisation attrs)
    | effectTarget attrs == target = do
        role <- field InvestigatorClass iid
        pure $ toModifiers attrs [ReduceCostOf (CardWithClass role) 3]
  getModifiersFor _ _ = pure []

instance RunMessage Improvisation where
  runMessage msg e@(Improvisation attrs) = case msg of
    CardEnteredPlay iid card | effectTarget attrs == InvestigatorTarget iid -> do
      role <- field InvestigatorClass iid
      e
        <$ when
          ( maybe False (== role)
              . headMay
              . toList
              $ cdClassSymbols
                (toCardDef card)
          )
          (push $ DisableEffect $ toId attrs)
    EndTurn iid
      | effectTarget attrs == InvestigatorTarget iid ->
          e <$ push (DisableEffect $ toId attrs)
    _ -> Improvisation <$> runMessage msg attrs
