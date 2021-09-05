module Arkham.Types.Effect.Effects.Improvisation
  ( Improvisation(..)
  , improvisation
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype Improvisation = Improvisation EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisation :: EffectArgs -> Improvisation
improvisation = Improvisation . uncurry4 (baseAttrs "03018")

instance HasSet ClassSymbol env InvestigatorId => HasModifiersFor env Improvisation where
  getModifiersFor _ target@(InvestigatorTarget iid) (Improvisation attrs)
    | effectTarget attrs == target = do
      roles <- getSetList iid
      case roles of
        [] -> pure []
        role : _ ->
          pure $ toModifiers attrs [ReduceCostOf (CardWithClass role) 3]
  getModifiersFor _ _ _ = pure []

instance (HasSet ClassSymbol env InvestigatorId, HasQueue env) => RunMessage env Improvisation where
  runMessage msg e@(Improvisation attrs) = case msg of
    PlayedCard iid card | effectTarget attrs == InvestigatorTarget iid -> do
      roles <- getSetList iid
      e <$ when
        (maybe False (`elem` roles) $ cdClassSymbol (toCardDef card))
        (push $ DisableEffect $ toId attrs)
    EndTurn iid | effectTarget attrs == InvestigatorTarget iid ->
      e <$ push (DisableEffect $ toId attrs)
    _ -> Improvisation <$> runMessage msg attrs
