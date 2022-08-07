module Arkham.Agenda.Cards.TheTerrifyingTruth
  ( TheTerrifyingTruth(..)
  , theTerrifyingTruth
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Types
import Arkham.Agenda.Runner
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype TheTerrifyingTruth = TheTerrifyingTruth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTerrifyingTruth :: AgendaCard TheTerrifyingTruth
theTerrifyingTruth =
  agenda (2, A) TheTerrifyingTruth Cards.theTerrifyingTruth (Static 3)

instance HasAbilities TheTerrifyingTruth where
  getAbilities (TheTerrifyingTruth a) =
    [ mkAbility a 1
        $ ForcedAbility
        $ AgendaWouldAdvance Timing.When DoomThreshold
        $ AgendaWithId
        $ toId a
    | onSide A a
    ]

instance RunMessage TheTerrifyingTruth where
  runMessage msg a@(TheTerrifyingTruth attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      storyCards <- selectList
        (UnderScenarioReferenceMatch $ CardWithType StoryType)
      result <- case storyCards of
        [] -> pure $ AdvanceAgenda (toId attrs)
        (x : xs) -> do
          card <- sample $ x :| xs
          pure $ ReadStory iid $ toCardDef card
      a <$ pushAll [RemoveAllDoom (toSource attrs), result]
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 3)
    _ -> TheTerrifyingTruth <$> runMessage msg attrs
