module Arkham.Agenda.Cards.TheTerrifyingTruth (
  TheTerrifyingTruth (..),
  theTerrifyingTruth,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype TheTerrifyingTruth = TheTerrifyingTruth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

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
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      storyCards <-
        selectList
          (UnderScenarioReferenceMatch $ CardWithType StoryType)
      result <- case storyCards of
        [] -> pure $ AdvanceAgenda (toId attrs)
        (x : xs) -> do
          card <- sample $ x :| xs
          pure $ ReadStory iid card ResolveIt Nothing
      a <$ pushAll [RemoveAllDoomFromPlay defaultRemoveDoomMatchers, result]
    AdvanceAgenda aid
      | aid == toId attrs && onSide B attrs ->
          a <$ push (ScenarioResolution $ Resolution 3)
    _ -> TheTerrifyingTruth <$> runMessage msg attrs
