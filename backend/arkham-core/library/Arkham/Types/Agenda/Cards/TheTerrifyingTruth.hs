module Arkham.Types.Agenda.Cards.TheTerrifyingTruth
  ( TheTerrifyingTruth
  , theTerrifyingTruth
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.AdvancementReason
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution
import qualified Arkham.Types.Timing as Timing

newtype TheTerrifyingTruth = TheTerrifyingTruth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
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

instance AgendaRunner env => RunMessage env TheTerrifyingTruth where
  runMessage msg a@(TheTerrifyingTruth attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      storyCards <- selectList
        (UnderScenarioReferenceMatch $ CardWithType StoryType)
      result <- case storyCards of
        [] -> pure $ AdvanceAgenda (toId attrs)
        (x : xs) -> do
          card <- sample $ x :| xs
          pure $ ReadStory card
      a <$ pushAll [RemoveAllDoom, result]
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ push (ScenarioResolution $ Resolution 3)
    _ -> TheTerrifyingTruth <$> runMessage msg attrs
