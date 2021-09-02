module Arkham.Types.Agenda.Cards.TheyreGettingOut where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Resolution
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype TheyreGettingOut = TheyreGettingOut AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreGettingOut :: AgendaCard TheyreGettingOut
theyreGettingOut =
  agenda (3, A) TheyreGettingOut Cards.theyreGettingOut (Static 10)

instance HasAbilities TheyreGettingOut where
  getAbilities (TheyreGettingOut x) =
    [ mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase
    , mkAbility x 2 $ ForcedAbility $ RoundEnds Timing.When
    ]

instance AgendaRunner env => RunMessage env TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs) = case msg of
    AdvanceAgenda aid
      | aid == toId attrs && agendaSequence attrs == Agenda 3 B -> do
        actIds <- getSet @ActId ()
        let
          resolution = if any ((`elem` actIds) . ActId) ["01108", "01109"]
            then Resolution 3
            else NoResolution
        a <$ push (ScenarioResolution resolution)
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      enemiesToMove <- selectList
        (UnengagedEnemy <> EnemyWithTrait Ghoul <> NotEnemy
          (EnemyAt $ LocationWithTitle "Parlor")
        )
      messages <- catMaybes <$> for
        enemiesToMove
        \eid -> do
          locationId <- getId eid
          closestLocationIds <- map unClosestPathLocationId
            <$> getSetList (locationId, LocationWithTitle "Parlor")
          case closestLocationIds of
            [] -> pure Nothing
            [x] -> pure $ Just $ EnemyMove eid locationId x
            xs -> pure $ Just $ chooseOne leadInvestigatorId $ map
              (EnemyMove eid locationId)
              xs
      a <$ unless
        (null messages)
        (push $ chooseOneAtATime leadInvestigatorId messages)
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      ghoulCount <- length <$> selectList
        (EnemyWithTrait Ghoul <> EnemyAt
          (LocationMatchAny $ map LocationWithTitle ["Parlor", "Hallway"])
        )
      a <$ pushAll (replicate ghoulCount PlaceDoomOnAgenda)
    _ -> TheyreGettingOut <$> runMessage msg attrs
