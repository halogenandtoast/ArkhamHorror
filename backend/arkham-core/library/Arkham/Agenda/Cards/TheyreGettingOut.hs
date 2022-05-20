module Arkham.Agenda.Cards.TheyreGettingOut where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Attrs (Field(..))
import Arkham.Agenda.Attrs
import qualified Arkham.Agenda.Cards as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Resolution
import qualified Arkham.Timing as Timing
import Arkham.Trait

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
      | aid == toId attrs && onSide B attrs -> do
        actSequence <- field ActSequenceNumber =<< selectJust AnyAct
        let
          resolution = if actSequence `elem` [1, 2]
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
          mLocationId <- selectOne $ LocationWithEnemy $ EnemyWithId eid
          case mLocationId of
            Nothing -> pure Nothing
            Just loc -> do
              closestLocationIds <- map unClosestPathLocationId
                <$> getSetList (loc, LocationWithTitle "Parlor")
              case closestLocationIds of
                [] -> pure Nothing
                [x] -> pure $ Just $ EnemyMove eid x
                xs -> pure $ Just $ chooseOne leadInvestigatorId $ map
                  (EnemyMove eid)
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
