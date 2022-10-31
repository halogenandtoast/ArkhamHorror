module Arkham.Agenda.Cards.TheyreGettingOut
  ( TheyreGettingOut(..)
  , theyreGettingOut
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Sequence qualified as AS
import Arkham.Act.Types ( Field (..) )
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Projection
import Arkham.Resolution
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype TheyreGettingOut = TheyreGettingOut AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theyreGettingOut :: AgendaCard TheyreGettingOut
theyreGettingOut =
  agenda (3, A) TheyreGettingOut Cards.theyreGettingOut (Static 10)

instance HasAbilities TheyreGettingOut where
  getAbilities (TheyreGettingOut x) =
    [ mkAbility x 1 $ ForcedAbility $ PhaseEnds Timing.When $ PhaseIs EnemyPhase
    , mkAbility x 2 $ ForcedAbility $ RoundEnds Timing.When
    ]

instance RunMessage TheyreGettingOut where
  runMessage msg a@(TheyreGettingOut attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      actSequence <- fieldMap ActSequence (AS.unActStep . AS.actStep)
        =<< selectJust AnyAct
      let
        resolution =
          if actSequence `elem` [1, 2] then Resolution 3 else NoResolution
      a <$ push (ScenarioResolution resolution)
    UseCardAbility _ (isAbility attrs 1 -> True) _ _ -> do
      leadInvestigatorId <- getLeadInvestigatorId
      enemiesToMove <- selectList
        (UnengagedEnemy <> EnemyWithTrait Ghoul <> NotEnemy
          (EnemyAt $ LocationWithTitle "Parlor")
        )
      messages <- catMaybes <$> for
        enemiesToMove
        \eid -> do
          mLocationId <- selectOne $ LocationWithEnemy $ EnemyWithId eid
          parlorId <- selectJust $ LocationWithTitle "Parlor"
          case mLocationId of
            Nothing -> pure Nothing
            Just loc -> do
              closestLocationIds <- selectList
                $ ClosestPathLocation loc parlorId
              case closestLocationIds of
                [] -> pure Nothing
                [x] -> pure $ Just $ targetLabel eid [EnemyMove eid x]
                xs -> pure $ Just $ targetLabel
                  eid
                  [ chooseOne
                      leadInvestigatorId
                      [ targetLabel x [EnemyMove eid x] | x <- xs ]
                  ]
      unless (null messages) $ push $ chooseOneAtATime
        leadInvestigatorId
        messages
      pure a
    UseCardAbility _ (isAbility attrs 2 -> True) _ _ -> do
      ghoulCount <- length <$> selectList
        (EnemyWithTrait Ghoul <> EnemyAt
          (LocationMatchAny $ map LocationWithTitle ["Parlor", "Hallway"])
        )
      a <$ pushAll (replicate ghoulCount PlaceDoomOnAgenda)
    _ -> TheyreGettingOut <$> runMessage msg attrs
