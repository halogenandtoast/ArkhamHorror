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
import Arkham.Criteria
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
    , restrictedAbility
        x
        2
        (EnemyCriteria
        $ EnemyExists
        $ UnengagedEnemy
        <> EnemyWithTrait Ghoul
        <> NotEnemy (EnemyAt $ LocationWithTitle "Parlor")
        )
      $ ForcedAbility
      $ RoundEnds Timing.When
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
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      lead <- getLead
      enemiesToMove <-
        selectList $ UnengagedEnemy <> EnemyWithTrait Ghoul <> NotEnemy
          (EnemyAt $ LocationWithTitle "Parlor")

      unless (null enemiesToMove) $ push $ chooseOneAtATime
        lead
        [ targetLabel
            enemy
            [MoveToward (toTarget enemy) (LocationWithTitle "Parlor")]
        | enemy <- enemiesToMove
        ]
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      ghoulCount <- selectCount $ EnemyWithTrait Ghoul <> EnemyAt
        (LocationMatchAny $ map LocationWithTitle ["Parlor", "Hallway"])
      pushAll $ replicate ghoulCount PlaceDoomOnAgenda
      pure a
    _ -> TheyreGettingOut <$> runMessage msg attrs
