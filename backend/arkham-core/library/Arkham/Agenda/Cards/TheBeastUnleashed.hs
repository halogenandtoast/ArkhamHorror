module Arkham.Agenda.Cards.TheBeastUnleashed
  ( TheBeastUnleashed(..)
  , theBeastUnleashed
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Attrs
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message
import Arkham.Resolution
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype TheBeastUnleashed = TheBeastUnleashed AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastUnleashed :: AgendaCard TheBeastUnleashed
theBeastUnleashed =
  agenda (3, A) TheBeastUnleashed Cards.theBeastUnleashed (Static 2)

instance HasAbilities TheBeastUnleashed where
  getAbilities (TheBeastUnleashed x) =
    [ mkAbility x 1
      $ ForcedAbility
      $ AgendaWouldAdvance Timing.When DoomThreshold
      $ AgendaWithId
      $ toId x
    , mkAbility x 2 $ Objective $ ForcedAbility $ EnemyEnters
      Timing.After
      (locationIs Cards.dormitories)
      (enemyIs Cards.theExperiment)
    ]

getTheExperiment :: (Monad m, HasGame m) => m EnemyId
getTheExperiment =
  fromJustNote "must be in play" <$> selectOne (enemyIs Cards.theExperiment)

instance RunMessage TheBeastUnleashed where
  runMessage msg a@(TheBeastUnleashed attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      experimentId <- getTheExperiment
      a <$ pushAll
        [ RemoveAllDoom (toSource attrs)
        , MoveToward
          (EnemyTarget experimentId)
          (LocationWithTitle "Dormitories")
        ]
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAgenda $ toId attrs)
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- getInvestigatorIds
      a <$ pushAll
        ([ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 3
         | iid <- investigatorIds
         ]
        <> [ chooseOne
               leadInvestigatorId
               [Label "Resolution 4" [ScenarioResolution $ Resolution 4]]
           ]
        )
    _ -> TheBeastUnleashed <$> runMessage msg attrs
