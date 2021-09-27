module Arkham.Types.Agenda.Cards.TheBeastUnleashed where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Agenda.AdvancementReason
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.EnemyId
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype TheBeastUnleashed = TheBeastUnleashed AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env)
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

getTheExperiment :: (MonadReader env m, Query EnemyMatcher env) => m EnemyId
getTheExperiment =
  fromJustNote "must be in play" <$> selectOne (enemyIs Cards.theExperiment)

instance AgendaRunner env => RunMessage env TheBeastUnleashed where
  runMessage msg a@(TheBeastUnleashed attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      experimentId <- getTheExperiment
      a <$ pushAll
        [ RemoveAllDoom
        , MoveToward
          (EnemyTarget experimentId)
          (LocationWithTitle "Dormitories")
        ]
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAgenda $ toId attrs)
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigatorIds <- getInvestigatorIds
      a <$ pushAll
        ([ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 3
         | iid <- investigatorIds
         ]
        <> [Label "Resolution 4" [ScenarioResolution $ Resolution 4]]
        )
    _ -> TheBeastUnleashed <$> runMessage msg attrs
