module Arkham.Agenda.Cards.TheBeastUnleashed (TheBeastUnleashed (..), theBeastUnleashed) where

import Arkham.Ability
import Arkham.Agenda.AdvancementReason
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Query (getInvestigators, getLead)
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher

newtype TheBeastUnleashed = TheBeastUnleashed AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastUnleashed :: AgendaCard TheBeastUnleashed
theBeastUnleashed = agenda (3, A) TheBeastUnleashed Cards.theBeastUnleashed (Static 2)

instance HasAbilities TheBeastUnleashed where
  getAbilities (TheBeastUnleashed x) =
    [ mkAbility x 1 $ forced $ AgendaWouldAdvance #when DoomThreshold $ AgendaWithId $ toId x
    , mkAbility x 2
        $ Objective
        $ forced
        $ EnemyEnters #after (locationIs Cards.dormitories) (enemyIs Cards.theExperiment)
    ]

getTheExperiment :: HasGame m => m EnemyId
getTheExperiment = fromJustNote "must be in play" <$> selectOne (enemyIs Cards.theExperiment)

instance RunMessage TheBeastUnleashed where
  runMessage msg a@(TheBeastUnleashed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      experimentId <- getTheExperiment
      pushAll
        [ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
        , MoveToward (toTarget experimentId) "Dormitories"
        ]
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAgenda $ toId attrs)
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      investigators <- getInvestigators
      lead <- getLead
      for_ investigators \investigator -> assignHorror investigator attrs 3
      chooseOne lead [Label "Resolution 4" [R4]]
      pure a
    _ -> TheBeastUnleashed <$> lift (runMessage msg attrs)
