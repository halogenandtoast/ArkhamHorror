module Arkham.Agenda.Cards.TheBeastUnleashed (theBeastUnleashed) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype TheBeastUnleashed = TheBeastUnleashed AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastUnleashed :: AgendaCard TheBeastUnleashed
theBeastUnleashed = agenda (3, A) TheBeastUnleashed Cards.theBeastUnleashed (Static 2)

instance HasAbilities TheBeastUnleashed where
  getAbilities (TheBeastUnleashed x) =
    [ mkAbility x 1 $ forced $ AgendaWouldAdvance #when #doom (be x)
    , mkAbility x 2
        $ Objective
        $ forced
        $ EnemyEnters #after (locationIs Cards.dormitories) (enemyIs Cards.theExperiment)
    ]

getTheExperiment :: (HasCallStack, HasGame m) => m EnemyId
getTheExperiment = selectJust (enemyIs Cards.theExperiment)

instance RunMessage TheBeastUnleashed where
  runMessage msg a@(TheBeastUnleashed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      theExperiment <- getTheExperiment
      moveToward theExperiment $ location_ "Dormitories"
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \investigator -> assignHorror investigator attrs 3
      push R4
      pure a
    _ -> TheBeastUnleashed <$> liftRunMessage msg attrs
