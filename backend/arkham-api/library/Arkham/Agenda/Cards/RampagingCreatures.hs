module Arkham.Agenda.Cards.RampagingCreatures (rampagingCreatures) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype RampagingCreatures = RampagingCreatures AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rampagingCreatures :: AgendaCard RampagingCreatures
rampagingCreatures = agenda (1, A) RampagingCreatures Cards.rampagingCreatures (Static 5)

instance HasAbilities RampagingCreatures where
  getAbilities (RampagingCreatures x) =
    [ restricted x 1 (exists $ InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth")
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance RunMessage RampagingCreatures where
  runMessage msg a@(RampagingCreatures attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      broodOfYogSothoth <- select $ InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth"
      leadChooseOneAtATimeM $ targets broodOfYogSothoth \x -> push $ ChooseRandomLocation (toTarget x) mempty
      pure a
    ChosenRandomLocation target@(EnemyTarget _) lid | onSide A attrs -> do
      moveToward target lid
      pure a
    ChosenRandomLocation target lid | isTarget attrs target && onSide B attrs -> do
      setAsideBroodOfYogSothoth <- shuffleM =<< getSetAsideBroodOfYogSothoth
      for_ (nonEmpty setAsideBroodOfYogSothoth) \(x :| _) -> createEnemyAt_ x lid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      push $ ChooseRandomLocation (toTarget attrs) mempty
      advanceAgendaDeck attrs
      pure a
    _ -> RampagingCreatures <$> liftRunMessage msg attrs
