module Arkham.Agenda.Cards.BidingItsTime (bidingItsTime) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype BidingItsTime = BidingItsTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bidingItsTime :: AgendaCard BidingItsTime
bidingItsTime = agenda (2, A) BidingItsTime Cards.bidingItsTime (Static 6)

instance HasAbilities BidingItsTime where
  getAbilities (BidingItsTime x) =
    [ restricted x 1 (exists $ InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth")
        $ forced
        $ PhaseEnds #when #enemy
    ]

instance RunMessage BidingItsTime where
  runMessage msg a@(BidingItsTime attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      broodOfYogSothoth <- selectTargets $ InPlayEnemy $ EnemyWithTitle "Brood of Yog-Sothoth"
      leadChooseOneAtATimeM $ targets broodOfYogSothoth \x -> push $ ChooseRandomLocation x mempty
      pure a
    ChosenRandomLocation target@(EnemyTarget _) lid -> do
      moveToward target lid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      lead <- getLead
      withLocationOf lead \lid -> do
        brood <- getSetAsideBroodOfYogSothoth
        for_ (nonEmpty brood) \xs -> do
          x <- sample xs
          enemy <- createEnemyAt x lid
          selectEach (colocatedWith lead) \iid -> do
            sid <- getRandom
            beginSkillTest sid iid attrs enemy #agility (Fixed 4)
      advanceAgendaDeck attrs
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) (Initiator (EnemyTarget enemy)) _ _ -> do
      initiateEnemyAttack enemy attrs iid
      pure a
    _ -> BidingItsTime <$> liftRunMessage msg attrs
