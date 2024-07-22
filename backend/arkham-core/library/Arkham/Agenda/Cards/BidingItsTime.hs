module Arkham.Agenda.Cards.BidingItsTime (
  BidingItsTime (..),
  bidingItsTime,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Attack
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Creation
import Arkham.Exception
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (ChosenRandomLocation)
import Arkham.Projection
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype BidingItsTime = BidingItsTime AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bidingItsTime :: AgendaCard BidingItsTime
bidingItsTime = agenda (2, A) BidingItsTime Cards.bidingItsTime (Static 6)

instance HasAbilities BidingItsTime where
  getAbilities (BidingItsTime x) = [mkAbility x 1 $ ForcedAbility $ PhaseEnds #when #enemy]

instance RunMessage BidingItsTime where
  runMessage msg a@(BidingItsTime attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLeadPlayer
      broodOfYogSothoth <- selectTargets $ EnemyWithTitle "Brood of Yog-Sothoth"
      pushWhen (notNull broodOfYogSothoth)
        $ chooseOneAtATime lead
        $ targetLabels broodOfYogSothoth (\target -> only $ ChooseRandomLocation target mempty)
      pure a
    ChosenRandomLocation target@(EnemyTarget _) lid -> do
      push $ MoveToward target (LocationWithId lid)
      pure a
    AdvanceAgenda aid | aid == agendaId attrs && onSide B attrs -> do
      anyBrood <- (> 0) . length <$> getSetAsideBroodOfYogSothoth
      pushAll
        $ ShuffleEncounterDiscardBackIn
        : [RequestSetAsideCard (toSource attrs) (CardCode "02255") | anyBrood]
          <> [advanceAgendaDeck attrs]
      pure a
    RequestedSetAsideCard source card | isSource attrs source -> do
      when (toCardCode card /= CardCode "02255") (throwIO $ InvalidState "wrong card")
      lead <- getLead
      location <- fieldJust InvestigatorLocation lead
      investigators <- select $ colocatedWith lead
      enemyCreation <- createEnemy card location
      sid <- getRandom
      pushAll
        $ toMessage enemyCreation
        : [ beginSkillTest sid iid source (enemyCreationEnemyId enemyCreation) #agility (Fixed 4)
          | iid <- investigators
          ]
      pure a
    FailedSkillTest iid _ (isSource attrs -> True) (Initiator (EnemyTarget enemy)) _ _ -> do
      push $ enemyAttack enemy attrs iid
      pure a
    _ -> BidingItsTime <$> runMessage msg attrs
