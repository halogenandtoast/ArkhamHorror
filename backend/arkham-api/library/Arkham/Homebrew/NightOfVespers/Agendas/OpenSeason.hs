module Arkham.Homebrew.NightOfVespers.Agendas.OpenSeason (openSeason) where

import Arkham.Agenda.Import.Lifted
import Arkham.Cost (Cost (GroupClueCost))
import Arkham.ForMovement
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Homebrew.NightOfVespers.CardDefs.Agendas qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Syndicate))

newtype OpenSeason = OpenSeason AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openSeason :: AgendaCard OpenSeason
openSeason = agenda (2, A) OpenSeason Cards.openSeason (Static 6)

instance HasModifiersFor OpenSeason where
  getModifiersFor (OpenSeason a) = do
    modified_ a GameTarget [EncounterCardDistancesAreBidirectional]
    modifySelect a UnrevealedLocation [AdditionalCostToEnter $ GroupClueCost (PerPlayer 1) Anywhere]

instance RunMessage OpenSeason where
  runMessage msg a@(OpenSeason attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator \iid -> do
        sid <- getRandom
        beginSkillTest sid iid attrs iid #agility (Fixed 3)
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      findEncounterCard iid attrs $ #enemy <> CardWithTrait Syndicate
      pure a
    FoundEncounterCard iid (isTarget attrs -> True) card -> do
      createEnemy_ card
        $ mapOneOf ($ locationWithInvestigator iid) [ConnectedTo NotForMovement, ConnectedFrom NotForMovement]
      pure a
    SearchNoneFound iid (isTarget attrs -> True) -> do
      withLocationOf iid \lid -> do
        nearest <-
          select $ NearestEnemyToFallback iid $ EnemyWithTrait Syndicate <> EnemyCanBeMovedBy (toSource attrs)
        chooseTargetM iid nearest \enemy -> do
          enemyMoveTo attrs enemy lid
          initiateEnemyAttack enemy attrs iid
      pure a
    _ -> OpenSeason <$> liftRunMessage msg attrs
