module Arkham.Agenda.Cards.ChildrenOfBlood.BloodMoney.PartyWithoutAHost (partyWithoutAHost) where

import Arkham.Agenda.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getLead, getPlayerCount, getSetAsideCardsMatching)
import Arkham.Difficulty
import Arkham.Helpers.Scenario (getDifficulty)
import Arkham.I18n
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ally, Civilian, Cultist))

newtype PartyWithoutAHost = PartyWithoutAHost AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

partyWithoutAHost :: AgendaCard PartyWithoutAHost
partyWithoutAHost = agenda (1, A) PartyWithoutAHost Cards.partyWithoutAHost (Static 5)

instance HasModifiersFor PartyWithoutAHost where
  getModifiersFor (PartyWithoutAHost a) = do
    cards <- findAllCards (`cardMatch` mapOneOf CardWithTrait [Ally, Civilian, Cultist])
    modifyEach a cards [GainVictory 0]

instance RunMessage PartyWithoutAHost where
  runMessage msg a@(PartyWithoutAHost attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      diningHall <- selectJust (locationIs Locations.diningHall)
      getSetAsideCardsMatching (#enemy <> CardWithTitle "Julia Stern") >>= \case
        (julia : _) -> createEnemyAt_ julia diningHall
        [] ->
          getSetAsideCardsMatching (cardIs Enemies.spawnOfZburamoarte) >>= \case
            (spawn : _) -> createEnemyAt_ spawn diningHall
            [] -> pure ()

      hardExpert <- (`elem` [Hard, Expert]) <$> getDifficulty
      replicateM_ (if hardExpert then 2 else 1) $ doStep 1 msg

      perPlayer <- getPlayerCount
      replicateM_ perPlayer
        $ findEncounterCard lead (ProxyTarget (toTarget attrs) (toTarget attrs)) civilianEnemy

      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      lead <- getLead
      inPlay <- select $ EnemyWithTrait Civilian
      chooseOneM lead $ withI18n do
        targets inPlay $ addToVictory lead
        labeled "searchEncounterDeckAndDiscard" $ findEncounterCard lead attrs civilianEnemy
      pure a
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      addToVictory iid card
      pure a
    FoundEncounterCard iid (ProxyTarget (isTarget attrs -> True) _) (toCard -> card) -> do
      empties <- select $ not_ (LocationWithInvestigator Anyone)
      ls <- if null empties then select Anywhere else pure empties
      chooseTargetM iid ls $ createEnemyAt_ card
      pure a
    _ -> PartyWithoutAHost <$> liftRunMessage msg attrs

-- | "1 {Civilian} enemy" -- the searches look at cards, not entities in play.
civilianEnemy :: CardMatcher
civilianEnemy = #enemy <> CardWithTrait Civilian
