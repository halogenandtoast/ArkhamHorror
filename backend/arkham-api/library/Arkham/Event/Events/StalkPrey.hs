module Arkham.Event.Events.StalkPrey (stalkPrey) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Zone

newtype StalkPrey = StalkPrey EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkPrey :: EventCard StalkPrey
stalkPrey = event StalkPrey Cards.stalkPrey

instance RunMessage StalkPrey where
  runMessage msg e@(StalkPrey attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      findEncounterCardIn iid attrs (cardMatcher_ #enemy) [FromEncounterDeck]
      pure e
    FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card) -> do
      drawCard iid card
      drawCards iid attrs 1
      discoverAtYourLocation NotInvestigate iid attrs 1
      doStep 1 msg
      pure e
    DoStep 1 (FoundEncounterCard iid (isTarget attrs -> True) (toCard -> card)) -> do
      menemy <- selectOne $ EnemyWithCardId card.id
      for_ menemy \eid -> do
        unlessM (eid <=~> EnemyAt (locationWithInvestigator iid)) do
          chooseOneM iid do
            labeled "Move once toward its location" $ moveToward iid (locationWithEnemy eid)
            labeled "Do not move" nothing
      pure e
    _ -> StalkPrey <$> liftRunMessage msg attrs
