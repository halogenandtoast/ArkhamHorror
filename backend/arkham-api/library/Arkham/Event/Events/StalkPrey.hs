module Arkham.Event.Events.StalkPrey (stalkPrey) where

import Arkham.Card
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Strategy
import Arkham.Zone

newtype StalkPrey = StalkPrey EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stalkPrey :: EventCard StalkPrey
stalkPrey = event StalkPrey Cards.stalkPrey

instance RunMessage StalkPrey where
  runMessage msg e@(StalkPrey attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      search iid attrs EncounterDeckTarget [(FromTopOfDeck 9, ShuffleBackIn)] #any (defer attrs IsDraw)
      pure e
    SearchNoneFound iid (isTarget attrs -> True) -> do
      continue_ iid
      drawCards iid attrs 1
      discoverAtYourLocation NotInvestigate iid attrs 1
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      let enemyCards = filter (`cardMatch` EnemyType) $ onlyEncounterCards cards
      case enemyCards of
        [] -> do
          continue_ iid
          drawCards iid attrs 1
          discoverAtYourLocation NotInvestigate iid attrs 1
        _ -> chooseOneM iid do
          targets enemyCards \card -> do
            push $ InvestigatorDrewEncounterCard iid card
            drawCards iid attrs 1
            discoverAtYourLocation NotInvestigate iid attrs 1
            doStep 1 (InvestigatorDrewEncounterCard iid card)
      pure e
    DoStep 1 (InvestigatorDrewEncounterCard iid card) -> do
      menemy <- selectOne $ EnemyWithCardId card.id
      for_ menemy \eid -> do
        unlessM (eid <=~> EnemyAt (locationWithInvestigator iid)) do
          chooseOneM iid do
            cardI18n $ scope "stalkPrey" $ labeled "moveToward" $ moveToward iid (locationWithEnemy eid)
            labeledI "doNotMove" nothing
      pure e
    _ -> StalkPrey <$> liftRunMessage msg attrs
