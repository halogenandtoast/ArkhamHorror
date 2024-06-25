module Arkham.Event.Cards.BurnAfterReading1 (burnAfterReading1, BurnAfterReading1 (..)) where

import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.Helpers.Investigator (getCanDiscoverClues, withLocationOf)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Projection

newtype BurnAfterReading1 = BurnAfterReading1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burnAfterReading1 :: EventCard BurnAfterReading1
burnAfterReading1 = event BurnAfterReading1 Cards.burnAfterReading1

instance RunMessage BurnAfterReading1 where
  runMessage msg e@(BurnAfterReading1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      canExile <- select $ inHandOf iid <> basic (CardWithMaxLevel 5)
      unless (null canExile)
        $ chooseOne iid [targetLabel x [handleTargetChoice iid attrs x.id] | x <- canExile]

      withLocationOf iid \lid -> do
        canDiscoverClues <- getCanDiscoverClues NotInvestigate iid lid
        hasClues <- fieldSome LocationClues lid
        pushWhen (canDiscoverClues && hasClues)
          $ Msg.DiscoverClues iid
          $ discover lid attrs 2

      push $ DoStep 2 msg
      push $ Exile (toTarget attrs)
      pure e
    DoStep 2 (PlayThisEvent iid eid) | eid == toId attrs -> do
      for_ (getEventMeta attrs) \case
        True -> do
          agendas <- select AnyAgenda
          chooseOrRunOne iid [targetLabel a [RemoveDoom (toSource attrs) (toTarget a) 1] | a <- agendas]
        False -> pure ()
      pure e
    HandleTargetChoice _ (isSource attrs -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      push $ Exile (CardIdTarget card.id)
      pure $ BurnAfterReading1 $ attrs & setMeta (maybe False (<= 5) card.level)
    _ -> BurnAfterReading1 <$> lift (runMessage msg attrs)
