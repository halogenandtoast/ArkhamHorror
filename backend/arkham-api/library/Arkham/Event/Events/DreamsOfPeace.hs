module Arkham.Event.Events.DreamsOfPeace (dreamsOfPeace) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.I18n
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Types (Field (..))

newtype DreamsOfPeace = DreamsOfPeace EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dreamsOfPeace :: EventCard DreamsOfPeace
dreamsOfPeace = event DreamsOfPeace Cards.dreamsOfPeace

instance RunMessage DreamsOfPeace where
  runMessage msg e@(DreamsOfPeace attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == attrs.id -> do
      treacheries <- select $ treacheryInThreatAreaOf iid <> TreacheryIsNonWeakness
      nonPeril <- filterM (fieldMap TreacheryKeywords (notMember Keyword.Peril)) treacheries
      chooseOneM iid do
        targets nonPeril $ toDiscardBy iid attrs
        cardI18n $ labeled "dreamsOfPeace.healDamageAndHorror" do
          healDamage iid attrs 1
          healHorror iid attrs 1
      pure e
    _ -> DreamsOfPeace <$> liftRunMessage msg attrs
