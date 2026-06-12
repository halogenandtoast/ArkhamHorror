module Arkham.Treachery.Cards.VengeantPast (vengeantPast) where

import Arkham.Asset.Types (Field (..))
import Arkham.Deck qualified as Deck
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.RelicsOfThePast.Helpers
import Arkham.Trait (Trait (Ancient))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype VengeantPast = VengeantPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vengeantPast :: TreacheryCard VengeantPast
vengeantPast = treachery VengeantPast Cards.vengeantPast

instance RunMessage VengeantPast where
  runMessage msg t@(VengeantPast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ AssetWithTrait Ancient <> AssetControlledBy Anyone
      for_ assets \asset -> do
        mController <- field AssetController asset
        for_ mController \controller -> do
          name <- field AssetName asset
          chooseOneM iid $ scenarioI18n $ nameVar name do
            questionLabeled' "vengeantPast.question"
            labeled' "vengeantPast.dealDamage" $ assignDamage controller attrs 1
            labeled' "vengeantPast.dealHorror" $ assignHorror controller attrs 1
            labeled' "vengeantPast.shuffleAsset"
              $ shuffleIntoDeck (Deck.ScenarioDeckByKey ExplorationDeck) asset
      shuffleIntoDeck (Deck.ScenarioDeckByKey ExplorationDeck) attrs
      pure t
    _ -> VengeantPast <$> liftRunMessage msg attrs
