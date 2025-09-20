module Arkham.Treachery.Cards.PerilsOfYoth (perilsOfYoth) where

import Arkham.Card
import Arkham.Helpers.Location
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Result (..))
import Arkham.Window qualified as Window

newtype PerilsOfYoth = PerilsOfYoth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perilsOfYoth :: TreacheryCard PerilsOfYoth
perilsOfYoth = treachery PerilsOfYoth Cards.perilsOfYoth

instance RunMessage PerilsOfYoth where
  runMessage msg t@(PerilsOfYoth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      depth <- getCurrentDepth
      mlid <- getLocationOf iid
      chooseOneM iid $ scenarioI18n do
        labeled' "perilsOfYoth.fail" do
          checkAfter $ Window.Explored iid mlid (Failure $ toCard attrs)
        labeled' "perilsOfYoth.continue" do
          assignDamageAndHorror iid attrs depth depth
          matcher <- case mlid of
            Just lid -> mapOneOf CardWithPrintedLocationSymbol <$> toConnections lid
            Nothing -> pure $ NotCard AnyCard
          source <- fromMaybe (toSource attrs) <$> getCurrentExploreSource
          push $ Do (Explore iid source matcher)
      removeTreachery attrs
      setCardAside (toCard attrs)
      pure t
    _ -> PerilsOfYoth <$> liftRunMessage msg attrs
