module Arkham.Treachery.Cards.StolenLight (stolenLight) where

import Arkham.Asset.Types (Field (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Projection
import Arkham.Scenarios.TheTwistedHollow.Helpers
import Arkham.Trait (Trait (Lit))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StolenLight = StolenLight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stolenLight :: TreacheryCard StolenLight
stolenLight = treachery StolenLight Cards.stolenLight

instance RunMessage StolenLight where
  runMessage msg t@(StolenLight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mLantern <- selectOne $ AssetWithTitle "Vale Lantern"
      mLoc <- runMaybeT $ MaybeT . field AssetLocation =<< hoistMaybe mLantern
      farthestLocations <- select $ FarthestLocationFromAll EmptyLocation

      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 1 $ labeled' "placeAgendaDoomCanAdvance" $ placeDoomOnAgendaAndCheckAdvance 1
        for_ mLantern \lantern -> do
          lit <- matches lantern $ AssetWithTrait Lit
          labeledValidate' (lit || maybe False (`notElem` farthestLocations) mLoc) "stolenLight.lantern" do
            when lit $ flipOverBy iid attrs lantern
            chooseTargetM iid farthestLocations $ place lantern . AtLocation
      pure t
    _ -> StolenLight <$> liftRunMessage msg attrs
