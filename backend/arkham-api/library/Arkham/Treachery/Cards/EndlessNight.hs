module Arkham.Treachery.Cards.EndlessNight (endlessNight) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TheLongestNight.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EndlessNight = EndlessNight TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessNight :: TreacheryCard EndlessNight
endlessNight = treachery EndlessNight Cards.endlessNight

instance RunMessage EndlessNight where
  runMessage msg t@(EndlessNight attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      agenda <- selectJust AnyAgenda
      mCaptives <- selectOne $ assetIs Assets.theCaptives
      chooseOneM iid $ scenarioI18n do
        unscoped $ countVar 1 $ labeled' "removeAgendaDoom" $ removeDoom attrs agenda 1
        for_ mCaptives \captives ->
          labeled' "endlessNight.damageCaptives" $ dealAssetDamage captives attrs 2
        unscoped $ numberVar "damage" 3 $ numberVar "horror" 3 $ labeled' "takeDirectDamageAndHorror" do
          directDamageAndHorror iid attrs 3 3
      pure t
    _ -> EndlessNight <$> liftRunMessage msg attrs
