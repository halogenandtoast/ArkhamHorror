module Arkham.Location.Cards.TadsGeneralStoreDay (tadsGeneralStoreDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (codex)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype TadsGeneralStoreDay = TadsGeneralStoreDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tadsGeneralStoreDay :: LocationCard TadsGeneralStoreDay
tadsGeneralStoreDay = symbolLabel $ location TadsGeneralStoreDay Cards.tadsGeneralStoreDay 2 (Static 0)

instance HasAbilities TadsGeneralStoreDay where
  getAbilities (TadsGeneralStoreDay a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> youExist (InvestigatorWithoutModifier $ ScenarioModifier "codex14"))
        actionAbility

instance RunMessage TadsGeneralStoreDay where
  runMessage msg l@(TadsGeneralStoreDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 14
      pure l
    _ -> TadsGeneralStoreDay <$> liftRunMessage msg attrs
