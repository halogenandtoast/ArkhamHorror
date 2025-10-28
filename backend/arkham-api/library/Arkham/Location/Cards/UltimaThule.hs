module Arkham.Location.Cards.UltimaThule (ultimaThule) where

import Arkham.Ability
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.Helpers.Scenario (scenarioFieldMap)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))

newtype UltimaThule = UltimaThule LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ultimaThule :: LocationCard UltimaThule
ultimaThule = location UltimaThule Cards.ultimaThule 4 (Static 2)

instance HasAbilities UltimaThule where
  getAbilities (UltimaThule a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ PutLocationIntoPlay #after Anyone (be a)

instance RunMessage UltimaThule where
  runMessage msg l@(UltimaThule attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chaosTokens <- scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
      let mElderSignToken = find ((== ElderSign) . chaosTokenFace) chaosTokens
      for_ mElderSignToken $ \chaosToken -> do
        pushAll [SealChaosToken chaosToken, SealedChaosToken chaosToken (Just iid) (toTarget attrs)]
      pure l
    _ -> UltimaThule <$> liftRunMessage msg attrs
