module Arkham.Event.Cards.BizarreDiagnosis (bizarreDiagnosis, BizarreDiagnosis (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BizarreDiagnosis = BizarreDiagnosis EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bizarreDiagnosis :: EventCard BizarreDiagnosis
bizarreDiagnosis = event BizarreDiagnosis Cards.bizarreDiagnosis

instance RunMessage BizarreDiagnosis where
  runMessage msg e@(BizarreDiagnosis attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      push $ InvestigatorPlaceCluesOnLocation iid (toSource attrs) 1

      assets <- selectTargets $ HealableAsset (toSource attrs) #damage (assetAtLocationWith iid)
      investigators <- selectTargets $ HealableInvestigator (toSource attrs) #damage $ colocatedWith iid

      chooseOneM iid $ targets (assets <> investigators) \target -> healDamage target attrs 3
      pure e
    _ -> BizarreDiagnosis <$> liftRunMessage msg attrs
