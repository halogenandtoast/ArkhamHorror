module Arkham.Treachery.Cards.HeavyRain (heavyRain) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HeavyRain = HeavyRain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

heavyRain :: TreacheryCard HeavyRain
heavyRain = treachery HeavyRain Cards.heavyRain

instance RunMessage HeavyRain where
  runMessage msg t@(HeavyRain attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTestBy iid (isSource attrs -> True) _) | n > 0 -> do
      clues <- field InvestigatorClues iid
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid attrs 1
        countVar 1
          $ labeledValidate' (clues > 0) "placeCluesOnYourLocation"
          $ placeCluesOnLocation iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> HeavyRain <$> liftRunMessage msg attrs
