module Arkham.Treachery.Cards.AshenRebirth (ashenRebirth) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (InvestigatorDamage)

newtype AshenRebirth = AshenRebirth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashenRebirth :: TreacheryCard AshenRebirth
ashenRebirth = treachery AshenRebirth Cards.ashenRebirth

instance RunMessage AshenRebirth where
  runMessage msg t@(AshenRebirth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      damage <- field InvestigatorDamage iid
      case compare horror damage of
        GT -> directHorror iid attrs 2
        LT -> directDamage iid attrs 2
        EQ -> chooseOneM iid $ withI18n do
          countVar 2 $ labeled' "takeDirectDamage" $ directDamage iid attrs 2
          countVar 2 $ labeled' "takeDirectHorror" $ directHorror iid attrs 2
      pure t
    _ -> AshenRebirth <$> liftRunMessage msg attrs
