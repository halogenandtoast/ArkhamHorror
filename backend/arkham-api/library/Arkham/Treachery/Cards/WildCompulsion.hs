module Arkham.Treachery.Cards.WildCompulsion (wildCompulsion) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype WildCompulsion = WildCompulsion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wildCompulsion :: TreacheryCard WildCompulsion
wildCompulsion = treachery WildCompulsion Cards.wildCompulsion

instance RunMessage WildCompulsion where
  runMessage msg t@(WildCompulsion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n | n > 0 -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasCards <- notNull <$> field InvestigatorHand iid
      resources <- field InvestigatorResources iid
      chooseOrRunOneM iid $ withI18n do
        when hasCards $ countVar 1 $ labeled' "discardRandomCardsFromHand" $ randomDiscard iid attrs
        when (resources > 0) $ countVar 1 $ labeled' "loseResources" $ loseResources iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> WildCompulsion <$> liftRunMessage msg attrs
