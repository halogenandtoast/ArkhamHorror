module Arkham.Treachery.Cards.CorrosiveSlime (corrosiveSlime) where

import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CorrosiveSlime = CorrosiveSlime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosiveSlime :: TreacheryCard CorrosiveSlime
corrosiveSlime = treachery CorrosiveSlime Cards.corrosiveSlime

instance RunMessage CorrosiveSlime where
  runMessage msg t@(CorrosiveSlime attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n | n > 0 -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hand <- field InvestigatorHand iid
      resources <- field InvestigatorResources iid
      mRandomCard <- traverse sample (nonEmpty hand)
      chooseOrRunOneM iid $ withI18n do
        cardI18n $ scope "corrosiveSlime" do
          when (resources > 0) do
            labeled' "devourResource" $ loseResources iid attrs 1
          for_ mRandomCard \card -> do
            labeled' "devourRandomCardFromHand" $ devour [card]
      doStep (n - 1) msg'
      pure t
    _ -> CorrosiveSlime <$> liftRunMessage msg attrs
