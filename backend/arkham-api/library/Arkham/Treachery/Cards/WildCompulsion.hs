module Arkham.Treachery.Cards.WildCompulsion (wildCompulsion) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
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
      beginSkillTest sid iid (toSource attrs) iid #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n | n > 0 -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasCards <- notNull <$> field InvestigatorHand iid
      resources <- field InvestigatorResources iid
      if hasCards && resources > 0
        then chooseOneM iid do
          labeled "Discard 1 card at random from your hand" do
            randomDiscard iid attrs
          labeled "Lose 1 resource" do
            loseResources iid attrs 1
        else if hasCards
          then randomDiscard iid attrs
          else loseResources iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> WildCompulsion <$> liftRunMessage msg attrs
