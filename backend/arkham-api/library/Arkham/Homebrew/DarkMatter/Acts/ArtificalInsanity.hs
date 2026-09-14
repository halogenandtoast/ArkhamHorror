module Arkham.Homebrew.DarkMatter.Acts.ArtificalInsanity (artificalInsanity) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype ArtificalInsanity = ArtificalInsanity ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artificalInsanity :: ActCard ArtificalInsanity
artificalInsanity = act (2, A) ArtificalInsanity Cards.artificalInsanity Nothing

instance RunMessage ArtificalInsanity where
  runMessage msg a@(ArtificalInsanity attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- select Anyone
      leadChooseOneM $ targets investigators (`forInvestigator` msg)
      advanceActDeck attrs
      pure a
    ForInvestigator iid (AdvanceAct (isSide B attrs -> True) _ _) -> do
      card <- fetchCard Assets.virtualAccessKey
      createAssetAt_ card (InPlayArea iid)
      chooseOneM iid $ campaignI18n do
        labeled "virtualAccessKey.addToDeck"
          $ addCampaignCardToDeck iid DoNotShuffleIn Assets.virtualAccessKey
        labeled "virtualAccessKey.doNotAddToDeck" nothing
      pure a
    _ -> ArtificalInsanity <$> liftRunMessage msg attrs
