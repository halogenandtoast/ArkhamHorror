module Arkham.Treachery.Cards.AlienWhispers (alienWhispers) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AlienWhispers = AlienWhispers TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alienWhispers :: TreacheryCard AlienWhispers
alienWhispers = treachery AlienWhispers Cards.alienWhispers

instance RunMessage AlienWhispers where
  runMessage msg t@(AlienWhispers attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n | n > 0 -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      hasCards <- notNull <$> field InvestigatorHand iid
      damageAssets <-
        select $ assetControlledBy iid <> AssetCanBeDamagedBySource (toSource attrs)
      horrorAssets <-
        select $ assetControlledBy iid <> AssetCanBeAssignedHorrorBy iid
      chooseOrRunOneM iid $ withI18n do
        cardI18n $ scope "alienWhispers" do
          when (notNull damageAssets) $ labeled' "damageAsset" $ chooseTargetM iid damageAssets \aid -> dealAssetDamage aid attrs 1
          when (notNull horrorAssets) $ labeled' "horrorAsset" $ chooseTargetM iid horrorAssets \aid -> dealAssetHorror aid attrs 1
        when hasCards $ labeled' "discardRandomCardsFromHand" $ randomDiscard iid attrs
      doStep (n - 1) msg'
      pure t
    _ -> AlienWhispers <$> liftRunMessage msg attrs
