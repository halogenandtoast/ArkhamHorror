module Arkham.Treachery.Cards.AnachronismDarkMatter (anachronismDarkMatter) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AnachronismDarkMatter = AnachronismDarkMatter TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anachronismDarkMatter :: TreacheryCard AnachronismDarkMatter
anachronismDarkMatter = treachery AnachronismDarkMatter Cards.anachronismDarkMatter

instance RunMessage AnachronismDarkMatter where
  runMessage msg t@(AnachronismDarkMatter attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      replicateM_ n do
        hasAssets <- selectAny $ DiscardableAsset <> AssetNonStory <> assetControlledBy iid
        chooseOneM iid $ withI18n do
          when hasAssets
            $ countVar 1
            $ labeled' "discardAssets"
            $ chooseAndDiscardAssetMatching iid attrs AssetNonStory
          chooseTakeHorror iid attrs 1
      pure t
    _ -> AnachronismDarkMatter <$> liftRunMessage msg attrs
