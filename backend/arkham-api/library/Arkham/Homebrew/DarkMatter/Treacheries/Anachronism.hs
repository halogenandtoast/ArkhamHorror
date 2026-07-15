module Arkham.Homebrew.DarkMatter.Treacheries.Anachronism (anachronism) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Anachronism = Anachronism TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

anachronism :: TreacheryCard Anachronism
anachronism = treachery Anachronism Cards.anachronism

instance RunMessage Anachronism where
  runMessage msg t@(Anachronism attrs) = runQueueT $ case msg of
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
    _ -> Anachronism <$> liftRunMessage msg attrs
