module Arkham.Homebrew.DarkMatter.Treacheries.ColdVacuum (coldVacuum) where

import Arkham.Discard
import Arkham.Helpers.Message.Discard (discardFromHand)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ColdVacuum = ColdVacuum TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coldVacuum :: TreacheryCard ColdVacuum
coldVacuum = treachery ColdVacuum Cards.coldVacuum

instance RunMessage ColdVacuum where
  runMessage msg t@(ColdVacuum attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #combat (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      hasAssets <- selectAny $ DiscardableAsset <> AssetNonStory <> assetControlledBy iid
      chooseOneM iid $ withI18n do
        when hasAssets
          $ countVar 1
          $ labeled' "discardAssets"
          $ chooseAndDiscardAssetMatching iid attrs AssetNonStory
        countVar 3 $ labeled' "discardCardsFromHand" $ push $ toMessage $ discardFromHand iid attrs DiscardChoose 3
      pure t
    _ -> ColdVacuum <$> liftRunMessage msg attrs
