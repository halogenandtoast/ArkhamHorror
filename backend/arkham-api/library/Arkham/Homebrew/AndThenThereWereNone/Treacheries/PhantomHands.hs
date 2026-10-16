{- | "__Revelation__ - Test {agility} (3). If you fail, you must either discard an
asset you control or lose 3 resources."
-}
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.PhantomHands (phantomHands) where

import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype PhantomHands = PhantomHands TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

phantomHands :: TreacheryCard PhantomHands
phantomHands = treachery PhantomHands Cards.phantomHands

instance RunMessage PhantomHands where
  runMessage msg t@(PhantomHands attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      -- "must either": the discard half is only an option while an asset can take it.
      hasAsset <- selectAny $ assetControlledBy iid <> DiscardableAsset
      chooseOneM iid $ scenarioI18n $ scope "phantomHands" do
        when hasAsset $ labeled "discardAsset" $ chooseAndDiscardAsset iid attrs
        labeled "loseResources" $ loseResources iid attrs 3
      pure t
    _ -> PhantomHands <$> liftRunMessage msg attrs
