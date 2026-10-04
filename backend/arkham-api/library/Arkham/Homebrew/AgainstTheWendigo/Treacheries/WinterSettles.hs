module Arkham.Homebrew.AgainstTheWendigo.Treacheries.WinterSettles (winterSettles) where

import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype WinterSettles = WinterSettles TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

winterSettles :: TreacheryCard WinterSettles
winterSettles = treachery WinterSettles Cards.winterSettles

instance RunMessage WinterSettles where
  runMessage msg t@(WinterSettles attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    -- "Choose and discard 1 asset you control. If you cannot, take 1 direct damage instead."
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> DiscardableAsset
      if null assets
        then directDamage iid attrs 1
        else chooseOrRunOneM iid $ targets assets $ toDiscardBy iid attrs
      pure t
    _ -> WinterSettles <$> liftRunMessage msg attrs
