module Arkham.Treachery.Cards.CryptChill (cryptChill) where

import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CryptChill = CryptChill TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptChill :: TreacheryCard CryptChill
cryptChill = treachery CryptChill Cards.cryptChill

instance RunMessage CryptChill where
  runMessage msg t@(CryptChill attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      hasAssets <- selectAny $ DiscardableAsset <> assetControlledBy iid
      if hasAssets
        then chooseAndDiscardAsset iid attrs
        else assignDamage iid attrs 2
      pure t
    _ -> CryptChill <$> liftRunMessage msg attrs
