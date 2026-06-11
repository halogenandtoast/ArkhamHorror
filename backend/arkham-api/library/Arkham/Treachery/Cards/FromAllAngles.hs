module Arkham.Treachery.Cards.FromAllAngles (fromAllAngles) where

import Arkham.Helpers.Location (getLocationOf)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Ally, Future, Past, Present))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FromAllAngles = FromAllAngles TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fromAllAngles :: TreacheryCard FromAllAngles
fromAllAngles = treachery FromAllAngles Cards.fromAllAngles

instance RunMessage FromAllAngles where
  runMessage msg t@(FromAllAngles attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      getLocationOf iid >>= traverse_ \lid -> do
        traits <- field LocationTraits lid
        allies <- select $ AssetWithTrait Ally <> AssetAt (LocationWithId lid)
        when (Past `member` traits) do
          assets <- select $ AssetAt (LocationWithId lid)
          for_ assets exhaustThis
        when (Present `member` traits) do
          directDamage iid attrs 1
          for_ allies \ally -> dealAssetDamage ally attrs 1
        when (Future `member` traits) do
          directHorror iid attrs 1
          for_ allies \ally -> dealAssetHorror ally attrs 1
      pure t
    _ -> FromAllAngles <$> liftRunMessage msg attrs
