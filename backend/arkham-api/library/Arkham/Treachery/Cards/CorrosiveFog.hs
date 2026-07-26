module Arkham.Treachery.Cards.CorrosiveFog (corrosiveFog) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillTest.Base
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CorrosiveFog = CorrosiveFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

corrosiveFog :: TreacheryCard CorrosiveFog
corrosiveFog = treachery CorrosiveFog Cards.corrosiveFog

instance RunMessage CorrosiveFog where
  runMessage msg t@(CorrosiveFog attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseBeginSkillTestEdit sid iid attrs iid [#intellect, #combat] (Fixed 4) setIsRevelation
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      if n >= 3
        then do
          directDamage iid attrs 1
          assets <- select $ assetControlledBy iid <> AssetWithHealth <> oneOf [#ally, #item]
          for_ assets \aid -> dealAssetDamage aid attrs 1
        else assignDamage iid attrs 1
      pure t
    _ -> CorrosiveFog <$> liftRunMessage msg attrs
