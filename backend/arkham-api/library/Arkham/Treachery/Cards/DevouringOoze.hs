module Arkham.Treachery.Cards.DevouringOoze (devouringOoze) where

import Arkham.Asset.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Trait (Trait (Ally, Item))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DevouringOoze = DevouringOoze TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devouringOoze :: TreacheryCard DevouringOoze
devouringOoze = treachery DevouringOoze Cards.devouringOoze

instance RunMessage DevouringOoze where
  runMessage msg t@(DevouringOoze attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid <> mapOneOf AssetWithTrait [Ally, Item]
      case assets of
        [] -> assignDamage iid attrs 2
        _ -> chooseTargetM iid assets \aid -> do
          card <- field AssetCard aid
          devour [card]
      pure t
    _ -> DevouringOoze <$> liftRunMessage msg attrs
