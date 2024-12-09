module Arkham.Location.Cards.BrackishWaters (BrackishWaters (..), brackishWaters) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BrackishWaters = BrackishWaters LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brackishWaters :: LocationCard BrackishWaters
brackishWaters = location BrackishWaters Cards.brackishWaters 1 (Static 0)

instance HasModifiersFor BrackishWaters where
  getModifiersFor (BrackishWaters a) = do
    whenRevealed a $ modifySelect a (investigatorAt a) [CannotPlay #asset]

instance HasAbilities BrackishWaters where
  getAbilities (BrackishWaters attrs) =
    extendRevealed1 attrs
      $ skillTestAbility
      $ restricted attrs 1 (Here <> notExists (assetIs Assets.fishingNet))
      $ actionAbilityWithCost
      $ DiscardFromCost 2 (FromHandOf You <> FromPlayAreaOf You) #asset

instance RunMessage BrackishWaters where
  runMessage msg l@(BrackishWaters attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      takeControlOfSetAsideAsset iid =<< getSetAsideCard Assets.fishingNet
      pure l
    _ -> BrackishWaters <$> liftRunMessage msg attrs
