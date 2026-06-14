module Arkham.Location.Cards.TreacherousPathErodedShelf (treacherousPathErodedShelf) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TheWesternWall.Helpers (getLocationLevel)

newtype TreacherousPathErodedShelf = TreacherousPathErodedShelf LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treacherousPathErodedShelf :: LocationCard TreacherousPathErodedShelf
treacherousPathErodedShelf = location TreacherousPathErodedShelf Cards.treacherousPathErodedShelf 0 (Static 1)

instance HasAbilities TreacherousPathErodedShelf where
  getAbilities (TreacherousPathErodedShelf a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ Enters #after You (be a)

instance RunMessage TreacherousPathErodedShelf where
  runMessage msg l@(TreacherousPathErodedShelf attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      level <- getLocationLevel iid
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed level)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      push $ IncreaseFloodLevel attrs.id
      pure l
    _ -> TreacherousPathErodedShelf <$> liftRunMessage msg attrs
