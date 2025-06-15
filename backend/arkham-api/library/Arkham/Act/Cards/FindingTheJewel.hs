module Arkham.Act.Cards.FindingTheJewel (
  findingTheJewel,
  FindingTheJewel(..),
) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (modifySelect)
import Arkham.Keyword
import Arkham.Matcher
import Arkham.Trait

newtype FindingTheJewel = FindingTheJewel ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findingTheJewel :: ActCard FindingTheJewel
findingTheJewel = act (2, A) FindingTheJewel Cards.findingTheJewel Nothing

instance HasModifiersFor FindingTheJewel where
  getModifiersFor (FindingTheJewel a) =
    modifySelect a (AssetWithTrait Guest) [DoNotTakeUpSlot AllySlot]

instance RunMessage FindingTheJewel where
  runMessage msg a@(FindingTheJewel attrs) = runQueueT $ case msg of
    -- TODO implement parley abilities and advancement
    _ -> FindingTheJewel <$> liftRunMessage msg attrs
