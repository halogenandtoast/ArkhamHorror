module Arkham.Treachery.Cards.WeightOfTheWorld (weightOfTheWorld) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WeightOfTheWorld = WeightOfTheWorld TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

weightOfTheWorld :: TreacheryCard WeightOfTheWorld
weightOfTheWorld = treachery WeightOfTheWorld Cards.weightOfTheWorld

instance HasAbilities WeightOfTheWorld where
  getAbilities (WeightOfTheWorld a) =
    [ restrictedAbility a 1 (InThreatAreaOf You)
        $ forced
        $ SkillTestResult #after You AnySkillTest #failure
    ]

instance RunMessage WeightOfTheWorld where
  runMessage msg t@(WeightOfTheWorld attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      shuffleIntoDeck iid attrs
      pure t
    _ -> WeightOfTheWorld <$> liftRunMessage msg attrs
