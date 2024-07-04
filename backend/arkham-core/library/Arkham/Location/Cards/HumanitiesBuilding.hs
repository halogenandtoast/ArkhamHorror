module Arkham.Location.Cards.HumanitiesBuilding where

import Arkham.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (humanitiesBuilding)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype HumanitiesBuilding = HumanitiesBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

humanitiesBuilding :: LocationCard HumanitiesBuilding
humanitiesBuilding = location HumanitiesBuilding Cards.humanitiesBuilding 3 (PerPlayer 2)

instance HasAbilities HumanitiesBuilding where
  getAbilities (HumanitiesBuilding attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (Here <> youExist InvestigatorWithAnyHorror)
          $ forced
          $ TurnEnds #when You
      ]

instance RunMessage HumanitiesBuilding where
  runMessage msg l@(HumanitiesBuilding attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      horror <- field InvestigatorHorror iid
      push $ DiscardTopOfDeck iid horror (attrs.ability 1) Nothing
      pure l
    _ -> HumanitiesBuilding <$> liftRunMessage msg attrs
