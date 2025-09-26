module Arkham.Location.Cards.FrankElwoodsRoom (frankElwoodsRoom) where

import Arkham.Ability
import Arkham.I18n
import Arkham.GameValue
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.TheSecretName.Helpers

newtype FrankElwoodsRoom = FrankElwoodsRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frankElwoodsRoom :: LocationCard FrankElwoodsRoom
frankElwoodsRoom = location FrankElwoodsRoom Cards.frankElwoodsRoom 3 (PerPlayer 1)

instance HasAbilities FrankElwoodsRoom where
  getAbilities (FrankElwoodsRoom a) =
    extendRevealed1 a $ scenarioI18n $ hauntedI "frankElwoodsRoom.haunted" a 1

instance RunMessage FrankElwoodsRoom where
  runMessage msg l@(FrankElwoodsRoom attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasClues <- fieldMap InvestigatorClues (> 0) iid
      chooseOneM iid $ scenarioI18n do
        labeledValidate' hasClues "frankElwoodsRoom.place" do
          removeTokens (attrs.ability 1) iid #clue 1
          placeClues (attrs.ability 1) attrs 1
        unscoped $ labeled' "placeAgendaDoom" $ placeDoomOnAgenda 1
      pure l
    _ -> FrankElwoodsRoom <$> liftRunMessage msg attrs
