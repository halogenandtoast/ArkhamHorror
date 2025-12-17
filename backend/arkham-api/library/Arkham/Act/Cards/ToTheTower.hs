module Arkham.Act.Cards.ToTheTower (toTheTower) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Trait (Trait (Tower))

newtype ToTheTower = ToTheTower ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

toTheTower :: ActCard ToTheTower
toTheTower = act (2, A) ToTheTower Cards.toTheTower Nothing

instance HasModifiersFor ToTheTower where
  getModifiersFor (ToTheTower a) = do
    modifySelect a (LocationWithTrait Tower) [ScenarioModifier "noCityOfRemnants"]

instance RunMessage ToTheTower where
  runMessage msg a@(ToTheTower attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> ToTheTower <$> liftRunMessage msg attrs
