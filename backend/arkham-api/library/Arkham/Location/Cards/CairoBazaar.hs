module Arkham.Location.Cards.CairoBazaar (cairoBazaar) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Item))

newtype CairoBazaar = CairoBazaar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cairoBazaar :: LocationCard CairoBazaar
cairoBazaar = symbolLabel $ location CairoBazaar Cards.cairoBazaar 2 (PerPlayer 1)

instance HasModifiersFor CairoBazaar where
  getModifiersFor (CairoBazaar a) = do
    modifySelect
      a
      (investigatorAt a)
      [ ReduceCostOf (#asset <> CardWithTrait Item) 1
      , IncreaseCostOf (basic $ #asset <> not_ (CardWithTrait Item)) 1
      ]

instance HasAbilities CairoBazaar where
  getAbilities (CairoBazaar a) =
    extendRevealed1 a $ restricted a 1 Here $ actionAbilityWithCost (ResourceCost 10)

instance RunMessage CairoBazaar where
  runMessage msg l@(CairoBazaar attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember BoughtAnOddTrinket
      pure l
    _ -> CairoBazaar <$> liftRunMessage msg attrs
