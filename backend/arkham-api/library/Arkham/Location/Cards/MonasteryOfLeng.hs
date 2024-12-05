module Arkham.Location.Cards.MonasteryOfLeng (monasteryOfLeng, MonasteryOfLeng (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (Blocked), modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.ScenarioLogKey

newtype MonasteryOfLeng = MonasteryOfLeng LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monasteryOfLeng :: LocationCard MonasteryOfLeng
monasteryOfLeng = location MonasteryOfLeng Cards.monasteryOfLeng 3 (PerPlayer 2)

instance HasModifiersFor MonasteryOfLeng where
  getModifiersFor (MonasteryOfLeng a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities MonasteryOfLeng where
  getAbilities (MonasteryOfLeng attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 (Here <> not_ (Remembered ManeuveredThePriestCloser))
          $ FastAbility (GroupClueCost (PerPlayer 1) (be attrs))
      ]

instance RunMessage MonasteryOfLeng where
  runMessage msg l@(MonasteryOfLeng attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      remember ManeuveredThePriestCloser
      pure l
    _ -> MonasteryOfLeng <$> liftRunMessage msg attrs
