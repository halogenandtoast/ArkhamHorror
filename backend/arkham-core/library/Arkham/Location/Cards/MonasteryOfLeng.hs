module Arkham.Location.Cards.MonasteryOfLeng (monasteryOfLeng, MonasteryOfLeng (..)) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.ScenarioLogKey

newtype MonasteryOfLeng = MonasteryOfLeng LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

monasteryOfLeng :: LocationCard MonasteryOfLeng
monasteryOfLeng = location MonasteryOfLeng Cards.monasteryOfLeng 3 (PerPlayer 2)

instance HasModifiersFor MonasteryOfLeng where
  getModifiersFor target (MonasteryOfLeng attrs) | attrs `is` target = do
    pure $ toModifiers attrs [Blocked | not attrs.revealed]
  getModifiersFor _ _ = pure []

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
    _ -> MonasteryOfLeng <$> lift (runMessage msg attrs)
