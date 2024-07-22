module Arkham.Location.Cards.TrappersCabin (
  TrappersCabin (..),
  trappersCabin,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher

newtype TrappersCabin = TrappersCabin LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

trappersCabin :: LocationCard TrappersCabin
trappersCabin = location TrappersCabin Cards.trappersCabin 3 (Static 0)

instance HasModifiersFor TrappersCabin where
  getModifiersFor (InvestigatorTarget iid) (TrappersCabin attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [CannotGainResources | here]
  getModifiersFor _ _ = pure []

instance HasAbilities TrappersCabin where
  getAbilities (TrappersCabin attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 (Here <> Negate (exists $ assetIs Assets.bearTrap))
            $ actionAbilityWithCost (ResourceCost 5)
        ]

instance RunMessage TrappersCabin where
  runMessage msg l@(TrappersCabin attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      bearTrap <- getSetAsideCard Assets.bearTrap
      push $ TakeControlOfSetAsideAsset iid bearTrap
      pure l
    _ -> TrappersCabin <$> runMessage msg attrs
