module Arkham.Location.Cards.ChapultepecPark (chapultepecPark, ChapultepecPark (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ChapultepecPark = ChapultepecPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecPark :: LocationCard ChapultepecPark
chapultepecPark = locationWith ChapultepecPark Cards.chapultepecPark 1 (Static 0) (labelL .~ "triangle")

instance HasAbilities ChapultepecPark where
  getAbilities (ChapultepecPark attrs) =
    withRevealedAbilities
      attrs
      [ restrictedAbility attrs 1 Here
          $ ForcedAbility
          $ SkillTestResult #after You (SkillTestWithSkillType #willpower) (FailureResult AnyValue)
      , restrictedAbility attrs 2 Here exploreAction_
      ]

instance RunMessage ChapultepecPark where
  runMessage msg l@(ChapultepecPark attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ assignHorror iid attrs 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 2) iid #willpower 3
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      let source = toAbilitySource attrs 2
      push $ Explore iid source $ CardWithPrintedLocationSymbol $ locationSymbol attrs
      pure l
    _ -> ChapultepecPark <$> runMessage msg attrs
