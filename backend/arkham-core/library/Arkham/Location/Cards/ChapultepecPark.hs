module Arkham.Location.Cards.ChapultepecPark
  ( chapultepecPark
  , ChapultepecPark(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype ChapultepecPark = ChapultepecPark LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapultepecPark :: LocationCard ChapultepecPark
chapultepecPark = locationWith
  ChapultepecPark
  Cards.chapultepecPark
  1
  (Static 0)
  (labelL .~ "triangle")

instance HasAbilities ChapultepecPark where
  getAbilities (ChapultepecPark attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility attrs 1 Here $ ForcedAbility $ SkillTestResult
          Timing.After
          You
          (SkillTestWithSkillType SkillWillpower)
          (FailureResult AnyValue)
        , restrictedAbility attrs 2 Here
        $ ActionAbility (Just Action.Explore)
        $ ActionCost 1
        ]
      else []

instance RunMessage ChapultepecPark where
  runMessage msg l@(ChapultepecPark attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 1
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      push $ beginSkillTest
        iid
        (toSource attrs)
        (InvestigatorTarget iid)
        SkillWillpower
        3
      pure l
    PassedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        push
          $ Explore iid (toSource attrs)
          $ CardWithPrintedLocationSymbol
          $ locationSymbol attrs
        pure l
    _ -> ChapultepecPark <$> runMessage msg attrs
