module Arkham.Location.Cards.FrenchHill_291 (
  frenchHill_291,
  FrenchHill_291 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype FrenchHill_291 = FrenchHill_291 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frenchHill_291 :: LocationCard FrenchHill_291
frenchHill_291 = location FrenchHill_291 Cards.frenchHill_291 4 (Static 0)

instance HasAbilities FrenchHill_291 where
  getAbilities (FrenchHill_291 attrs) =
    withRevealedAbilities attrs [restrictedAbility attrs 1 Here $ ActionAbility [] $ ActionCost 1]

instance RunMessage FrenchHill_291 where
  runMessage msg l@(FrenchHill_291 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (attrs.ability 1) attrs #willpower 2
      pure l
    PassedSkillTest _iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ n -> do
      let n' = min (maybe 0 countBreaches $ locationBreaches attrs) n
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) n', PlaceBreaches (toTarget act) n]
      pure l
    _ -> FrenchHill_291 <$> runMessage msg attrs
