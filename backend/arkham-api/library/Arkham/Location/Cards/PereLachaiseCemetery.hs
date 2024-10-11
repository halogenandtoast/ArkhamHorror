module Arkham.Location.Cards.PereLachaiseCemetery (pereLachaiseCemetery, PereLachaiseCemetery (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype PereLachaiseCemetery = PereLachaiseCemetery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pereLachaiseCemetery :: LocationCard PereLachaiseCemetery
pereLachaiseCemetery =
  location PereLachaiseCemetery Cards.pereLachaiseCemetery 1 (PerPlayer 2)

instance HasAbilities PereLachaiseCemetery where
  getAbilities (PereLachaiseCemetery attrs) =
    extendRevealed1 attrs
      $ restricted attrs 1 Here
      $ forced
      $ SkillTestResult #after You (WhileInvestigating $ be attrs) (SuccessResult AnyValue)

instance RunMessage PereLachaiseCemetery where
  runMessage msg a@(PereLachaiseCemetery attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) iid CannotMove
      pure a
    _ -> PereLachaiseCemetery <$> liftRunMessage msg attrs
