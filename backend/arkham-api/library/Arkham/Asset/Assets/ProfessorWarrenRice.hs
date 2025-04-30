module Arkham.Asset.Assets.ProfessorWarrenRice (professorWarrenRice) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher

newtype ProfessorWarrenRice = ProfessorWarrenRice AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWarrenRice :: AssetCard ProfessorWarrenRice
professorWarrenRice = ally ProfessorWarrenRice Cards.professorWarrenRice (2, 3)

instance HasModifiersFor ProfessorWarrenRice where
  getModifiersFor (ProfessorWarrenRice a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities ProfessorWarrenRice where
  getAbilities (ProfessorWarrenRice a) =
    [restricted a 1 ControlsThis $ triggered (DiscoveringLastClue #after You YourLocation) (exhaust a)]

instance RunMessage ProfessorWarrenRice where
  runMessage msg a@(ProfessorWarrenRice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> ProfessorWarrenRice <$> liftRunMessage msg attrs
