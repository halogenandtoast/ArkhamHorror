module Arkham.Asset.Assets.ProfessorWarrenRice where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ProfessorWarrenRice = ProfessorWarrenRice AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWarrenRice :: AssetCard ProfessorWarrenRice
professorWarrenRice =
  allyWith
    ProfessorWarrenRice
    Cards.professorWarrenRice
    (2, 3)
    (isStoryL .~ True)

instance HasModifiersFor ProfessorWarrenRice where
  getModifiersFor (ProfessorWarrenRice a) = controllerGets a [SkillModifier #intellect 1]

instance HasAbilities ProfessorWarrenRice where
  getAbilities (ProfessorWarrenRice a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (DiscoveringLastClue #after You YourLocation) (exhaust a)
    ]

instance RunMessage ProfessorWarrenRice where
  runMessage msg a@(ProfessorWarrenRice attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> ProfessorWarrenRice <$> runMessage msg attrs
