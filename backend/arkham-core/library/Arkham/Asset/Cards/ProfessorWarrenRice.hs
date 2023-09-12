module Arkham.Asset.Cards.ProfessorWarrenRice where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ProfessorWarrenRice = ProfessorWarrenRice AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWarrenRice :: AssetCard ProfessorWarrenRice
professorWarrenRice =
  allyWith
    ProfessorWarrenRice
    Cards.professorWarrenRice
    (2, 3)
    (isStoryL .~ True)

instance HasModifiersFor ProfessorWarrenRice where
  getModifiersFor (InvestigatorTarget iid) (ProfessorWarrenRice a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities ProfessorWarrenRice where
  getAbilities (ProfessorWarrenRice a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (DiscoveringLastClue Timing.After You YourLocation) (exhaust a)
    ]

instance RunMessage ProfessorWarrenRice where
  runMessage msg a@(ProfessorWarrenRice attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> ProfessorWarrenRice <$> runMessage msg attrs
