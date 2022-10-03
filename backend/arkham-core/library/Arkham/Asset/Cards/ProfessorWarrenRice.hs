module Arkham.Asset.Cards.ProfessorWarrenRice where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ProfessorWarrenRice = ProfessorWarrenRice AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWarrenRice :: AssetCard ProfessorWarrenRice
professorWarrenRice = allyWith
  ProfessorWarrenRice
  Cards.professorWarrenRice
  (2, 3)
  (isStoryL .~ True)

instance HasModifiersFor ProfessorWarrenRice where
  getModifiersFor (InvestigatorTarget iid) (ProfessorWarrenRice a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance HasAbilities ProfessorWarrenRice where
  getAbilities (ProfessorWarrenRice a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (DiscoveringLastClue Timing.After You YourLocation)
        (ExhaustCost $ toTarget a)
    ]

instance RunMessage ProfessorWarrenRice where
  runMessage msg a@(ProfessorWarrenRice attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> ProfessorWarrenRice <$> runMessage msg attrs
