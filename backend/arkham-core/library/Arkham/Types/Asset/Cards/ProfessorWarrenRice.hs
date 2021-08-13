module Arkham.Types.Asset.Cards.ProfessorWarrenRice where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype ProfessorWarrenRice = ProfessorWarrenRice AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWarrenRice :: AssetCard ProfessorWarrenRice
professorWarrenRice = allyWith
  ProfessorWarrenRice
  Cards.professorWarrenRice
  (2, 3)
  (isStoryL .~ True)

instance HasModifiersFor env ProfessorWarrenRice where
  getModifiersFor _ (InvestigatorTarget iid) (ProfessorWarrenRice a) =
    pure [ toModifier a (SkillModifier SkillIntellect 1) | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions ProfessorWarrenRice where
  getActions (ProfessorWarrenRice a) =
    [ restrictedAbility a 1 OwnsThis $ ReactionAbility
        (DiscoveringLastClue Timing.After You YourLocation)
        ExhaustThis
    ]

instance AssetRunner env => RunMessage env ProfessorWarrenRice where
  runMessage msg a@(ProfessorWarrenRice attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> ProfessorWarrenRice <$> runMessage msg attrs
