module Arkham.Types.Asset.Cards.CombatTraining1
  ( combatTraining1
  , CombatTraining1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype CombatTraining1 = CombatTraining1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

combatTraining1 :: AssetCard CombatTraining1
combatTraining1 =
  assetWith CombatTraining1 Cards.combatTraining1 (sanityL ?~ 1)

instance HasAbilities CombatTraining1 where
  getAbilities (CombatTraining1 x) =
    [ restrictedAbility x idx (OwnsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ResourceCost 1
    | idx <- [1, 2]
    ]

instance HasModifiersFor env CombatTraining1 where
  getModifiersFor _ target@(AssetTarget aid) (CombatTraining1 attrs)
    | toId attrs == aid = pure
    $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env CombatTraining1 where
  runMessage msg a@(CombatTraining1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      )
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillAgility 1)
      )
    _ -> CombatTraining1 <$> runMessage msg attrs
