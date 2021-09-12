module Arkham.Types.Asset.Cards.Grounded1
  ( grounded1
  , Grounded1(..)
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
import Arkham.Types.Target
import Arkham.Types.Trait

newtype Grounded1 = Grounded1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grounded1 :: AssetCard Grounded1
grounded1 = assetWith Grounded1 Cards.grounded1 (sanityL ?~ 1)

instance HasAbilities Grounded1 where
  getAbilities (Grounded1 x) =
    [ restrictedAbility
          x
          1
          (OwnsThis <> DuringSkillTest
            (SkillTestSourceMatches $ SourceWithTrait Spell)
          )
        $ FastAbility
        $ ResourceCost 1
    ]

instance HasModifiersFor env Grounded1 where
  getModifiersFor _ (AssetTarget aid) (Grounded1 attrs) | toId attrs == aid =
    pure $ toModifiers attrs [NonDirectHorrorMustBeAssignToThisFirst]
  getModifiersFor _ _ _ = pure []

instance AssetRunner env => RunMessage env Grounded1 where
  runMessage msg a@(Grounded1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push
        (skillTestModifier attrs (InvestigatorTarget iid) (AnySkillValue 1))
    _ -> Grounded1 <$> runMessage msg attrs
