module Arkham.Types.Asset.Cards.SpiritAthame1
  ( spiritAthame1
  , SpiritAthame1(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Trait

newtype SpiritAthame1 = SpiritAthame1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritAthame1 :: AssetCard SpiritAthame1
spiritAthame1 = hand SpiritAthame1 Cards.spiritAthame1

instance HasAbilities SpiritAthame1 where
  getAbilities (SpiritAthame1 x) =
    [ restrictedAbility
      x
      1
      (OwnsThis
      <> DuringSkillTest (SkillTestSourceMatches $ SourceWithTrait Spell)
      )
    $ FastAbility
    $ ExhaustCost (toTarget x)
    , restrictedAbility x 2 OwnsThis $ ActionAbility
      (Just Action.Fight)
      (Costs [ActionCost 1, ExhaustCost (toTarget x)])
    ]

instance AssetRunner env => RunMessage env SpiritAthame1 where
  runMessage msg a@(SpiritAthame1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (skillTestModifier source (InvestigatorTarget iid) (AnySkillValue 2))
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        source
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 2)
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    _ -> SpiritAthame1 <$> runMessage msg attrs
