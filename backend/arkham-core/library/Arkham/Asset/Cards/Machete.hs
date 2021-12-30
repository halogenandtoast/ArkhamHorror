module Arkham.Asset.Cards.Machete
  ( Machete(..)
  , machete
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Modifier
import Arkham.Query
import Arkham.SkillType
import Arkham.Target

newtype Machete = Machete AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = asset Machete Cards.machete

instance HasAbilities Machete where
  getAbilities (Machete a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
    ]

instance AssetRunner env => RunMessage env Machete where
  runMessage msg a@(Machete attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      criteriaMet <- (== 1) . unEnemyCount <$> getCount iid
      a <$ pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          ([ DamageDealt 1 | criteriaMet ] <> [SkillModifier SkillCombat 1])
        , ChooseFightEnemy iid source Nothing SkillCombat mempty False
        ]
    _ -> Machete <$> runMessage msg attrs
