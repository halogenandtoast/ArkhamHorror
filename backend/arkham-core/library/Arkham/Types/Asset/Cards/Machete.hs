module Arkham.Types.Asset.Cards.Machete
  ( Machete(..)
  , machete
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Machete = Machete AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = hand Machete Cards.machete

instance HasActions Machete where
  getActions (Machete a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight) (ActionCost 1)
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
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    _ -> Machete <$> runMessage msg attrs
