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
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Machete = Machete AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = hand Machete Cards.machete

instance HasModifiersFor env Machete

instance HasActions env Machete where
  getActions iid _ (Machete a) | ownedBy a iid = pure
    [ UseAbility
        iid
        (mkAbility
          (toSource a)
          1
          (ActionAbility (Just Action.Fight) (ActionCost 1))
        )
    ]
  getActions _ _ _ = pure []

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
