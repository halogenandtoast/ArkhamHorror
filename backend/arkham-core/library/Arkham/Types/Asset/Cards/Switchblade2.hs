module Arkham.Types.Asset.Cards.Switchblade2
  ( Switchblade2(..)
  , switchblade2
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
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Switchblade2 = Switchblade2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade2 :: AssetCard Switchblade2
switchblade2 = hand Switchblade2 Cards.switchblade2

instance HasActions Switchblade2 where
  getActions (Switchblade2 a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight) (ActionCost 1)
    ]

instance AssetRunner env => RunMessage env Switchblade2 where
  runMessage msg a@(Switchblade2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 2)
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget{} _ n
      | n >= 2 && isSource attrs source
      -> a <$ push
        (skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1))
    _ -> Switchblade2 <$> runMessage msg attrs
