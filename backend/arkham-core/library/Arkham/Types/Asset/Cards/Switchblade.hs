module Arkham.Types.Asset.Cards.Switchblade
  ( Switchblade(..)
  , switchblade
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

newtype Switchblade = Switchblade AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade :: AssetCard Switchblade
switchblade = hand Switchblade Cards.switchblade

instance HasActions Switchblade where
  getActions (Switchblade a) =
    [ restrictedAbility a 1 OwnsThis
        $ ActionAbility (Just Action.Fight) (ActionCost 1)
    ]

instance (AssetRunner env) => RunMessage env Switchblade where
  runMessage msg a@(Switchblade attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (ChooseFightEnemy iid source SkillCombat mempty False)
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget{} _ n
      | n >= 2 && isSource attrs source
      -> a <$ push
        (skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1))
    _ -> Switchblade <$> runMessage msg attrs
