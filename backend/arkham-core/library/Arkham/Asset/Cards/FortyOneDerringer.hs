module Arkham.Asset.Cards.FortyOneDerringer
  ( FortyOneDerringer(..)
  , fortyOneDerringer
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype FortyOneDerringer = FortyOneDerringer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyOneDerringer :: AssetCard FortyOneDerringer
fortyOneDerringer = asset FortyOneDerringer Cards.fortyOneDerringer

instance HasAbilities FortyOneDerringer where
  getAbilities (FortyOneDerringer a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Fight)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Ammo 1])
    ]

instance RunMessage FortyOneDerringer where
  runMessage msg a@(FortyOneDerringer attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 2)
      , ChooseFightEnemy iid source Nothing SkillCombat mempty False
      ]
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget{} _ n
      | isSource attrs source && n >= 2
      -> a <$ push
        (skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1))
    _ -> FortyOneDerringer <$> runMessage msg attrs
