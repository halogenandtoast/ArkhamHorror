module Arkham.Types.Asset.Cards.Knife
  ( Knife(..)
  , knife
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action (Action(Fight))
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype Knife = Knife AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knife :: AssetCard Knife
knife = hand Knife Cards.knife

instance HasModifiersFor env Knife

instance HasActions env Knife where
  getActions iid NonFast (Knife a) | ownedBy a iid = pure
    [ UseAbility iid (mkAbility a 1 (ActionAbility (Just Fight) (ActionCost 1)))
    , UseAbility
      iid
      (mkAbility
        a
        2
        (ActionAbility
          (Just Fight)
          (Costs [ActionCost 1, DiscardCost (toTarget a)])
        )
      )
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Knife where
  runMessage msg a@(Knife attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers
        attrs
        (InvestigatorTarget iid)
        [SkillModifier SkillCombat 2, DamageDealt 1]
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    _ -> Knife <$> runMessage msg attrs
