module Arkham.Types.Asset.Cards.FireExtinguisher1 where

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
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype FireExtinguisher1 = FireExtinguisher1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

fireExtinguisher1 :: AssetCard FireExtinguisher1
fireExtinguisher1 = hand FireExtinguisher1 Cards.fireExtinguisher1

instance HasModifiersFor env FireExtinguisher1

instance HasActions env FireExtinguisher1 where
  getActions iid NonFast (FireExtinguisher1 a) | ownedBy a iid = do
    pure
      [ mkAbility a 1 $ ActionAbility (Just Action.Fight) (ActionCost 1)
      , mkAbility a 2 $ ActionAbility
        (Just Action.Evade)
        (Costs [ActionCost 1, ExileCost $ toTarget a])
      ]
  getActions i window (FireExtinguisher1 x) = getActions i window x

instance (AssetRunner env) => RunMessage env FireExtinguisher1 where
  runMessage msg a@(FireExtinguisher1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    _ -> FireExtinguisher1 <$> runMessage msg attrs
