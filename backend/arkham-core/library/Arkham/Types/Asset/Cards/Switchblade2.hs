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
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Switchblade2 = Switchblade2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade2 :: AssetCard Switchblade2
switchblade2 = hand Switchblade2 Cards.switchblade2

instance HasModifiersFor env Switchblade2 where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env Switchblade2 where
  getActions iid window (Switchblade2 a) | ownedBy a iid = do
    let
      ability = mkAbility
        (toSource a)
        1
        (ActionAbility (Just Action.Fight) (ActionCost 1))
    fightAvailable <- hasFightActions iid window
    pure [ UseAbility iid ability | fightAvailable ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Switchblade2 where
  runMessage msg a@(Switchblade2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 2])
        source
        (InvestigatorTarget iid)
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget{} _ n
      | n >= 2 && isSource attrs source
      -> a <$ push
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [DamageDealt 1])
          source
          (InvestigatorTarget iid)
        )
    _ -> Switchblade2 <$> runMessage msg attrs
