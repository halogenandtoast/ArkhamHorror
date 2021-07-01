module Arkham.Types.Asset.Cards.FireAxe
  ( FireAxe(..)
  , fireAxe
  )
where

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
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillTest
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype FireAxe = FireAxe AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireAxe :: AssetCard FireAxe
fireAxe = hand FireAxe Cards.fireAxe

fightAbility :: AssetAttrs -> Ability
fightAbility attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility (Just Action.Fight) (ActionCost 1))

reactionAbility :: AssetAttrs -> Ability
reactionAbility attrs = base { abilityLimit = PlayerLimit PerTestOrAbility 3 } -- per attack
  where base = mkAbility (toSource attrs) 2 (ReactionAbility $ ResourceCost 1)

instance HasCount ResourceCount env InvestigatorId => HasModifiersFor env FireAxe where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) (InvestigatorTarget iid) (FireAxe a)
    | ownedBy a iid && isSource a source
    = do
      resourceCount <- getResourceCount iid
      pure $ toModifiers a [ DamageDealt 1 | resourceCount == 0 ]
  getModifiersFor _ _ _ = pure []

instance (ActionRunner env, HasSkillTest env) => HasActions env FireAxe where
  getActions iid NonFast (FireAxe a) | ownedBy a iid = do
    fightAvailable <- hasFightActions iid NonFast
    pure [ ActivateCardAbilityAction iid (fightAbility a) | fightAvailable ]
  getActions iid (WhenSkillTest _) (FireAxe a) | ownedBy a iid = do
    msource <- getSkillTestSource
    let
      using = case msource of
        Just (SkillTestSource _ _ source _ (Just Action.Fight))
          | isSource a source -> True
        _ -> False
    pure [ ActivateCardAbilityAction iid (reactionAbility a) | using ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env FireAxe where
  runMessage msg a@(FireAxe attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessages
        [ CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 1])
          source
          (InvestigatorTarget iid)
        , ChooseFightEnemy iid source SkillCombat mempty False
        ]
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      a <$ unshiftMessage
        (CreateWindowModifierEffect
          EffectSkillTestWindow
          (EffectModifiers $ toModifiers attrs [SkillModifier SkillCombat 2])
          source
          (InvestigatorTarget iid)
        )
    _ -> FireAxe <$> runMessage msg attrs
