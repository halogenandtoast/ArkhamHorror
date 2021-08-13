module Arkham.Types.Asset.Cards.FireAxe
  ( FireAxe(..)
  , fireAxe
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
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype FireAxe = FireAxe AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fireAxe :: AssetCard FireAxe
fireAxe = hand FireAxe Cards.fireAxe

instance HasCount ResourceCount env InvestigatorId => HasModifiersFor env FireAxe where
  getModifiersFor (SkillTestSource _ _ source _ (Just Action.Fight)) (InvestigatorTarget iid) (FireAxe a)
    | ownedBy a iid && isSource a source
    = do
      resourceCount <- getResourceCount iid
      pure $ toModifiers a [ DamageDealt 1 | resourceCount == 0 ]
  getModifiersFor _ _ _ = pure []

instance HasActions FireAxe where
  getActions (FireAxe a) =
    [ restrictedAbility a 1 OwnsThis
      $ ActionAbility (Just Action.Fight) (ActionCost 1)
    , restrictedAbility
        a
        2
        (OwnsThis
        <> DuringSkillTest (WhileAttackingAnEnemy AnyEnemy <> UsingThis)
        )
      $ FastAbility (ResourceCost 1)
    ]

instance AssetRunner env => RunMessage env FireAxe where
  runMessage msg a@(FireAxe attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 1)
      , ChooseFightEnemy iid source SkillCombat mempty False
      ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> a <$ push
      (skillTestModifier
        attrs
        (InvestigatorTarget iid)
        (SkillModifier SkillCombat 2)
      )
    _ -> FireAxe <$> runMessage msg attrs
