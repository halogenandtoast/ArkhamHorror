module Arkham.Asset.Cards.Stealth
  ( stealth
  , Stealth(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype Stealth = Stealth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stealth :: AssetCard Stealth
stealth = asset Stealth Cards.stealth

instance HasAbilities Stealth where
  getAbilities (Stealth attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Evade)
        $ ActionCost 1
    ]

instance RunMessage Stealth where
  runMessage msg a@(Stealth attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (ChooseEvadeEnemy
        iid
        source
        (Just $ toTarget attrs)
        SkillAgility
        AnyEnemy
        False
      )
    ChosenEvadeEnemy source eid | isSource attrs source ->
      a <$ push (skillTestModifier source (EnemyTarget eid) (EnemyEvade (-2)))
    AfterSkillTestEnds source target@(EnemyTarget eid) n
      | isSource attrs source && n >= 0 -> case assetController attrs of
        Just iid -> a <$ pushAll
          [ CreateWindowModifierEffect
            EffectTurnWindow
            (EffectModifiers $ toModifiers attrs [EnemyCannotEngage iid])
            source
            target
          , DisengageEnemy iid eid
          ]
        Nothing -> error "must be owned"
    _ -> Stealth <$> runMessage msg attrs
