module Arkham.Types.Asset.Cards.Stealth
  ( stealth
  , Stealth(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

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

instance AssetRunner env => RunMessage env Stealth where
  runMessage msg a@(Stealth attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (ChooseEvadeEnemy iid source SkillAgility False)
    ChosenEvadeEnemy source eid | isSource attrs source -> a <$ push
      (skillTestModifiers
        source
        (EnemyTarget eid)
        [EnemyEvade (-2), AlternateSuccessfullEvasion]
      )
    AfterSkillTestEnds source target@(EnemyTarget eid) n
      | isSource attrs source && n >= 0 -> case assetInvestigator attrs of
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
