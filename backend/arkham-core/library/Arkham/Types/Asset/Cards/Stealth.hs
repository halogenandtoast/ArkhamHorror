module Arkham.Types.Asset.Cards.Stealth
  ( stealth
  , Stealth(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Stealth = Stealth AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stealth :: AssetCard Stealth
stealth = asset Stealth Cards.stealth

instance HasActions Stealth where
  getActions (Stealth attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Evade)
        $ ActionCost 1
    ]

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Stealth where
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
