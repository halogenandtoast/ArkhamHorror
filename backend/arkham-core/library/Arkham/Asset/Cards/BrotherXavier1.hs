module Arkham.Asset.Cards.BrotherXavier1
  ( brotherXavier1
  , BrotherXavier1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.DamageEffect
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Matcher qualified as Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype BrotherXavier1 = BrotherXavier1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherXavier1 :: AssetCard BrotherXavier1
brotherXavier1 = ally BrotherXavier1 Cards.brotherXavier1 (3, 3)

instance HasModifiersFor BrotherXavier1 where
  getModifiersFor _ (InvestigatorTarget iid) (BrotherXavier1 a)
    | controlledBy a iid = pure $ toModifiers a [SkillModifier SkillWillpower 1]
  getModifiersFor (InvestigatorSource iid) target (BrotherXavier1 a)
    | isTarget a target = case assetPlacement a of
      InPlayArea iid' | iid' /= iid -> do
        locationId <- field InvestigatorLocation iid
        assetLocationId <- field AssetLocation (toId a)
        pure
          [ toModifier a CanBeAssignedDamage
          | (locationId == assetLocationId)
            && isJust locationId
          ]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities BrotherXavier1 where
  getAbilities (BrotherXavier1 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (Matcher.AssetDefeated Timing.When $ AssetWithId $ toId a)
        Free
    ]

instance RunMessage BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemies <- selectList (EnemyAt YourLocation)
      a <$ pushAll
        [ chooseOrRunOne
            iid
            [ targetLabel eid [EnemyDamage eid iid source NonAttackDamageEffect 2]
            | eid <- enemies
            ]
        ]
    _ -> BrotherXavier1 <$> runMessage msg attrs
