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
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( NonAttackDamageEffect )
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype BrotherXavier1 = BrotherXavier1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherXavier1 :: AssetCard BrotherXavier1
brotherXavier1 = ally BrotherXavier1 Cards.brotherXavier1 (3, 3)

instance HasModifiersFor BrotherXavier1 where
  getModifiersFor (InvestigatorTarget iid) (BrotherXavier1 a)
    | controlledBy a iid = pure $ toModifiers a [SkillModifier SkillWillpower 1]
  getModifiersFor (InvestigatorTarget iid) (BrotherXavier1 a)
    | not (controlledBy a iid) = do
      locationId <- field InvestigatorLocation iid
      assetLocationId <- field AssetLocation (toId a)
      pure
        [ toModifier a (CanAssignDamageToAsset $ toId a)
        | (locationId == assetLocationId) && isJust locationId
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities BrotherXavier1 where
  getAbilities (BrotherXavier1 a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (Matcher.AssetDefeated Timing.When ByAny $ AssetWithId $ toId a)
        Free
    ]

instance RunMessage BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      enemies <- selectList $ EnemyAt $ locationWithInvestigator iid
      push $ chooseOrRunOne
        iid
        [ targetLabel eid [EnemyDamage eid $ nonAttack source 2]
        | eid <- enemies
        ]
      pure a
    _ -> BrotherXavier1 <$> runMessage msg attrs
