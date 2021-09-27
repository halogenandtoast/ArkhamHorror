module Arkham.Types.Asset.Cards.BrotherXavier1
  ( brotherXavier1
  , BrotherXavier1(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.DamageEffect
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message hiding (AssetDefeated)
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype BrotherXavier1 = BrotherXavier1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brotherXavier1 :: AssetCard BrotherXavier1
brotherXavier1 = ally BrotherXavier1 Cards.brotherXavier1 (3, 3)

instance (HasId LocationId env InvestigatorId) => HasModifiersFor env BrotherXavier1 where
  getModifiersFor _ (InvestigatorTarget iid) (BrotherXavier1 a)
    | ownedBy a iid = pure $ toModifiers a [SkillModifier SkillWillpower 1]
  getModifiersFor (InvestigatorSource iid) target (BrotherXavier1 a)
    | isTarget a target = do
      locationId <- getId @LocationId iid
      assetLocationId <- getId @LocationId
        $ fromJustNote "unowned" (assetInvestigator a)
      pure
        [ toModifier a CanBeAssignedDamage
        | locationId == assetLocationId && Just iid /= assetInvestigator a
        ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BrotherXavier1 where
  getAbilities (BrotherXavier1 a) =
    [ restrictedAbility a 1 OwnsThis
        $ ReactionAbility
            (AssetDefeated Timing.When $ AssetWithId $ toId a)
            Free
    ]

instance AssetRunner env => RunMessage env BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      enemies <- selectList (EnemyAt YourLocation)
      a <$ pushAll
        [ chooseOrRunOne
            iid
            [ EnemyDamage eid iid source NonAttackDamageEffect 2
            | eid <- enemies
            ]
        ]
    _ -> BrotherXavier1 <$> runMessage msg attrs
