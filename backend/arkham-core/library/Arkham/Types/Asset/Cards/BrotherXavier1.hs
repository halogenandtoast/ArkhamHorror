module Arkham.Types.Asset.Cards.BrotherXavier1
  ( brotherXavier1
  , BrotherXavier1(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import qualified Arkham.Types.Restriction as R
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

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

instance HasActions BrotherXavier1 where
  getActions (BrotherXavier1 x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility
          (R.AssetDefeated Timing.When (AssetWithId $ toId x))
          Free
        )
    ]

instance AssetRunner env => RunMessage env BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- getSetList locationId
      a <$ pushAll
        [ chooseOne
            iid
            [ EnemyDamage eid iid (toSource attrs) 2 | eid <- locationEnemyIds ]
        ]
    _ -> BrotherXavier1 <$> runMessage msg attrs
