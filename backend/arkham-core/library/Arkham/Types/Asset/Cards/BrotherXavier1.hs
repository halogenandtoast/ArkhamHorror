module Arkham.Types.Asset.Cards.BrotherXavier1
  ( brotherXavier1
  , BrotherXavier1(..)
  )
where

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
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window

newtype BrotherXavier1 = BrotherXavier1 AssetAttrs
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

ability :: AssetAttrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ReactionAbility Free)

instance HasActions env BrotherXavier1 where
  getActions iid (WhenDefeated source) (BrotherXavier1 a) | isSource a source =
    pure [ ActivateCardAbilityAction iid (ability a) | ownedBy a iid ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env BrotherXavier1 where
  runMessage msg a@(BrotherXavier1 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId (getInvestigator attrs)
      locationEnemyIds <- getSetList locationId
      a <$ unshiftMessages
        [ chooseOne
            iid
            [ EnemyDamage eid iid (toSource attrs) 2 | eid <- locationEnemyIds ]
        ]
    _ -> BrotherXavier1 <$> runMessage msg attrs
