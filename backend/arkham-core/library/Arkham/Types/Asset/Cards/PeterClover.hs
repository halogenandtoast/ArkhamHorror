module Arkham.Types.Asset.Cards.PeterClover
  ( peterClover
  , PeterClover(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher hiding (EnemyEvaded)
import Arkham.Types.Message
import Arkham.Types.Trait

newtype PeterClover = PeterClover AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterClover :: AssetCard PeterClover
peterClover =
  allyWith PeterClover Cards.peterClover (3, 2)
    $ (slotsL .~ [])
    . (isStoryL .~ True)

instance HasAbilities env PeterClover where
  getAbilities _ _ (PeterClover attrs) = pure
    [ restrictedAbility
        attrs
        1
        (OwnsThis
        <> EnemyExists (EnemyAt YourLocation <> EnemyWithTrait Criminal)
        )
        (FastAbility $ ExhaustCost $ toTarget attrs)
    ]

instance AssetRunner env => RunMessage env PeterClover where
  runMessage msg a@(PeterClover attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      criminals <- getSetList ([Criminal], lid)
      a <$ push (chooseOne iid [ EnemyEvaded iid eid | eid <- criminals ])
    BeginEnemy | isNothing assetInvestigator ->
      a <$ push (AssetDamage assetId (toSource attrs) 1 0)
    _ -> PeterClover <$> runMessage msg attrs
