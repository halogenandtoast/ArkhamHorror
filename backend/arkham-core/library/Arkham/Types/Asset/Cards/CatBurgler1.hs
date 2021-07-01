module Arkham.Types.Asset.Cards.CatBurgler1
  ( CatBurgler1(..)
  , catBurgler1
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype CatBurgler1 = CatBurgler1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catBurgler1 :: AssetCard CatBurgler1
catBurgler1 = ally CatBurgler1 Cards.catBurgler1 (2, 2)

instance HasModifiersFor env CatBurgler1 where
  getModifiersFor _ (InvestigatorTarget iid) (CatBurgler1 a) =
    pure $ toModifiers a [ SkillModifier SkillAgility 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance HasActions env CatBurgler1 where
  getActions iid NonFast (CatBurgler1 a) | ownedBy a iid = pure
    [ assetAction iid a 1 Nothing
        $ Costs [ActionCost 1, ExhaustCost (toTarget a)]
    ]
  getActions i window (CatBurgler1 x) = getActions i window x

instance AssetRunner env => RunMessage env CatBurgler1 where
  runMessage msg (CatBurgler1 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage $ CreateWindowModifierEffect
        EffectSkillTestWindow
        (EffectModifiers $ toModifiers attrs [SkillModifier SkillAgility 1])
        (toSource attrs)
        (InvestigatorTarget iid)
      CatBurgler1 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      engagedEnemyIds <- getSetList iid
      locationId <- getId @LocationId iid
      accessibleLocationIds <- map unAccessibleLocationId
        <$> getSetList locationId
      unshiftMessages
        $ [ DisengageEnemy iid eid | eid <- engagedEnemyIds ]
        <> [ chooseOne
               iid
               [ MoveAction iid lid Free False | lid <- accessibleLocationIds ]
           | not (null accessibleLocationIds)
           ]
      pure $ CatBurgler1 $ attrs & exhaustedL .~ True
    _ -> CatBurgler1 <$> runMessage msg attrs
