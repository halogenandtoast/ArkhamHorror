module Arkham.Types.Asset.Cards.Shrivelling
  ( Shrivelling(..)
  , shrivelling
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Shrivelling = Shrivelling AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

shrivelling :: AssetCard Shrivelling
shrivelling = arcane Shrivelling Cards.shrivelling

instance HasModifiersFor env Shrivelling where
  getModifiersFor = noModifiersFor

instance HasActions env Shrivelling where
  getActions iid _ (Shrivelling a) | ownedBy a iid = do
    pure
      [ UseAbility
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility
              (Just Action.Fight)
              (Costs [ActionCost 1, UseCost (toId a) Charge 1])
            )
          )
      ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Shrivelling where
  runMessage msg a@(Shrivelling attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Shrivelling <$> runMessage msg (attrs & usesL .~ Uses Charge 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      [ skillTestModifiers attrs (InvestigatorTarget iid) [DamageDealt 1]
      , CreateEffect "01060" Nothing source (InvestigatorTarget iid)
      , ChooseFightEnemy iid source SkillWillpower mempty False
      ]
    _ -> Shrivelling <$> runMessage msg attrs
