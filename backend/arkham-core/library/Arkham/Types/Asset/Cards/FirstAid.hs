module Arkham.Types.Asset.Cards.FirstAid
  ( FirstAid(..)
  , firstAid
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Source
import Arkham.Types.Target

newtype FirstAid = FirstAid AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetCard FirstAid
firstAid = asset FirstAid Cards.firstAid

instance HasModifiersFor env FirstAid

instance HasActions FirstAid where
  getActions (FirstAid x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ActionAbility Nothing $ Costs [ActionCost 1, UseCost (toId x) Supply 1]
        )
    ]

instance AssetRunner env => RunMessage env FirstAid where
  runMessage msg a@(FirstAid attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid (AssetSource aid) _ 1 _ | aid == assetId -> do
      lid <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList lid
      a <$ push
        (chooseOne
          iid
          [ TargetLabel
              target
              [chooseOne iid [HealDamage target 1, HealHorror target 1]]
          | target <- investigatorTargets
          ]
        )
    _ -> FirstAid <$> runMessage msg attrs
