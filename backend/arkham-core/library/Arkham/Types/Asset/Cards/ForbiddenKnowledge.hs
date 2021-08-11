module Arkham.Types.Asset.Cards.ForbiddenKnowledge where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype ForbiddenKnowledge = ForbiddenKnowledge AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

forbiddenKnowledge :: AssetCard ForbiddenKnowledge
forbiddenKnowledge = asset ForbiddenKnowledge Cards.forbiddenKnowledge


instance HasModifiersFor env ForbiddenKnowledge

instance HasActions ForbiddenKnowledge where
  getActions (ForbiddenKnowledge a) =
    [ restrictedAbility
        (toSource a)
        1
        OwnsThis
        (FastAbility $ Costs
          [ UseCost (toId a) Secret 1
          , HorrorCost (toSource a) YouTarget 1
          , ExhaustThis
          ]
        )
    ]

instance (AssetRunner env) => RunMessage env ForbiddenKnowledge where
  runMessage msg a@(ForbiddenKnowledge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      (TakeResources iid 1 False
      : [ Discard (toTarget attrs) | useCount (assetUses attrs) == 0 ]
      )
    _ -> ForbiddenKnowledge <$> runMessage msg attrs
