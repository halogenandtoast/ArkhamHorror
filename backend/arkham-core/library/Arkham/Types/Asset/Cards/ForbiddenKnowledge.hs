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
import Arkham.Types.Target
import Arkham.Types.Window

newtype ForbiddenKnowledge = ForbiddenKnowledge AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

forbiddenKnowledge :: AssetCard ForbiddenKnowledge
forbiddenKnowledge = assetWith
  ForbiddenKnowledge
  Cards.forbiddenKnowledge
  (startingUsesL ?~ Uses Secret 4)


instance HasModifiersFor env ForbiddenKnowledge

instance HasActions env ForbiddenKnowledge where
  getActions iid FastPlayerWindow (ForbiddenKnowledge a) | ownedBy a iid = pure
    [ UseAbility
        iid
        (mkAbility
          (toSource a)
          1
          (FastAbility $ Costs
            [ UseCost (toId a) Secret 1
            , HorrorCost (toSource a) (InvestigatorTarget iid) 1
            , ExhaustCost (toTarget a)
            ]
          )
        )
    | useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ForbiddenKnowledge where
  runMessage msg a@(ForbiddenKnowledge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ pushAll
      (TakeResources iid 1 False
      : [ Discard (toTarget attrs) | useCount (assetUses attrs) == 0 ]
      )
    _ -> ForbiddenKnowledge <$> runMessage msg attrs
