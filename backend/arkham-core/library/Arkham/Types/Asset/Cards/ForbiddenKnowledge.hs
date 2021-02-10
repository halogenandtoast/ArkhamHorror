module Arkham.Types.Asset.Cards.ForbiddenKnowledge where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses

newtype ForbiddenKnowledge = ForbiddenKnowledge AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

forbiddenKnowledge :: AssetId -> ForbiddenKnowledge
forbiddenKnowledge uuid = ForbiddenKnowledge $ baseAttrs uuid "01058"

instance HasModifiersFor env ForbiddenKnowledge where
  getModifiersFor = noModifiersFor

instance HasActions env ForbiddenKnowledge where
  getActions iid FastPlayerWindow (ForbiddenKnowledge a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility
          (toSource a)
          1
          (FastAbility $ Costs
            [ UseCost (toId a) Secret 1
            , HorrorCost (toSource a) (InvestigatorTarget iid) 1
            ]
          )
        )
    | useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env ForbiddenKnowledge where
  runMessage msg a@(ForbiddenKnowledge attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      ForbiddenKnowledge <$> runMessage msg (attrs & usesL .~ Uses Secret 4)
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeResources iid 1 False)
    _ -> ForbiddenKnowledge <$> runMessage msg attrs
