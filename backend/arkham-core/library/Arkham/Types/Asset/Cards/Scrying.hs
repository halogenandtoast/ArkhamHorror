module Arkham.Types.Asset.Cards.Scrying
  ( Scrying(..)
  , scrying
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype Scrying = Scrying AssetAttrs
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

scrying :: AssetCard Scrying
scrying = arcane Scrying Cards.scrying

instance HasModifiersFor env Scrying where
  getModifiersFor = noModifiersFor

instance HasActions env Scrying where
  getActions iid NonFast (Scrying a) | ownedBy a iid = pure
    [ assetAction iid a 1 Nothing
        $ Costs
            [ActionCost 1, UseCost (toId a) Charge 1, ExhaustCost (toTarget a)]
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Scrying where
  runMessage msg a@(Scrying attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Scrying <$> runMessage msg (attrs & usesL .~ Uses Charge 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      targets <- map InvestigatorTarget <$> getSetList locationId
      a <$ unshiftMessage
        (chooseOne iid
        $ SearchTopOfDeck iid EncounterDeckTarget 3 [] PutBackInAnyOrder
        : [ SearchTopOfDeck iid target 3 [] PutBackInAnyOrder
          | target <- targets
          ]
        )
    _ -> Scrying <$> runMessage msg attrs
