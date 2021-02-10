module Arkham.Types.Asset.Cards.OldBookOfLore
  ( OldBookOfLore(..)
  , oldBookOfLore
  ) where

import Arkham.Prelude

import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Target
import Arkham.Types.Window
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetId -> OldBookOfLore
oldBookOfLore uuid =
  OldBookOfLore $ (baseAttrs uuid "01031") { assetSlots = [HandSlot] }

instance HasModifiersFor env OldBookOfLore where
  getModifiersFor = noModifiersFor

instance HasActions env OldBookOfLore where
  getActions iid NonFast (OldBookOfLore a) | ownedBy a iid = pure
    [ assetAction iid a 1 Nothing
        $ Costs [ActionCost 1, ExhaustCost $ toTarget a]
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs@AssetAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorIds <- getSetList locationId
      a <$ unshiftMessage
        (chooseOne
          iid
          [ SearchTopOfDeck iid' (InvestigatorTarget iid') 3 [] ShuffleBackIn
          | iid' <- investigatorIds
          ]
        )
    _ -> OldBookOfLore <$> runMessage msg attrs
