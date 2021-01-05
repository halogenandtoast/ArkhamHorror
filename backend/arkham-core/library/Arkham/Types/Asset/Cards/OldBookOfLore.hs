module Arkham.Types.Asset.Cards.OldBookOfLore
  ( OldBookOfLore(..)
  , oldBookOfLore
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype OldBookOfLore = OldBookOfLore Attrs
  deriving newtype (Show, ToJSON, FromJSON)

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
  runMessage msg a@(OldBookOfLore attrs@Attrs {..}) = case msg of
    UseCardAbility iid source _ 1 | isSource attrs source -> do
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
