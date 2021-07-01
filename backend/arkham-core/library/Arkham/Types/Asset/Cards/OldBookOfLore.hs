module Arkham.Types.Asset.Cards.OldBookOfLore
  ( OldBookOfLore(..)
  , oldBookOfLore
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype OldBookOfLore = OldBookOfLore AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldBookOfLore :: AssetCard OldBookOfLore
oldBookOfLore = hand OldBookOfLore Cards.oldBookOfLore

instance HasModifiersFor env OldBookOfLore where
  getModifiersFor = noModifiersFor

instance HasActions env OldBookOfLore where
  getActions iid NonFast (OldBookOfLore a) | ownedBy a iid = pure
    [ assetAction iid a 1 Nothing
        $ Costs [ActionCost 1, ExhaustCost $ toTarget a]
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env OldBookOfLore where
  runMessage msg a@(OldBookOfLore attrs) = case msg of
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
