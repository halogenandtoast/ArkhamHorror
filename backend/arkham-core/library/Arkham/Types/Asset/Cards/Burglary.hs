module Arkham.Types.Asset.Cards.Burglary
  ( Burglary(..)
  , burglary
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype Burglary = Burglary AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burglary :: AssetCard Burglary
burglary = asset Burglary Cards.burglary

instance HasModifiersFor env Burglary where
  getModifiersFor = noModifiersFor

instance HasActions env Burglary where
  getActions iid NonFast (Burglary a) | ownedBy a iid = pure
    [ assetAction iid a 1 (Just Action.Investigate)
        $ Costs [ActionCost 1, ExhaustCost (toTarget a)]
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Burglary where
  runMessage msg a@(Burglary attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      a <$ unshiftMessage
        (CreateEffect "01045" Nothing source (InvestigationTarget iid lid))
    _ -> Burglary <$> runMessage msg attrs
