module Arkham.Types.Asset.Cards.Adaptable1
  ( adaptable1
  , Adaptable1(..)
  ) where


import Arkham.Types.Asset.Attrs

newtype Adaptable1 = Adaptable1 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

adaptable1 :: AssetId -> Adaptable1
adaptable1 uuid = Adaptable1 $ baseAttrs uuid "02110"

instance HasActions env Adaptable1 where
  getActions iid window (Adaptable1 attrs) = getActions iid window attrs

instance HasModifiersFor env Adaptable1 where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env Adaptable1 where
  runMessage msg (Adaptable1 attrs) = Adaptable1 <$> runMessage msg attrs
