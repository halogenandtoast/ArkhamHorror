module Arkham.Asset.Cards.HeaddressOfYhaNthlei
  ( headdressOfYhaNthlei
  , HeaddressOfYhaNthlei(..)
  )
where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype HeaddressOfYhaNthlei = HeaddressOfYhaNthlei AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

headdressOfYhaNthlei :: AssetCard HeaddressOfYhaNthlei
headdressOfYhaNthlei = asset HeaddressOfYhaNthlei Cards.headdressOfYhaNthlei

instance RunMessage HeaddressOfYhaNthlei where
  runMessage msg (HeaddressOfYhaNthlei attrs) = runQueueT $ case msg of
    _ -> HeaddressOfYhaNthlei <$> liftRunMessage msg attrs
