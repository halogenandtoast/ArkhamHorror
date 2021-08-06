module Arkham.Types.EffectId where

import Arkham.Prelude

newtype EffectId = EffectId { unEffectId :: UUID }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)
