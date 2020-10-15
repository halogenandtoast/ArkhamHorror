{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ForgottenMarsh where

import Arkham.Import

import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ForgottenMarsh = ForgottenMarsh Attrs
  deriving newtype (Show, ToJSON, FromJSON)

forgottenMarsh :: ForgottenMarsh
forgottenMarsh = ForgottenMarsh $ baseAttrs
  "81013"
  "Forgotten Marsh"
  2
  (Static 0)
  Diamond
  [Moon, Square, Triangle, Hourglass]
  [Wilderness, Bayou]

instance HasModifiersFor env investigator ForgottenMarsh where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator ForgottenMarsh where
  getActions i window (ForgottenMarsh attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ForgottenMarsh where
  runMessage msg l@(ForgottenMarsh attrs@Attrs {..}) = case msg of
    Will (MoveTo iid lid)
      | iid `elem` locationInvestigators && lid /= locationId ->
        l <$ unshiftMessage (SpendResources iid 2)
    _ -> ForgottenMarsh <$> runMessage msg attrs
