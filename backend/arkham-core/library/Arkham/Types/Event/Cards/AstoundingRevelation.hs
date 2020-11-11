{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.AstoundingRevelation
  ( astoundingRevelation
  , AstoundingRevelation(..)
  )
where

import Arkham.Import

import Arkham.Types.Event.Attrs

-- Check player card with behavior

newtype AstoundingRevelation = AstoundingRevelation Attrs
  deriving newtype (Show, ToJSON, FromJSON)

astoundingRevelation :: InvestigatorId -> EventId -> AstoundingRevelation
astoundingRevelation iid uuid =
  AstoundingRevelation $ baseAttrs iid uuid "06023"

instance HasActions env AstoundingRevelation where
  getActions iid window (AstoundingRevelation attrs) =
    getActions iid window attrs

instance HasModifiersFor env AstoundingRevelation where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env AstoundingRevelation where
  runMessage msg (AstoundingRevelation attrs) =
    AstoundingRevelation <$> runMessage msg attrs
