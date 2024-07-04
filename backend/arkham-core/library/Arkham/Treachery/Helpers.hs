{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-operator-whitespace #-}

module Arkham.Treachery.Helpers (module X, module Arkham.Treachery.Helpers) where

import Arkham.Game.Helpers as X

import Arkham.Helpers.Placement
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Target
import Data.Kind (Constraint)
import GHC.TypeLits

type family NotEqual (a :: Type) (b :: Type) :: Constraint where
  NotEqual a a = TypeError ('Text "Type must not be " ':<>: 'ShowType a)
  NotEqual _a _b = ()

attachTreachery
  :: ( HasCallStack
     , AsId a
     , IdOf a ~ TreacheryId
     , Targetable target
     , Show target
     , NotEqual target InvestigatorId
     )
  => a
  -> target
  -> Message
attachTreachery treachery target = PlaceTreachery (asId treachery) (attachTo target)

placeInThreatArea
  :: (AsId a, IdOf a ~ TreacheryId)
  => a
  -> InvestigatorId
  -> Message
placeInThreatArea treachery = PlaceTreachery (asId treachery) . InThreatArea
