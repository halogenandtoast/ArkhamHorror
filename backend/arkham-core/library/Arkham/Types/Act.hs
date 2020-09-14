{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act
  ( Act(..)
  , lookupAct
  , canAdvance
  )
where

import Arkham.Json
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Cards.DisruptingTheRitual
import Arkham.Types.Act.Cards.IntoTheDarkness
import Arkham.Types.Act.Cards.InvestigatingTheTrail
import Arkham.Types.Act.Cards.TheBarrier
import Arkham.Types.Act.Cards.Trapped
import Arkham.Types.Act.Cards.UncoveringTheConspiracy
import Arkham.Types.Act.Cards.WhatHaveYouDone
import Arkham.Types.Act.Runner
import Arkham.Types.ActId
import Arkham.Types.Classes
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Generics.SOP hiding (Generic)
import qualified Generics.SOP as SOP
import Safe (fromJustNote)

lookupAct :: ActId -> Act
lookupAct = fromJustNote "Unknown act" . flip HashMap.lookup allActs

allActs :: HashMap ActId Act
allActs = HashMap.fromList $ map
  (\a -> (actId $ actAttrs a, a))
  [ Trapped' trapped
  , TheBarrier' theBarrier
  , WhatHaveYouDone' whatHaveYouDone
  , UncoveringTheConspiracy' uncoveringTheConspiracy
  , InvestigatingTheTrail' investigatingTheTrail
  , IntoTheDarkness' intoTheDarkness
  , DisruptingTheRitual' disruptingTheRitual
  ]

instance IsAdvanceable Act where
  isAdvanceable = actCanAdvance . actAttrs

data Act
  = Trapped' Trapped
  | TheBarrier' TheBarrier
  | WhatHaveYouDone' WhatHaveYouDone
  | UncoveringTheConspiracy' UncoveringTheConspiracy
  | InvestigatingTheTrail' InvestigatingTheTrail
  | IntoTheDarkness' IntoTheDarkness
  | DisruptingTheRitual' DisruptingTheRitual
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, SOP.Generic)

actAttrs :: Act -> Attrs
actAttrs = getAttrs

deriving anyclass instance (ActionRunner env investigator) => HasActions env investigator Act
deriving anyclass instance (ActRunner env) => RunMessage env Act

class (Coercible a Attrs) => IsAttrs a
instance (Coercible a Attrs) => IsAttrs a

getAttrs :: (All2 IsAttrs (Code a), SOP.Generic a) => a -> Attrs
getAttrs a = go (unSOP $ from a)
 where
  go :: (All2 IsAttrs xs) => NS (NP I) xs -> Attrs
  go (S next) = go next
  go (Z (I x :* _)) = coerce x
  go (Z Nil) = error "should not happen"
