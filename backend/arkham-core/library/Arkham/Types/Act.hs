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
import Arkham.Types.Helpers
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
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
  deriving anyclass (ToJSON, FromJSON)

actAttrs :: Act -> Attrs
actAttrs = \case
  Trapped' attrs -> coerce attrs
  TheBarrier' attrs -> coerce attrs
  WhatHaveYouDone' attrs -> coerce attrs
  UncoveringTheConspiracy' attrs -> coerce attrs
  InvestigatingTheTrail' attrs -> coerce attrs
  IntoTheDarkness' attrs -> coerce attrs
  DisruptingTheRitual' (DisruptingTheRitual (attrs `With` _)) -> attrs

deriving anyclass instance (ActionRunner env investigator) => HasActions env investigator Act
deriving anyclass instance (ActRunner env) => RunMessage env Act
