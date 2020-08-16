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
  ]

instance HasAbilities Act where
  getAbilities = actAbilities . actAttrs

instance IsAdvanceable Act where
  isAdvanceable = actCanAdvance . actAttrs

data Act
  = Trapped' Trapped
  | TheBarrier' TheBarrier
  | WhatHaveYouDone' WhatHaveYouDone
  | UncoveringTheConspiracy' UncoveringTheConspiracy
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

actAttrs :: Act -> Attrs
actAttrs = \case
  Trapped' attrs -> coerce attrs
  TheBarrier' attrs -> coerce attrs
  WhatHaveYouDone' attrs -> coerce attrs
  UncoveringTheConspiracy' attrs -> coerce attrs

instance (ActRunner env) => RunMessage env Act where
  runMessage msg = \case
    Trapped' x -> Trapped' <$> runMessage msg x
    TheBarrier' x -> TheBarrier' <$> runMessage msg x
    WhatHaveYouDone' x -> WhatHaveYouDone' <$> runMessage msg x
    UncoveringTheConspiracy' x -> UncoveringTheConspiracy' <$> runMessage msg x
