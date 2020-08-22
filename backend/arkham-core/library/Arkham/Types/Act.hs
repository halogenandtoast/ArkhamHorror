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
  DisruptingTheRitual' attrs -> coerce attrs

instance (ActionRunner env investigator) => HasActions env investigator Act where
  getActions i window = \case
    Trapped' x -> getActions i window x
    TheBarrier' x -> getActions i window x
    WhatHaveYouDone' x -> getActions i window x
    UncoveringTheConspiracy' x -> getActions i window x
    InvestigatingTheTrail' x -> getActions i window x
    IntoTheDarkness' x -> getActions i window x
    DisruptingTheRitual' x -> getActions i window x

instance (ActRunner env) => RunMessage env Act where
  runMessage msg = \case
    Trapped' x -> Trapped' <$> runMessage msg x
    TheBarrier' x -> TheBarrier' <$> runMessage msg x
    WhatHaveYouDone' x -> WhatHaveYouDone' <$> runMessage msg x
    UncoveringTheConspiracy' x -> UncoveringTheConspiracy' <$> runMessage msg x
    InvestigatingTheTrail' x -> InvestigatingTheTrail' <$> runMessage msg x
    IntoTheDarkness' x -> IntoTheDarkness' <$> runMessage msg x
    DisruptingTheRitual' x -> DisruptingTheRitual' <$> runMessage msg x
