{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Concealed.Kind where

import Arkham.Prelude
import Data.Aeson.TH

data ConcealedCardKind
  = Decoy
  | AcolyteAny
  | ApportionedKa
  | CityOfRemnants -- Placeholder for all three parts
  | CityOfRemnantsL
  | CityOfRemnantsM
  | CityOfRemnantsR
  | CoterieEnforcerA
  | CoterieEnforcerB
  | CoterieAgentA
  | CoterieAgentB
  | CoterieAgentC
  | CoterieAssassinA
  | CoterieAssassinB
  | DecoyVoidChimeraEarsplitter
  | DecoyVoidChimeraFellbeak
  | DecoyVoidChimeraFellhound
  | DecoyVoidChimeraGorefeaster
  | DesiderioDelgadoAlvarez
  | EmissaryFromYuggoth
  | LaChicaRoja
  | MimeticNemesis
  | SinisterAspirantA
  | SinisterAspirantB
  | SinisterAspirantC
  | TheRedGlovedMan
  | TzuSanNiang
  | VoidChimeraTrueForm
  | WizardOfTheOrder
  deriving stock (Show, Eq, Ord, Data, Enum, Bounded)

$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''ConcealedCardKind)
