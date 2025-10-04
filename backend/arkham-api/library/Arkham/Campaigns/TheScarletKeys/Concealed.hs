{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Concealed where

import Arkham.Prelude
import Data.Aeson.TH

data ConcealedCard
  = Decoy
  | AcolyteAny
  | ApportionedKa
  | CityOfRemnantsL
  | CityOfRemnantsM
  | CityOfRemnantsR
  | CoterieAgentA
  | CoterieAgentB
  | CoterieAgentC
  | CoterieAssasinA
  | CoterieAssasinB
  | CoteriaEnforcerA
  | CoteriaEnforcerB
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
  deriving stock (Show, Eq, Ord, Data, Generic)

$(deriveJSON (defaultOptions {tagSingleConstructors = True}) ''ConcealedCard)
