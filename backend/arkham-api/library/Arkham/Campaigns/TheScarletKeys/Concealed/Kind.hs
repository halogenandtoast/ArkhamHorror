module Arkham.Campaigns.TheScarletKeys.Concealed.Kind where

import Arkham.Prelude
import Data.Data (dataTypeConstrs, dataTypeOf, fromConstr, showConstr)

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
  | {- | Open extension point for homebrew content. Do not use directly; each
    homebrew box owns named, bidirectional pattern synonyms over this (see its
    @ConcealedKinds.hs@ and 'Arkham.Homebrew.TH.declareHomebrewConcealedCardKinds').
    The 'Text' tag is the box id then the kind, kebab-cased
    (@":the-phantom-of-the-opera:the-phantom"@): unique across boxes, impossible
    to confuse with a core constructor name, and where the frontend looks for
    the mini-card art.
    -}
    HomebrewConcealedCardKind Text
  deriving stock (Show, Eq, Ord, Data)

{- | Core kinds serialize as their bare constructor name (as the derived
all-nullary encoding did); a 'HomebrewConcealedCardKind' serializes as its tag,
so in-flight Scarlet Keys games round-trip unchanged.
-}
instance ToJSON ConcealedCardKind where
  toJSON = \case
    HomebrewConcealedCardKind t -> toJSON t
    k -> toJSON (tshow k)

instance FromJSON ConcealedCardKind where
  parseJSON = withText "ConcealedCardKind" $ \t ->
    pure $ findWithDefault (HomebrewConcealedCardKind t) t coreConcealedCardKindByName

{- | Every non-homebrew kind. Replaces @[minBound .. maxBound]@ now that
'ConcealedCardKind' carries the open constructor and can no longer derive
'Enum'. For the full set including homebrew, see
@Arkham.Campaigns.TheScarletKeys.Concealed.Helpers.allConcealedCardDefs@.
-}
coreConcealedCardKinds :: [ConcealedCardKind]
coreConcealedCardKinds =
  [ fromConstr con
  | con <- dataTypeConstrs (dataTypeOf (HomebrewConcealedCardKind ""))
  , showConstr con /= "HomebrewConcealedCardKind"
  ]

coreConcealedCardKindByName :: Map Text ConcealedCardKind
coreConcealedCardKindByName = mapFromList [(tshow k, k) | k <- coreConcealedCardKinds]
