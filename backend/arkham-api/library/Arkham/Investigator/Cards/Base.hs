{- | The investigator card-def builders, split out of 'Arkham.Investigator.Cards'
so a card definition can be written without importing the aggregate table.

'Arkham.Investigator.Cards.allInvestigatorCards' folds in
'Arkham.Homebrew.Defs.investigatorsMap', so a homebrew @CardDefs/Investigators.hs@
that imported the aggregate would close an import cycle through its own campaign.
It imports this module instead — the same split 'Arkham.Asset.Cards.Base' makes
for assets.
-}
module Arkham.Investigator.Cards.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.Name
import Arkham.Prelude
import Arkham.Trait (Trait)

investigator :: CardCode -> Name -> ClassSymbol -> [Trait] -> CardDef
investigator cardCode name classSymbol traits =
  (emptyCardDef cardCode name InvestigatorType)
    { cdClassSymbols = singleton classSymbol
    , cdCardTraits = setFromList traits
    , cdUnique = True
    , cdLevel = Nothing
    }

withAlternate :: CardCode -> CardDef -> CardDef
withAlternate ccode = withAlternates [ccode]

withAlternates :: [CardCode] -> CardDef -> CardDef
withAlternates ccodes def = def {cdAlternateCardCodes = ccodes}
