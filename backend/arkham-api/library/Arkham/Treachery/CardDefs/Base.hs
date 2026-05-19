module Arkham.Treachery.CardDefs.Base where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.CardType
import Arkham.ClassSymbol
import Arkham.EncounterSet hiding (Byakhee, Dunwich, Poison)
import Arkham.Keyword qualified as Keyword
import Arkham.Name
import Arkham.Prelude

baseTreachery
  :: CardCode
  -> Name
  -> Maybe (EncounterSet, Int)
  -> Maybe CardSubType
  -> CardDef
baseTreachery cardCode name mEncounterSet isWeakness =
  (emptyCardDef cardCode name (if isJust isWeakness then PlayerTreacheryType else TreacheryType))
    { cdCardSubType = isWeakness
    , cdClassSymbols = if isJust isWeakness then singleton Neutral else mempty
    , cdEncounterSet = fst <$> mEncounterSet
    , cdEncounterSetQuantity = snd <$> mEncounterSet
    , cdRevelation = IsRevelation
    , cdLevel = Nothing
    }

surge :: CardDef -> CardDef
surge def = def {cdKeywords = insertSet Keyword.Surge (cdKeywords def)}

peril :: CardDef -> CardDef
peril def = def {cdKeywords = insertSet Keyword.Peril (cdKeywords def)}

hidden :: CardDef -> CardDef
hidden def = def {cdKeywords = insertSet Keyword.Hidden (cdKeywords def)}

weakness :: CardCode -> Name -> CardDef
weakness cardCode name = baseTreachery cardCode name Nothing (Just Weakness)

basicWeakness :: CardCode -> Name -> CardDef
basicWeakness cardCode name =
  baseTreachery cardCode name Nothing (Just BasicWeakness)

treachery :: CardCode -> Name -> EncounterSet -> Int -> CardDef
treachery cardCode name encounterSet encounterSetQuantity =
  baseTreachery
    cardCode
    name
    (Just (encounterSet, encounterSetQuantity))
    Nothing
