module Arkham.Asset.Cards.Promo where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

greenManMedallionHourOfTheHuntress :: CardDef
greenManMedallionHourOfTheHuntress =
  signature "02003"
    $ (asset "98002" ("Green Man Medallion" <:> "Hour of the Huntress") 1 Neutral)
      { cdCardTraits = setFromList [Item, Relic]
      , cdSkills = [#wild, #wild]
      , cdKeywords = setFromList [Keyword.Replacement]
      , cdUnique = True
      , cdSlots = [#accessory]
      }

splitTheAngleIreOfTheVoid :: CardDef
splitTheAngleIreOfTheVoid =
  signature "08004"
    $ (asset "98008" ("Split the Angle" <:> "Ire of the Void") 2 Neutral)
      { cdCardTraits = setFromList [Spell]
      , cdSkills = [#willpower, #intellect, #wild]
      , cdKeywords = setFromList [Keyword.Replacement]
      }

foolishnessFoolishCatOfUlthar :: CardDef
foolishnessFoolishCatOfUlthar =
  signature "05001"
    $ (asset "98011" ("Foolishness" <:> "Foolish Cat of Ulthar") 4 Neutral)
      { cdCardTraits = setFromList [Ally, Creature, Dreamlands]
      , cdSkills = [#wild, #wild]
      , cdKeywords = setFromList [Keyword.Replacement]
      , cdUnique = True
      , cdSlots = [#ally]
      }

mollyMaxwell :: CardDef
mollyMaxwell =
  signature "07004"
    $ (asset "98017" ("Molly Maxwell" <:> "The Exotic Morgana") 3 Neutral)
      { cdCardTraits = setFromList [Ally, Assistant]
      , cdSkills = [#willpower, #agility, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Replacement]
      }

ruthWestmacottDarkRevelations :: CardDef
ruthWestmacottDarkRevelations =
  signature "98019"
    $ (asset "98020" ("Ruth Westmacott" <:> "Dark Revelations") 3 Neutral)
      { cdCardTraits = setFromList [Ally, Artist]
      , cdSkills = [#intellect, #intellect, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      , cdKeywords = setFromList [Keyword.Replacement]
      }
