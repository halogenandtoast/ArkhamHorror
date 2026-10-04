{- | Consternation on the Constellation's assets.

Five physical cards share one printed front, "Crate of Goods", and differ only
on the back. Setup shuffles the Tablet of Dagon together with two of the other
four and removes the rest, so which crate holds what is hidden until a crate is
opened. The fronts therefore need five separate card codes -- one per back --
even though they are identical to read.

Four backs are assets; the fifth, Hand of the Strangler, is an enemy and lives
in @CardDefs.Enemies@. All five keep the encounter back of their front side.

Inspector Legrasse is the scenario's reward and is printed on a *player* back:
"Additional Rewards" lets one investigator add him to their deck, which an
encounter-backed card could not survive.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Card.CardCode (CardCode)
import Arkham.Homebrew.ConsternationOnTheConstellation.Sets qualified as Set
import Arkham.Homebrew.ConsternationOnTheConstellation.Traits

-- * The five crates

{- | The shared front. Attached to a location, it takes clues until it holds one
per investigator, then flips and is drawn.
-}
crateOfGoods :: CardCode -> CardCode -> CardDef
crateOfGoods cardCode backCardCode =
  (encounterAsset_ cardCode "Crate of Goods" Set.ConsternationOnTheConstellation)
    { cdCardTraits = singleton Item
    , cdOtherSide = Just backCardCode
    }

crateOfGoodsCrimsonLedger :: CardDef
crateOfGoodsCrimsonLedger =
  crateOfGoods ":consternation-on-the-constellation:010" ":consternation-on-the-constellation:010b"

crateOfGoodsHandOfTheStrangler :: CardDef
crateOfGoodsHandOfTheStrangler =
  crateOfGoods ":consternation-on-the-constellation:011" ":consternation-on-the-constellation:011b"

crateOfGoodsAbyssalSword :: CardDef
crateOfGoodsAbyssalSword =
  crateOfGoods ":consternation-on-the-constellation:012" ":consternation-on-the-constellation:012b"

crateOfGoodsRingOfTheDeep :: CardDef
crateOfGoodsRingOfTheDeep =
  crateOfGoods ":consternation-on-the-constellation:013" ":consternation-on-the-constellation:013b"

crateOfGoodsTabletOfDagon :: CardDef
crateOfGoodsTabletOfDagon =
  crateOfGoods ":consternation-on-the-constellation:014" ":consternation-on-the-constellation:014b"

-- * What is inside them

crimsonLedger :: CardDef
crimsonLedger =
  ( encounterAsset_
      ":consternation-on-the-constellation:010b"
      "Crimson Ledger"
      Set.ConsternationOnTheConstellation
  )
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdUnique = True
    , cdOtherSide = Just ":consternation-on-the-constellation:010"
    }

abyssalSword :: CardDef
abyssalSword =
  ( encounterAsset_
      ":consternation-on-the-constellation:012b"
      "Abyssal Sword"
      Set.ConsternationOnTheConstellation
  )
    { cdCardTraits = setFromList [Item, Relic, Weapon, Melee]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdOtherSide = Just ":consternation-on-the-constellation:012"
    }

ringOfTheDeep :: CardDef
ringOfTheDeep =
  ( encounterAsset_
      ":consternation-on-the-constellation:013b"
      "Ring of the Deep"
      Set.ConsternationOnTheConstellation
  )
    { cdCardTraits = setFromList [Item, Relic, Cursed]
    , cdSlots = [#accessory]
    , cdUnique = True
    , cdOtherSide = Just ":consternation-on-the-constellation:013"
    }

-- | The one the cult is looking for. Both act 2 and agenda 2 turn on who holds it.
tabletOfDagon :: CardDef
tabletOfDagon =
  ( encounterAsset_
      ":consternation-on-the-constellation:014b"
      "Tablet of Dagon"
      Set.ConsternationOnTheConstellation
  )
    { cdCardTraits = setFromList [Item, Relic]
    , cdUnique = True
    , cdOtherSide = Just ":consternation-on-the-constellation:014"
    }

-- * The reward

inspectorLegrasse :: CardDef
inspectorLegrasse =
  ( storyAsset
      ":consternation-on-the-constellation:038"
      "Inspector Legrasse"
      3
      Set.ConsternationOnTheConstellation
  )
    { cdCardTraits = setFromList [Ally, Retired, Detective]
    , cdSkills = [#intellect, #intellect, #combat, #wild]
    , cdSlots = [#ally]
    , cdDeckRestrictions = [PerDeckLimit 1]
    }
