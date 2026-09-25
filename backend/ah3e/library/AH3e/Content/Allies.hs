module AH3e.Content.Allies (cards) where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids

ally :: CardCode -> Text -> Trait -> Int -> Int -> Text -> CardDef
ally c n trait health sanity txt =
  CardDef
    c
    n
    CoreSet
    1
    (AssetCard (AssetDef Ally AllyDeck [trait] Nothing 0 (Just health) (Just sanity) 0 txt))

cards :: [CardDef]
cards =
  [ ally "sachiko-higa" "Sachiko Higa" "Pugilist" 4 1 "You get +2 strength as part of an attack action."
  , ally
      "tetsuo-mori"
      "Tetsuo Mori"
      "Police Officer"
      4
      1
      "After you defeat a monster as part of an attack action, you may research one clue."
  , ally
      "zora-larson"
      "Zora Larson"
      "Secretary"
      2
      2
      "Action: An investigator or ally in your space recovers one sanity. (You are an investigator in your space.)"
  , ally "lewis-hayes" "Lewis Hayes" "University Professor" 1 4 "You get +2 lore while casting a spell."
  , ally
      "leland-williams"
      "Leland Williams"
      "Antiquities Dealer"
      3
      2
      "After you gain this card from the deck, gain one curio item."
  , ally
      "jenica-capra"
      "Jenica Capra"
      "Mystic Bounty Hunter"
      2
      3
      "After you defeat a monster as part of an attack action, you may remove one doom from your space."
  , ally
      "henry-wan"
      "Henry Wan"
      "Aspiring Actor"
      2
      3
      "Once per round, you may pay half price (rounded up) while buying an item. (Does not stack with other cost reductions.)"
  , ally
      "grace-bechman"
      "Grace Bechman"
      "Paranormal Detective"
      2
      3
      "You get +2 lore as part of a ward action."
  , ally
      "gabriel-carillo"
      "Gabriel Carillo"
      "Teacher"
      2
      1
      "You may perform one additional action during your turn."
  , ally
      "delphinia-bell"
      "Delphinia Bell"
      "University Professor"
      1
      4
      "Action: You may spend one remnant to focus one skill of your choice, even if it exceeds your focus limit."
  , ally
      "arthur-johnson"
      "Arthur Johnson"
      "Consultant"
      3
      3
      "Once per round, you may reroll one die while resolving a test."
  , ally
      "alice-luxley"
      "Alice Luxley"
      "Police Detective"
      3
      2
      "Once per round, while resolving a test, you may roll one additional die for each clue you have and one additional die for each clue in your neighborhood."
  ]
