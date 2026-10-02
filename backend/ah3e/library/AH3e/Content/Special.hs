-- | The special pile: the named cards encounters hand out by name.
module AH3e.Content.Special (cards, focusLimitBonuses) where

import AH3e.Content.Vocabulary (fromBox)
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids

{- | Cards that raise their holder's focus limit while held (435.11). Any pile's
cards may, so a starting possession sits here beside the special pile's.
-}
focusLimitBonuses :: [(CardCode, Int)]
focusLimitBonuses = [("the-moon", 1), ("deputy-of-arkham", 1), ("synergy", 1), ("miles-crown", 1)]

special
  :: CardCode -> Text -> AssetType -> [Trait] -> Int -> Maybe Int -> Maybe Int -> Text -> CardDef
special c n ty traits hands health sanity txt =
  CardDef c n CoreSet 1 (AssetCard (AssetDef ty SpecialPile traits Nothing hands health sanity 0 txt))

talent :: CardCode -> Text -> Trait -> Text -> CardDef
talent c n trait = special c n Talent [trait] 0 Nothing Nothing

companion :: CardCode -> Text -> Trait -> Int -> Int -> Text -> CardDef
companion c n trait health sanity = special c n Ally [trait] 0 (Just health) (Just sanity)

-- | A spell handed out by name, which pays horror to cast like any other.
ritual :: CardCode -> Text -> Trait -> Int -> Int -> Text -> CardDef
ritual c n trait hands horror txt =
  CardDef
    c
    n
    CoreSet
    1
    (AssetCard (AssetDef Spell SpecialPile [trait] Nothing hands Nothing Nothing horror txt))

cards :: [CardDef]
cards =
  core
    <> fromBox DeadOfNight deadOfNight
    <> fromBox UnderDarkWaves underDarkWaves
    <> fromBox SecretsOfTheOrder secretsOfTheOrder

core :: [CardDef]
core =
  [ talent
      "stevedore"
      "Stevedore"
      "Retainer"
      "After you perform a gather resources action in the Merchant District neighborhood, test strength. If you pass, you gain an additional $2."
  , companion
      "stray-cat"
      "Stray Cat"
      "Cat"
      1
      2
      "As part of an evade action, you may discard this card to add two successes to your test result."
  , companion
      "stray-dog"
      "Stray Dog"
      "Dog"
      3
      1
      "After a monster deals damage to you or this ally, this ally deals one damage to that monster."
  , special
      "wooden-homunculus"
      "Wooden Homunculus"
      Item
      ["Magical", "Curio"]
      0
      (Just 3)
      (Just 0)
      "Once per round, as part of an attack action, you may reroll one or all of your dice."
  , talent
      "rare-books-access"
      "Rare Books Access"
      "Membership"
      "After you have an encounter in the Miskatonic University neighborhood, you may discard one tome if you have one. If you do, or if you have no tomes, you gain one tome item."
  , special
      "service-piece"
      "Service Piece"
      Item
      ["Common", "Weapon"]
      1
      Nothing
      Nothing
      "You get +2 strength as part of an attack action."
  , talent
      "reporting-gig"
      "Reporting Gig"
      "Retainer"
      "You get +1 observation as part of a research action. After you gain a clue, you gain $2."
  , talent
      "server-at-velmas"
      "Server at Velma's"
      "Retainer"
      "After you perform a gather resources action in the Eastside neighborhood, test influence. If you pass, you gain an additional $2."
  , companion
      "lita-chantler"
      "Lita Chantler"
      "Zealot"
      3
      3
      "After you or another investigator deals damage to a monster in your space, Lita Chantler deals one damage to that monster."
  , talent
      "performer"
      "Performer"
      "Retainer"
      "After you perform a gather resources action in the Merchant District neighborhood, test influence. If you pass, you gain an additional $2."
  , special "the-moon" "The Moon" Item ["Curio"] 0 (Just 0) (Just 3) "Increase your focus limit by one."
  , special
      "mysterious-serum"
      "Mysterious Serum"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "Action: Discard this card to recover all of your health and sanity and focus one skill of your choice, even if it exceeds your focus limit."
  , companion
      "ezra-graves"
      "Ezra Graves"
      "University Professor"
      3
      2
      "Action: You may spend two remnants and suffer one direct horror to gain one ally."
  , talent
      "innsmouth-look"
      "Innsmouth Look"
      "Innate"
      "When you spend a focus to reroll a die, you may instead reroll dice up to the amount of horror you have suffered. Reckoning-You suffer one horror unless you spend one focus."
  , talent
      "gravedigger"
      "Gravedigger"
      "Retainer"
      "After you perform a gather resources action in the Rivertown neighborhood, test strength. If you pass, you gain an additional $2."
  , companion
      "hunting-dog"
      "Hunting Dog"
      "Dog"
      2
      2
      "Once per round, while resolving a test, you may reroll dice up to the number of clues in your neighborhood."
  , talent
      "dark-blessing"
      "Dark Blessing"
      "Innate"
      "While resolving a test, 4s, 5s, and 6s count as successes. Roll two dice while resolving the reckoning effect of your DARK PACT. You cannot be BLESSED or CURSED. (Discard them.)"
  , companion
      "dayana-esperence"
      "Dayana Esperence"
      "Witch"
      2
      3
      "Once per round, while casting a spell, you may reroll one or all of your dice."
  , talent
      "deputy-of-arkham"
      "Deputy of Arkham"
      "Retainer"
      "You get +1 observation during the encounter phase. Increase your focus limit by one."
  , talent
      "clover-club-member"
      "Clover Club Member"
      "Membership"
      "After you perform a gather resources action in the Downtown neighborhood, you may spend $2 to gamble. If you do, roll one die and gain that much money."
  , companion
      "daniel-chesterfield"
      "Daniel Chesterfield"
      "Asylum Patient"
      3
      1
      "Once per round, you may reroll any number of dice. Then if you do not have a DARK PACT, you gain one."
  , special
      "contraband-whiskey"
      "Contraband Whiskey"
      Item
      ["Common"]
      0
      (Just 0)
      (Just 3)
      "After you perform a gather resources action in the Merchant District or Downtown neighborhoods, you may place one horror on this card to gain $2."
  , special
      "abandoned-luggage"
      "Abandoned Luggage"
      Item
      ["Common"]
      0
      Nothing
      Nothing
      "When you gain this card from the deck, place the top two cards of the item deck facedown under this card. Action: Test observation -1. If you pass, you gain those items and discard this card."
  , special
      "ace-of-rods"
      "Ace of Rods"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "When you spend a focus to reroll a die, you may reroll any number of dice instead."
  , special
      "astrolabe"
      "Astrolabe"
      Item
      ["Magical", "Curio"]
      1
      Nothing
      Nothing
      "As part of a ward action, roll one additional die for each clue you have and one additional die for each clue in your neighborhood."
  , companion
      "black-cat"
      "Black Cat"
      "Cat Familiar"
      1
      3
      "While casting a spell, if this ally suffers one or more horror, you add one success to your test result."
  ]

-- Dead of Night
deadOfNight :: [CardDef]
deadOfNight =
  [ talent
      "armed-backup"
      "Armed Backup"
      "O'Bannion Reputation"
      "Once per round, at the end of the monster phase, you may spend $1 to exhaust one non-epic monster in your space."
  , special
      "black-grimoire"
      "Black Grimoire"
      Item
      ["Curio", "Tome"]
      0
      Nothing
      Nothing
      "Action: Test lore -1. If you pass, gain one spell. If you fail, suffer one horror or place one doom in your space."
  , talent
      "bocce-champion"
      "Bocce Champion"
      "Retainer"
      "After you perform a gather resources action in the Downtown neighborhood, you may test observation. If you pass, gain an additional $3. If you fail, discard this card."
  , talent
      "cleaner"
      "Cleaner"
      "O'Bannion Reputation"
      "When you defeat a Sheldon monster, you may recover one sanity or focus one skill of your choice."
  , special
      "donohues-new-45s"
      "Donohue's New .45s"
      Item
      ["Common", "Weapon"]
      2
      Nothing
      Nothing
      "You get +3 strength as part of an attack action. Once per round, while resolving a test, you may reroll one die."
  , companion
      "friendly-raven"
      "Friendly Raven"
      "Animal"
      2
      1
      "At the start of your turn, you may deal one damage to this ally to exhaust one non-epic monster in your neighborhood."
  , talent
      "good-standing"
      "Good Standing"
      "Arkham Reputation"
      "Once per round, when buying an item, you may test influence. Reduce the value of that item by your test result, to a minimum of $1."
  , special
      "grave-dirt"
      "Grave Dirt"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "While resolving a test, if you are not CURSED, you may suffer one direct horror to change one die to a 6. After resolving the test, become CURSED."
  , talent
      "hired-muscle"
      "Hired Muscle"
      "Sheldon Reputation"
      "Once per round, during your turn, you may spend $1 to deal one damage to a monster in your space."
  , special
      "hypnotists-mirror"
      "Hypnotist's Mirror"
      Item
      ["Common", "Curio"]
      0
      Nothing
      Nothing
      "Once per round, when an ally or investigator in your space recovers sanity, they recover one additional sanity. (You are an investigator in your space.)"
  , talent
      "joey-vigils-supply"
      "Joey Vigil's Supply"
      "O'Bannion Reputation"
      "Increase the size of the display by one card. If JOEY VIGIL'S SUPPLY is discarded, discard the item in the display with the highest value."
  , talent
      "legbreaker"
      "Legbreaker"
      "Sheldon Reputation"
      "When you defeat an O'Bannion monster, you may recover one health or gain $1."
  , companion
      "leo-de-luca"
      "Leo De Luca"
      "The Louisiana Lion"
      3
      2
      "Once per round, as an additional action during your turn, you may perform an action that you have already performed this round."
  , talent
      "library-docent"
      "Library Docent"
      "Retainer"
      "After you perform a gather resources action in the Miskatonic University neighborhood, each investigator in your neighborhood may focus lore."
  , companion
      "maeve-chapman"
      "Maeve Chapman"
      "Nurse"
      2
      2
      "Action: An investigator or ally in your space recovers one health. (You are an investigator in your space.)"
  , special
      "mas-apple-pie"
      "Ma's Apple Pie"
      Item
      ["Common"]
      0
      (Just 0)
      (Just 3)
      "After you perform a gather resources action, you may deal one horror to this item for an investigator or ally in your space to recover one sanity."
  , special
      "mi-go-brain-case"
      "Mi-Go Brain Case"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "As part of a trade action, you may also exchange focus tokens and talents. (No investigator may exceed their focus limit as a result of this trade.)"
  , companion
      "miles-crown"
      "Miles Crown"
      "Victim of the Mi-Go"
      1
      3
      "Increase your focus limit by one. You can focus each skill one additional time."
  , companion
      "peter-sylvestre"
      "Peter Sylvestre"
      "Student Athlete"
      2
      3
      "While performing a test, you can use one additional hand's worth of assets."
  , special
      "puzzle-box"
      "Puzzle Box"
      Item
      ["Curio"]
      0
      (Just 0)
      (Just 2)
      "Action: Test lore -1. If you pass, gain two curios from the deck (not the display) and discard this card."
  , special
      "schoffners-catalogue"
      "Schoffner's Catalogue"
      Item
      ["Common", "Curio"]
      0
      Nothing
      Nothing
      "Action: Buy one common item from the display, increasing its value by $1."
  , talent
      "smuggler-contacts"
      "Smuggler Contacts"
      "Sheldon Reputation"
      "Action: Spend any number of remnants to gain $1 for each remnant spent this way."
  , special
      "the-star"
      "The Star"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "When one or more mythos tokens are added or returned to the mythos cup, you may recover one health or one sanity."
  , special
      "the-world"
      "The World"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "After you perform a move action, if you moved more than two spaces, you may focus one skill of your choice."
  , talent
      "trusted-source"
      "Trusted Source"
      "Police Reputation"
      "Once per round, while resolving a test, if there are one or more clues in your neighborhood, you may reroll one die."
  , talent
      "valued-donor"
      "Valued Donor"
      "Retainer"
      "After you perform a gather resources action in the Southside neighborhood, you may spend one remnant to focus one skill of your choice."
  , special
      "velmas-cherry-pie"
      "Velma's Cherry Pie"
      Item
      ["Common"]
      0
      (Just 3)
      (Just 0)
      "After you perform a gather resources action, you may deal one damage to this item for an investigator or ally in your space to recover one health."
  , special
      "witchweed"
      "Witchweed"
      Item
      ["Curio"]
      0
      (Just 0)
      (Just 3)
      "When this item suffers one or more horror, you may focus one skill of your choice, even if it exceeds your focus limit."
  ]

-- Under Dark Waves
underDarkWaves :: [CardDef]
underDarkWaves =
  [ special
      "death"
      "Death"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "After a card is added to the codex or a card in the codex is flipped, you may remove one doom from any space or spawn one clue."
  , special
      "eben-halls-journal"
      "Eben Hall's Journal"
      Item
      ["Curio", "Tome"]
      1
      Nothing
      Nothing
      "You get +2 lore while casting a spell. After you perform a gather resources action in Kingsport, you may test lore -1. If you pass, you gain one spell."
  , talent
      "eye-for-appraisal"
      "Eye for Appraisal"
      "Kingsport Reputation"
      "Before you would buy or gain one or more curios, you may discard and replace one item from the display."
  , special
      "four-of-cups"
      "Four of Cups"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "Once per round, while performing a test, if you are the only investigator in your neighborhood, you may reroll any number of dice."
  , talent
      "friend-of-a-friend"
      "Friend of a Friend"
      "Kingsport Reputation"
      "After you resolve a street encounter, you may discard an item to gain one item of equal or lesser value from the display."
  , special
      "golden-crown"
      "Golden Crown"
      Item
      ["Magical", "Curio"]
      0
      Nothing
      Nothing
      "You get +2 lore while casting a spell. Reckoning-Suffer one horror unless you place one doom in your space."
  , special
      "harpoon"
      "Harpoon"
      Item
      ["Common", "Weapon"]
      1
      Nothing
      Nothing
      "You get +3 strength as part of an attack action. Before you perform an attack action, you may move a monster in an adjacent space to your space."
  , talent
      "hotel-porter"
      "Hotel Porter"
      "Retainer"
      "After you perform a gather resources action in the Innsmouth Shore neighborhood, you gain an additional $1 for each focus you have."
  , companion
      "inuksuk"
      "Inuksuk"
      "Keeper of Paths"
      3
      3
      "Encounter: Move an unengaged investigator from any space to any space in another neighborhood."
  , special
      "kerosene"
      "Kerosene"
      Item
      ["Common"]
      0
      Nothing
      Nothing
      "When you would gain a remnant, you may instead discard this card to remove one doom from your space and for you or an ally to recover two sanity."
  , companion
      "lonnie-ritter"
      "Lonnie Ritter"
      "Feisty Mechanic"
      3
      3
      "Action: Spend up to $3 and choose an investigator in your space. One of that investigator's items recovers health equal to the amount spent."
  , special
      "lucky-coin"
      "Lucky Coin"
      Item
      ["Common", "Curio"]
      0
      Nothing
      Nothing
      "After you roll a die, you may discard this card to change that die roll to a result of your choice. (You may change any die you roll, even if it is not part of a test.)"
  , special
      "strangers-contract"
      "Stranger's Contract"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "During your turn, you may discard this card and gain a DARK PACT to defeat all non-epic monsters in your space and remove all doom from your space."
  , -- a talent that soaks damage, which the special pile's other talents do not
    special
      "twisted-flesh"
      "Twisted Flesh"
      Talent
      ["Innate"]
      0
      (Just 3)
      (Just 0)
      "When this talent is discarded, draw and resolve two tokens from the mythos cup."
  ]

-- Secrets of the Order
secretsOfTheOrder :: [CardDef]
secretsOfTheOrder =
  [ companion
      "aquinnah"
      "Aquinnah"
      "Forgotten Daughter"
      2
      3
      "When a monster attacks you, you may deal one horror to this ally to prevent all damage and horror dealt to you by that attack and deal one damage to a monster in your space."
  , special
      "blackened-athame"
      "Blackened Athame"
      Item
      ["Magical", "Curio"]
      1
      Nothing
      Nothing
      "While casting a spell, you may suffer one damage to reroll any number of dice."
  , special
      "book-of-shadows"
      "Book of Shadows"
      Item
      ["Curio", "Tome"]
      1
      Nothing
      Nothing
      "Once per round, when you would suffer horror to cast a spell, you may prevent that horror. If you fail the test to cast that spell, suffer that spell's horror twice."
  , talent
      "cabbies-favor"
      "Cabbie's Favor"
      "Arkham Reputation"
      "After you spend at least $1 as part of a move action, you may move one additional space."
  , special
      "chthonian-stone"
      "Chthonian Stone"
      Item
      ["Magical", "Curio"]
      0
      Nothing
      Nothing
      "Once per round, while resolving a test, you may reroll one die or all dice. If you do, place one doom in your space after that test."
  , special
      "cryptic-sketches"
      "Cryptic Sketches"
      Item
      ["Curio", "Tome"]
      0
      Nothing
      Nothing
      "After a non-human monster spawns, you may discard one remnant to focus one skill of your choice."
  , special
      "cyclopean-hammer"
      "Cyclopean Hammer"
      Item
      ["Magical", "Curio", "Weapon"]
      2
      Nothing
      Nothing
      "You get +4 strength as part of an attack action. You may always test strength while performing an attack action. (Modifiers still apply.)"
  , companion
      "david-renfield"
      "David Renfield"
      "Eschatologist"
      1
      3
      "At the end of your turn, you may place one doom in your space for this ally to recover two horror."
  , companion
      "guiding-spirit"
      "Guiding Spirit"
      "Unquiet Dead"
      0
      2
      "Once per round, you may discard the top card of your neighborhood's encounter deck. If you discard an event this way, discard one clue from your neighborhood and spawn two clues."
  , talent
      "hidden-routes"
      "Hidden Routes"
      "Arkham Reputation"
      "Action: Discard one focus token to move directly to any street space."
  , talent
      "inner-sanctum-access"
      "Inner Sanctum Access"
      "Lodge Reputation"
      "After you perform a gather resources action in the French Hill neighborhood, you may remove all doom from any space and place an equal amount of doom in any other space."
  , special
      "lost-journal"
      "Lost Journal"
      Item
      ["Common", "Tome"]
      1
      (Just 0)
      (Just 2)
      "Once per round, after you gain a remnant, this item recovers one sanity."
  , companion
      "michael-leigh"
      "Michael Leigh"
      "Experienced Hunter"
      3
      3
      "Action: Test will. If you pass, exhaust a monster in your space; then you may move that monster one space. You can perform this action while engaged with a monster."
  , special
      "nine-of-rods"
      "Nine of Rods"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "While resolving a test, after you spend a focus token to reroll a die, if you have no focus tokens remaining, roll one additional die."
  , talent
      "steward-of-the-order"
      "Steward of the Order"
      "Lodge Reputation"
      "After you cast a spell, you may spend one remnant to remove one doom from your space."
  , special
      "the-hierophant"
      "The Hierophant"
      Item
      ["Curio"]
      0
      Nothing
      Nothing
      "After you remove one or more doom from your space, if there is no doom remaining in your space, you or an ally may recover one sanity."
  , special
      "the-red-clock"
      "The Red Clock"
      Item
      ["Magical", "Curio"]
      0
      Nothing
      Nothing
      "After you become delayed, you may disengage all monsters and move directly to the unstable space to become DRIVEN. If you do, you are no longer delayed."
  , ritual
      "unknown-liturgy"
      "Unknown Liturgy"
      "Ritual"
      0
      1
      "Action: Place one doom in any space and test lore. An investigator in that space may recover health and sanity, both equal to your test result."
  ]
