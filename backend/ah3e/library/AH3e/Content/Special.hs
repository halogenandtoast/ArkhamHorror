-- | The special pile: the named cards encounters hand out by name.
module AH3e.Content.Special (cards, focusLimitBonuses) where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids

-- | Cards that raise their holder's focus limit while held (435.11).
focusLimitBonuses :: [(CardCode, Int)]
focusLimitBonuses = [("the-moon", 1), ("deputy-of-arkham", 1)]

special
  :: CardCode -> Text -> AssetType -> [Trait] -> Int -> Maybe Int -> Maybe Int -> Text -> CardDef
special c n ty traits hands health sanity txt =
  CardDef c n CoreSet 1 (AssetCard (AssetDef ty SpecialPile traits Nothing hands health sanity 0 txt))

talent :: CardCode -> Text -> Trait -> Text -> CardDef
talent c n trait = special c n Talent [trait] 0 Nothing Nothing

companion :: CardCode -> Text -> Trait -> Int -> Int -> Text -> CardDef
companion c n trait health sanity = special c n Ally [trait] 0 (Just health) (Just sanity)

cards :: [CardDef]
cards =
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
