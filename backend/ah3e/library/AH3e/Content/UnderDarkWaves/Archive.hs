{- | Under Dark Waves' archive cards (61-120). An archive card is pure text: the
side showing and, for the ones that turn over, what is on the back. What each one
actually /does/ lives in its scenario's codex behaviours, so these are the cards
themselves and nothing more.

Card 61 is shared by every terror scenario; the rest belong to one of the box's
four scenarios.
-}
module AH3e.Content.UnderDarkWaves.Archive (cards) where

import AH3e.Content.Vocabulary (fromBox)
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n name front back =
  CardDef
    (CardCode ("archive-" <> tshow n))
    name
    CoreSet
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

{- | An artifact: an archive card that is gained as an item rather than added to
the codex (Under Dark Waves, p. 7). It is an item in every respect, and goes back
to the archive rather than to the item deck when discarded.
-}
artifact :: Int -> Text -> [Trait] -> Text -> CardDef
artifact n name traits txt =
  CardDef
    (CardCode ("archive-" <> tshow n))
    name
    CoreSet
    1
    (AssetCard (AssetDef Item Archive ("Artifact" : traits) Nothing 0 Nothing Nothing 0 txt))

{- | An epic monster that takes an archive number of its own, the way Umordhoth
takes card 19: it is a monster card, not an archive card, and nothing else stands
for that number.
-}
epic
  :: Int
  -> Text
  -> [Trait]
  -> Int
  -> SpaceRule
  -> Activation
  -> (Int, Int)
  -> (Int, Int)
  -> (Int, Int)
  -> [Keyword]
  -> Text
  -> CardDef
epic n name traits speed spawn activation (health, elite) (atk, evade) (damage, horror) keywords text =
  CardDef
    (CardCode ("archive-" <> tshow n))
    name
    CoreSet
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn
          , activation
          , speed
          , traits
          , health
          , elite
          , attackSkill = Strength
          , attackModifier = atk
          , evadeModifier = evade
          , damage
          , horror
          , remnant = False
          , keywords
          , epic = True
          , text
          }
    )

cards :: [CardDef]
cards = fromBox UnderDarkWaves archive

archive :: [CardDef]
archive =
  [ archiveCard
      61
      "Terror"
      "When a neighborhood has six or more doom, remove all doom from one space in that neighborhood and place one doom on the scenario sheet. Then spread terror in that neighborhood.\nBefore you resolve an encounter in a neighborhood with one or more attached terror cards, encounter one of those cards.\nAction: You gather support and organize resistance to the threat (influence). For each success you roll, you may discard one terror token or terror card from your neighborhood."
      Nothing
  , archiveCard
      62
      "Caught in the Act"
      "When there is four or more doom on the scenario sheet, flip this card."
      ( Just
          "Siren Call\nWhen there is eight or more doom on the scenario sheet, add card 73 to the codex and return this card to the archive.\nReckoning—Place one doom in any space in each neighborhood without a lead. (White markers are leads.)"
      )
  , archiveCard
      63
      "Cursed Gold"
      "When there are two or more clues on the scenario sheet, flip this card."
      ( Just
          "Create the investigation deck using cards 68-71. Shuffle the investigation deck.\nAdd card 64 to the codex and return this card to the archive."
      )
  , archiveCard
      64
      "Find the Source"
      "White markers are \"leads.\" (Facedown white markers are still considered leads.)\nWhen an investigator researches a clue, instead of placing it on the scenario sheet, they may discard that clue to place a lead faceup in the neighborhood that has the most doom and does not have a lead.\nEncounter: You may flip a lead in your neighborhood facedown to reveal the top card of the investigation deck. If it is an artifact, you gain it; otherwise, add it to the codex.\nAfter a Deep One relic is revealed, if card 65 is in the archive, add card 65 to the codex.\nAfter three Deep One relics have been revealed, flip this card (even if one or more relics have been discarded)."
      ( Just
          "The Panoply of Y'ha-nthlei\nDiscard all leads (white markers) and return the rest of the investigation deck to the archive.\nRemove one spawn monster token from the mythos cup and add one blank token to the mythos cup.\nReckoning—Remove one doom from any space."
      )
  , archiveCard
      65
      "Against the Deep"
      "Choose one:\n- Any investigator discards one Deep One relic to take card 74 (Father Dagon epic monster), spawn it at Devil Reef, and add card 66 to the codex.\n- Any investigator discards one Deep One relic to take card 75 (Mother Hydra epic monster), spawn it at Devil Reef, and add card 67 to the codex.\n- Return this card to the archive. The investigators may only choose this option if they have fewer than three Deep One relics."
      (Just "The investigators win the game!")
  , archiveCard
      66
      "Father Dagon"
      "When Father Dagon is defeated, instead of returning card 74 to the archive, place it under this card.\nAfter you damage Father Dagon, you may spend any number of clues from the scenario sheet to deal that much additional damage.\nReckoning—If Father Dagon has been defeated, flip this card."
      ( Just
          "Hydra's Fury\nTake card 75 (Mother Hydra epic monster) and spawn it at Devil Reef.\nReckoning—Mother Hydra deals one damage and one horror to each investigator unless the investigators spend a clue from the scenario sheet.\nWhen Mother Hydra is defeated, flip card 65."
      )
  , archiveCard
      67
      "Mother Hydra"
      "When Mother Hydra is defeated, instead of returning card 75 to the archive, place it under this card.\nAfter you damage Mother Hydra, you may spend any number of clues from the scenario sheet to deal that much additional damage.\nReckoning—If Mother Hydra has been defeated, flip this card."
      ( Just
          "Dagon's Rage\nTake card 74 (Father Dagon epic monster) and spawn it at Devil Reef.\nReckoning—Spawn one Deep One monster in Father Dagon's space unless the investigators spend a clue from the scenario sheet.\nWhen Father Dagon is defeated, flip card 65."
      )
  , artifact
      68
      "Headdress of Y'ha-nthlei"
      ["Deep One Relic"]
      "At the start of your turn, you may place one focus token on this card. Each focus token on this card increases your corresponding skill by one. (You may not spend them.)\nReckoning—Discard all of your focus tokens that match the tokens on this card. Then discard all tokens on this card."
  , artifact
      69
      "Waveworn Idol"
      ["Deep One Relic"]
      "Once per round, after a monster attacks you, you may recover one health or one sanity.\nReckoning—Choose one:\n- You suffer one direct damage and recover two sanity.\n- You suffer one direct horror and recover two health."
  , artifact
      70
      "Awakened Mantle"
      ["Deep One Relic"]
      "Increase each of your skills by one.\nReckoning—You suffer one direct horror and one direct damage."
  , archiveCard
      71
      ""
      "Spawn a Deep One monster in your space.\nThen place one lead faceup in the neighborhood that has the most doom and does not contain a lead, if possible, and return this card to the archive."
      Nothing
  , archiveCard
      72
      "Act of Desperation"
      "Reckoning—Move one damage from the epic monster that has suffered the most damage to another epic monster in any space. Repeat this effect until all epic monsters have suffered the same amount of damage, if possible.\nAfter both Mother Hydra and Father Dagon are defeated, flip this card and read \"Bloody Victory.\""
      ( Just
          "Bloody Victory\nThe investigators win the game!\n\nThey Feed; They Rise\nThe investigators lose the game!"
      )
  , archiveCard
      73
      ""
      "Take card 74 (Father Dagon epic monster) and card 75 (Mother Hydra epic monster) from the archive and spawn them both at Devil Reef.\nIf either epic monster is not in the archive, do not spawn it.\nIf either epic monster is already in play, it recovers health equal to the number of investigators.\nReturn all other non-rumor cards in the codex except 61 to the archive.\nAdd card 72 to the codex. Then flip this card."
      ( Just
          "Rampage!\nWhen there is twelve or more doom on the scenario sheet, flip card 72 and read \"They Feed; They Rise.\"\nReckoning—Place one doom on the scenario sheet unless the investigators spend a clue from the scenario sheet."
      )
  , epic
      74
      "Father Dagon"
      ["Deep One Herald"]
      0
      (CustomSpaceRule "Spawned at Devil Reef by card 65, 67 or 73")
      (Lurker (Custom "father-dagon-lurk"))
      (4, 3)
      (-1, 0)
      (1, 2)
      [Massive, Retaliate]
      "Massive, Lurker—Each Deep One monster recovers two health.\nElite 3 (Has 3 additional health per investigator.)\nRetaliate (After you perform an attack action, if you dealt no damage to Dagon, he attacks you.)\nAfter you disengage this monster, spawn one monster in its space (after you perform your additional action from a successful evade action)."
  , epic
      75
      "Mother Hydra"
      ["Deep One Herald"]
      1
      (CustomSpaceRule "Spawned at Devil Reef by card 65, 66 or 73")
      (Hunter (HighestSkill Lore))
      (4, 3)
      (-1, 0)
      (1, 2)
      [Pursuit, Massive]
      "Pursuit, Massive. Hunter—Move toward and engage highest lore.\nElite 3 (Has 3 additional health per investigator.)\nPursuit (After Hydra is dealt damage by an investigator in another space, she moves her speed toward that investigator.)\nAfter you become engaged with this monster, test will. If you fail, suffer one damage and one horror."
  , archiveCard
      76
      "A Missing Client"
      "When there are two or more clues on the scenario sheet, flip this card.\nWhen there is four or more doom on the scenario sheet, add card 84 to the codex and return this card to the archive."
      ( Just
          "Choose one:\n- To infiltrate the Lantern Club by posing as a wealthy new member, add card 77 to the codex.\n- To find and interrogate one of the Society's officers, add card 78 to the codex.\nThen add card 84 to the codex, \"Behind the Mask\" side up, and return this card to the archive."
      )
  , archiveCard
      77
      "Infiltration"
      "Encounter: You attempt to garner an invitation to the Club (influence). You may spend any amount of money to add one success to your roll for each dollar spent this way. If your test result is six or more, flip this card. Resolve this encounter ability only at the Hall School."
      ( Just
          "Membership Perks\nAction: Spend two clues from the scenario sheet and resolve a gate burst to add card 79 to the codex \"Inner Workings\" side up and add card 82 to the codex. Then return this card to the archive.\nReckoning—Each investigator tests will. Each investigator that fails places one doom in their space."
      )
  , archiveCard
      78
      "Interrogation"
      "Encounter: You may spread doom once to test observation -1. If you pass, spend two clues from the scenario sheet to flip this card. Resolve this encounter only at the unstable space."
      ( Just
          "Catch and Release\nSpawn the set-aside Declan Pearce monster in the unstable space.\nWhen Declan Pearce is defeated, return him to the game box, add card 79 to the codex \"Society Secrets\" side up, and add card 82 to the codex. Then return this card to the archive.\nReckoning—Each investigator tests observation. Each investigator that fails places one doom in their space."
      )
  , -- both sides of 79 are numbered and titled; a card adds it one side up or the other
    archiveCard
      79
      "Inner Workings"
      "Investigators may use influence instead of lore as part of a ward action.\nReckoning—Each investigator tests will -1. Each investigator that fails gains $1 and places one doom in their space."
      ( Just
          "Society Secrets\nEncounter: You may remove one doom from any space in your neighborhood.\nReckoning—Each investigator tests observation. Each investigator that fails places one doom in their space."
      )
  , archiveCard
      80
      "Find the Trail"
      "Action: You may spend four clues from the scenario sheet to flip this card. Perform this action only at the unstable space."
      ( Just
          "Take one blue marker, one green marker, and three white markers, randomize them, and place one facedown in each neighborhood, in the space with the most doom.\nThen add card 81 to the codex and return this card to the archive."
      )
  , archiveCard
      81
      "Find the Truth"
      "Encounter: Reveal and discard a marker in your space to resolve the effect based on that marker's color. Resolve this encounter ability only in a space with an unrevealed marker.\nWhen you reveal a white marker, you find nothing conclusive when you search the area; resolve an encounter in your space as normal.\nWhen you reveal the green marker, you find several Society shipping manifests; you may draw up to two tokens from the mythos cup to spawn an equal number of clues.\nWhen you reveal the blue marker, you catch several Club members red-handed. Flip this card and discard all remaining unrevealed markers."
      ( Just
          "Alien Masters\nSpawn one moon-beast monster in your space.\nThen add card 82 to the codex. (Do not remove this card from the codex.)\nReckoning—Each investigator tests lore. Each investigator that fails places one doom in their space."
      )
  , archiveCard
      82
      "Steal the Lantern"
      "Action: Test observation -1 to attempt to steal the lantern. As part of this test, investigators in your space may spend any number of focus or clues to add an equal number of successes. If your test result is four or more, flip this card. Perform this action only at the Hall School."
      ( Just
          "An investigator at the Hall School gains card 90 (the Pale Lantern artifact).\nTake card 89 (The Bloodless Man epic monster) and spawn it at the Hall School.\nAdd card 83 to the codex and return this card to the archive."
      )
  , archiveCard
      83
      "Sunder the Lantern"
      "The Bloodless Man's text box is blank while this card is in the codex (including keywords and ability text, but not activation text).\nWhen the Bloodless Man epic monster is defeated, flip this card.\nAction: Test lore -2 to move one clue from the scenario sheet to the Pale Lantern. (This will help you win the game.) Perform this action only in a space with the Pale Lantern."
      ( Just
          "Before He Returns\nWhen there are three clues on the Pale Lantern, flip card 90 and read the \"A Broken Mask\" effect.\nAction: Test lore to move one clue from the scenario sheet to the Pale Lantern. Perform this action only in a space with the Pale Lantern.\nReckoning—Take card 89 (The Bloodless Man epic monster) and spawn it in the unstable space. Then place one doom on the scenario sheet and flip this card."
      )
  , archiveCard
      84
      "The Lost One"
      "When there are two or more clues on the scenario sheet, flip this card and add card 80 to the codex. (Do not discard the clues.)\nWhen there is eight or more doom on the scenario sheet, add card 85 to the codex. (Do not remove this card from the codex.)"
      ( Just
          "Behind the Mask\nIf cards 85 or 86 are in the codex, return this card to the archive. Otherwise, each investigator may focus one skill of their choice, even if it exceeds their focus limit.\nWhen there is eight or more doom on the scenario sheet, add card 85 to the codex and return this card to the archive."
      )
  , archiveCard
      85
      "Creeping Threat"
      "Add two spread doom tokens to the mythos cup.\nWhen there is twelve or more doom on the scenario sheet, flip this card."
      ( Just
          "Take card 89 (The Bloodless Man epic monster) and spawn it at the Hall School. If it is already in play, it moves directly to the investigator with the Pale Lantern artifact.\nReturn cards 77-84 to the archive and defeat all non-epic monsters.\nDeal the Bloodless Man four damage for each clue on card 90, then return card 90 to the archive.\nAdd card 86 to the codex and return this card to the archive."
      )
  , archiveCard
      86
      "Waning Hope"
      "After the Bloodless Man is defeated, flip this card and read the \"Many Lost Souls\" effect.\nWhen there is sixteen or more doom on the scenario sheet, flip this card and read the \"Taken Away\" effect.\nReckoning—The Bloodless Man deals one damage and one horror to each investigator in his neighborhood. Then he disengages all investigators and moves directly to the unstable space unless the investigators spend a clue from the scenario sheet."
      ( Just
          "Many Lost Souls\nThe investigators win the game!\n\nTaken Away\nThe investigators lose the game!"
      )
  , archiveCard
      87
      "A Storm Rages"
      "Place three doom tokens on this card.\nAfter you pass a test as part of an encounter at the Strange High House, replace one doom token on this card with a clue token.\nReckoning—Remove one doom from this card. Then if there is no doom on this card, flip it."
      ( Just
          "Beacon on the Rock\nFor each clue on this card, place a focus token of your choice on this card. Then discard those clues.\nAfter an investigator resolves an encounter at the Strange High House, they may choose and test a skill. If they pass, they may place a focus token matching that skill on this card.\nWhen this card has three or more different focus tokens on it, add card 88 to the codex. Then return this card to the archive."
      )
  , archiveCard
      88
      ""
      "Place two clues on the scenario sheet.\nThen choose one:\n- One investigator may retire for each other investigator to become BLESSED or recover one health and one sanity. Then return this card to the archive.\n- Flip this card."
      ( Just
          "The Aid of Nodens\nReckoning—One investigator may suffer horror equal to the number of horror tokens on this card to remove all doom from any space and return all blank tokens to the mythos cup. If any investigator does, place one horror token on this card."
      )
  , epic
      89
      "The Bloodless Man"
      ["Aspect"]
      0
      (CustomSpaceRule "Revealed from the archive by card 82, 83 or 85")
      (Lurker (Seq [PlaceDoomAt SourceSpace (N 1), PlaceDoomAt TheUnstableSpace (N 1)]))
      (4, 4)
      (-2, 0)
      (2, 2)
      [Massive]
      "Lurker—Place one doom in this space. Then place one doom in the unstable space.\nElite 4 (This monster has four additional health per investigator.)\nMassive (This monster engages and attacks each investigator in his space. He cannot be exhausted.)\nAfter the Bloodless Man damages you, either suffer one horror or deal one damage to another investigator in your space."
  , {- 90 is an artifact with a text back, which 'AssetDef' has no room for. Its
    reverse reads:

    A Broken Mask
    The investigators win the game!

    Dying Light
    The defeated investigator tests will. Roll one additional die during this test
    for each clue on this card. If you pass, the nearest investigator gains this
    card faceup (if there is no other investigator, gain this card at the start of
    your next turn). If you fail, add doom to the scenario sheet until there is
    eight doom on that sheet and return this card to the archive. Either way, you
    are devoured. -}
    artifact
      90
      "The Pale Lantern"
      ["Magical", "Curio"]
      "Once per phase, before resolving a test, you may place one doom in your space to roll two additional dice.\nWhile you have the Pale Lantern, your space is the unstable space, instead of the normal unstable space.\nWhen you would be defeated (for any reason), flip this card and read the \"Dying Light\" effect. (Do not discard any clues on this card.)"
  , archiveCard
      91
      "The Hunter's Keen"
      "Spawn card 104 (the wendigo epic monster) at the unstable space (after you spread starting doom.)\nFlip this card."
      ( Just
          "Cold, Cruel Hunger\nAdd card 92 to the codex.\nWhen there is four or more doom on the scenario sheet, add card 103 to the codex and return this card to the archive."
      )
  , archiveCard
      92
      "Friend or Foe?"
      "When the wendigo epic monster is defeated, flip this card.\nAction: You may take any number of clues from the scenario sheet and place them in your play area. Deal two damage to the wendigo epic monster for each clue you take this way."
      ( Just
          "Choose an investigator to gain INUKSUK.\nTake cards 93-95, select two of them at random, and add them to the codex. Return the other card to the archive.\nReturn this card to the archive."
      )
  , archiveCard
      93
      "Bent to His Will"
      "After you defeat a thrall monster in a location with no marker, you may spend one clue from the scenario sheet to place one green marker faceup in your space. (Streets are not locations.)\nWhen there are three faceup green markers on the board, return cards 94 and 95 to the codex and flip this card."
      ( Just
          "Move all white and blue markers on the board to the scenario sheet. Add one blank token to the mythos cup for each marker moved in this way.\nThen choose one:\n- You may attempt to summon and battle Ithaqua before it can regain its full strength; add card 99 to the codex.\n- If there are any blue markers on the scenario sheet, you may attempt to cleanse the victims of Ithaqua's influence; add card 96 to the codex.\n- If there are any white markers on the scenario sheet, you may attempt to seal Ithaqua under the polar ice; add card 98 to the codex.\nThen return this card to the archive."
      )
  , archiveCard
      94
      "Find the Heart"
      "Action: Spend one clue from the scenario sheet to place a blue marker facedown in any location in a neighborhood with no investigators and no marker.\nAfter you resolve a non-terror encounter in a space with a facedown marker, flip that marker.\nWhen there are three faceup blue markers on the board, return cards 93 and 95 to the codex and flip this card."
      ( Just
          "Move all green and white markers on the board to the scenario sheet. Add one blank token to the mythos cup for each marker moved in this way.\nThen choose one:\n- You may attempt to shatter the heart of ice; add card 100 to the codex.\n- If there are any green markers on the scenario sheet, you may attempt to cleanse the victims of Ithaqua's influence; add card 96 to the codex.\n- If there are any white markers on the scenario sheet, you may attempt to banish Ithaqua to another world; add card 97 to the codex.\nThen return this card to the archive."
      )
  , archiveCard
      95
      "Countered Magic"
      "Action: Test lore -1. If you succeed, spend one clue from the scenario sheet to place one white marker faceup in your space. Perform this action only in a neighborhood that does not contain a white marker.\nIf one neighborhood in each of Innsmouth, Kingsport, and Arkham has a faceup white marker, return cards 93 and 94 to the codex and flip this card."
      ( Just
          "Move all blue and green markers on the board to the scenario sheet. Add one blank token to the mythos cup for each marker moved in this way.\nThen choose one:\n- You may attempt to erect an eternal ward around the region; add card 101 to the codex.\n- If there are any blue markers on the scenario sheet, you may attempt to banish Ithaqua to another world; add card 97 to the codex.\n- If there are any green markers on the scenario sheet, you may attempt to seal Ithaqua under the polar ice; add card 98 to the codex.\nThen return this card to the archive."
      )
  , archiveCard
      96
      "Cleanse the Victims"
      "Action: Test lore +1. Roll one fewer die on this test for each terror in your neighborhood. If you pass, spend one clue from the scenario sheet or spread terror in another neighborhood with a faceup marker to flip a marker in your space facedown.\nWhen all markers have been flipped facedown, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      97
      "Banish Ithaqua"
      "Action: Test will -1. If you pass, spend one clue from the scenario sheet or become delayed to flip a marker in your space facedown. Perform this action only if there is no doom in your space.\nWhen all markers have been flipped facedown, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      98
      "Seal Ithaqua Away"
      "Encounter: Test lore -1. If you pass, spend one clue from the scenario sheet or place three doom in your space to flip a marker in your space facedown.\nWhen all markers have been flipped facedown, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      99
      "Vanquish Ithaqua"
      "Action: Take card 105 (Ithaqua epic monster) and spawn it in your space.\nEncounter: Flip a green marker in your space facedown and spend any number of clues from the scenario sheet to deal an equal amount of damage to Ithaqua.\nAfter Ithaqua is defeated, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      100
      "Shatter the Stone"
      "Move all markers on the board to the space with the most doom. This space contains \"the Heart of Ice.\"\nEncounter: Draw two tokens from the mythos cup to flip one marker in your space facedown. You may spend up to two clues from the scenario sheet instead of drawing an equal number of those tokens. Then suffer one damage or one horror for each doom in your neighborhood.\nWhen all markers are facedown, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      101
      "Ward the Valley"
      "Action: Discard a spell or spend one clue from the scenario sheet to place a white marker in your space. Perform this action only in a neighborhood with no marker.\nWhen each neighborhood has a white marker, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      102
      "The Last Stand"
      "Take card 105 (Ithaqua epic monster) and spawn it at the unstable space. If it is already in play, it recovers two health per investigator. Return cards 93-101 to the codex.\nIthaqua's health is reduced by two for each clue on the scenario sheet.\nWhen Ithaqua is defeated, flip this card and read the \"No Other Way\" effect.\nWhen there is fifteen or more doom on the scenario sheet, flip this card and read the \"It's All Too Much\" effect."
      ( Just
          "No Other Way\nThe investigators win the game!\n\nIt's All Too Much\nThe investigators lose the game!"
      )
  , archiveCard
      103
      "Endless Winter"
      "Add one gate burst token and one spread terror token to the mythos cup.\nWhen there is ten or more doom on the scenario sheet, discard all markers from play and any number of clues from among the scenario sheet and all investigators. If a total of five clues and/or markers were discarded this way, add card 102 to the codex and return this card to the archive. Otherwise, flip this card."
      (Just "The investigators lose the game!")
  , epic
      104
      "Wendigo"
      ["Spirit Thrall"]
      0
      (CustomSpaceRule "Spawned at the unstable space by card 91")
      (Lurker (Custom "wendigo-lurk"))
      (4, 2)
      (-1, -2)
      (1, 2)
      [Feed, Retaliate]
      "Lurker—Spread terror in this neighborhood.\nElite 2 (This monster has two additional health per investigator.)\nFeed (After this monster deals damage to an investigator or ally, it recovers that much health.)\nRetaliate (After you perform an attack action, if you did not damage this monster, it attacks you.)"
  , -- the card prints "Elite 4" but its reminder text says two per investigator; the keyword governs
    epic
      105
      "Ithaqua"
      ["Ancient One"]
      2
      (CustomSpaceRule "Spawned by card 99 or 102")
      (Hunter (LowestSkill Observation))
      (8, 4)
      (-3, -1)
      (3, 2)
      [Massive]
      "Hunter—Move toward and engage lowest observation.\nElite 4\nMassive (Ithaqua engages and attacks each investigator in its space. It cannot be exhausted.)\nAfter this monster attacks you, become TAINTED. If you cannot, place one doom in your space."
  , archiveCard
      106
      "Song of Chaos"
      "When there are two or more clues on the scenario sheet, flip this card."
      ( Just
          "Agents of Madness\nPlace one white marker in the space with the most doom in each neighborhood that does not contain a white marker.\nAction: Discard a white marker from your space to reveal a random card from the investigation deck and return that card to the archive. If you do, you may spend one clue from the scenario sheet to add one blank token to the mythos cup.\nWhen there is only one card in the investigation deck, add that card to the codex. Then return this card and card 107 to the archive."
      )
  , archiveCard
      107
      "Maddening Melody"
      "Each time doom is placed on the scenario sheet, place one white marker on the space with the most doom in a neighborhood that does not contain a white marker.\nEncounter: You may suffer one direct horror to research a clue. This ability may only be performed by an investigator in a space with a white marker.\nWhen there is four or more doom on the scenario sheet, flip this card."
      ( Just
          "Add one card at random from the investigation deck to the codex and return the others to the archive.\nDiscard two clues from the scenario sheet.\nThen return this card and card 106 to the codex."
      )
  , archiveCard
      108
      "The Cult Revealed"
      "When there are two or more clues on the scenario sheet, discard two clues from the scenario sheet and flip this card."
      ( Just
          "Take cards 109-112 from the archive, shuffle them, and randomly add one of them to the codex. Return the other cards to the archive.\nThen return this card to the archive."
      )
  , archiveCard
      109
      "Cut Off the Head"
      "Action: Take card 40 (Cthulhu epic monster) and spawn it at your location. This action can only be performed at the ritual site.\nEach time a clue would be added to the scenario sheet, instead deal four damage to the Cthulhu epic monster. (It cannot suffer damage before it spawns.)\nAfter the Cthulhu epic monster has been defeated, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      110
      "Call for Help"
      "Action: You attempt to call in the Feds (influence -2). If your test result is five or more, flip this card. As part of this test, you may spend any number of clues from the scenario sheet to add a number of successes to your test result equal to the number of clues spent. This action may only be performed at the cultist shrine."
      (Just "The investigators win the game!")
  , archiveCard
      111
      "Endless Song"
      "Action: Spend one clue from the scenario sheet to place a white marker in your space. You may only perform this action if there is no doom in your space.\nWhen all spaces in the neighborhood with the ritual site have a white marker, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      112
      "Raze the Shrine"
      "Action: Spend two clues from the scenario sheet and return all tokens to the mythos cup to place a facedown marker in your space. This is the bomb. This action may only be performed at the cultist shrine.\nAll monsters consider the bomb to be their prey and destination and engage it and attack it as if it is an investigator. If the bomb suffers any damage from a monster in its space, it is discarded.\nReckoning—If the bomb is at the cultist shrine, flip this card."
      (Just "The investigators win the game!")
  , archiveCard
      113
      "A Dark Herald"
      "Take card 39 (Servitor of R'lyeh epic monster) and spawn it at Falcon Point.\nWhen there is ten or more doom on the scenario sheet, flip this card."
      ( Just
          "One investigator may suffer two direct horror to attempt to hold the end at bay and test lore. If your test result is greater than the amount of doom on card 117, move one doom from the scenario sheet to card 117 and flip this card. Otherwise, continue reading.\nThe investigators lose the game!"
      )
  , archiveCard
      114
      "Crippling Visions"
      "When an investigator removes two or more doom from the board in a single ward action, they suffer one horror unless they discard one focus.\nWhen there is ten or more doom on the scenario sheet, flip this card.\nReckoning—Place one doom at the ritual rite."
      ( Just
          "One investigator may spend one focus to attempt to hold the end at bay and test will. If your test result is greater than the amount of doom on card 118, move one doom from the scenario sheet to card 118 and flip this card. Otherwise, continue reading.\nThe investigators lose the game!"
      )
  , archiveCard
      115
      "Blood Sacrament"
      "Remove one blank token from the mythos cup and add one spawn monster token to the mythos cup.\nWhen there is ten or more doom on the scenario sheet, flip this card.\nReckoning—Spawn one monster at the cultist shrine."
      ( Just
          "One investigator may suffer two direct damage to attempt to hold the end at bay and test strength. If your test result is greater than the amount of doom on card 119, move one doom from the scenario sheet to card 119 and flip this card. Otherwise, continue reading.\nThe investigators lose the game!"
      )
  , archiveCard
      116
      "A Dark Alliance"
      "Take card 75 (Mother Hydra epic monster) and spawn it at the North Point Lighthouse.\nWhen there is ten or more doom on the scenario sheet, flip this card."
      ( Just
          "One investigator may suffer one direct damage and one direct horror to attempt to hold the end at bay and test will. If your test result is greater than the amount of doom on card 120, move one doom from the scenario sheet to card 120 and flip this card. Otherwise, continue reading.\nThe investigators lose the game!"
      )
  , -- 117-120 each print a map showing where the markers and monster go; that
    -- layout is on the card art rather than in this text
    archiveCard
      117
      ""
      "Add Innsmouth Village and Innsmouth Shore to the board and place one red and one blue marker as shown below.\nShuffle the event discard pile and the set-aside Innsmouth event cards into the event deck.\nPlace one doom in the indicated space for each white marker in Arkham, then discard all white markers. Spawn one monster in the indicated space.\nThen flip this card."
      ( Just
          "The Beast Stirs\nThe blue marker is the \"Ritual Site.\"\nThe red marker is the \"Cultist Shrine.\"\nAdd card 108 to the codex.\nWhen there is seven or more doom on the scenario sheet, add card 113 to the codex. (Do not return this card to the archive.)"
      )
  , archiveCard
      118
      ""
      "Add Central Kingsport and Kingsport Harbor to the board and place one red and one blue marker as shown below.\nShuffle the event discard pile and the set-aside Kingsport event cards into the event deck.\nPlace one doom in the indicated space for each white marker in Arkham, then discard all white markers. Spawn one monster in the indicated space.\nThen flip this card."
      ( Just
          "Grim Crescendo\nThe blue marker is the \"Ritual Site.\"\nThe red marker is the \"Cultist Shrine.\"\nAdd card 108 to the codex.\nWhen there is seven or more doom on the scenario sheet, add card 114 to the codex. (Do not return this card to the archive.)"
      )
  , archiveCard
      119
      ""
      "Add Innsmouth Village and Innsmouth Shore to the board and place one red and one blue marker as shown below.\nShuffle the event discard pile and the set-aside Innsmouth event cards into the event deck.\nPlace one doom in the indicated space for each white marker in Arkham, then discard all white markers. Spawn one monster in the indicated space.\nThen flip this card."
      ( Just
          "Lurking Hordes\nThe blue marker is the \"Ritual Site.\"\nThe red marker is the \"Cultist Shrine.\"\nAdd card 108 to the codex.\nWhen there is seven or more doom on the scenario sheet, add card 115 to the codex. (Do not return this card to the archive.)"
      )
  , archiveCard
      120
      ""
      "Add Central Kingsport and Kingsport Harbor to the board and place one red and one blue marker as shown below.\nShuffle the event discard pile and the set-aside Kingsport event cards into the event deck.\nPlace one doom in the indicated space for each white marker in Arkham, then discard all white markers. Spawn one monster in the indicated space.\nThen flip this card."
      ( Just
          "Drawing Closer\nThe blue marker is the \"Ritual Site.\"\nThe red marker is the \"Cultist Shrine.\"\nAdd card 108 to the codex.\nWhen there is seven or more doom on the scenario sheet, add card 116 to the codex. (Do not return this card to the archive.)"
      )
  ]
