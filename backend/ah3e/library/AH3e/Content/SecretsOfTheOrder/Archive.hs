{- | Secrets of the Order's archive cards. An archive card is pure text: the side
showing and, for the ones that turn over, what is on the back. What each one does
lives in its scenario's codex behaviours.

Cards 121-134 belong to Bound to Serve. Cards 131-134 are the four versions of
Carl Sanford's answer, one of which is drawn at random and left facedown under
card 122 until the investigators present their evidence; they are printed on
headline backs, so neither side of one is ever read until then.
-}
module AH3e.Content.SecretsOfTheOrder.Archive (cards) where

import AH3e.Content.Vocabulary (fromBox)
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Ids

cards :: [CardDef]
cards = fromBox SecretsOfTheOrder (boundToServe <> appeals)

archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n name front back =
  CardDef
    (CardCode ("archive-" <> tshow n))
    name
    CoreSet
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

boundToServe :: [CardDef]
boundToServe =
  [ archiveCard
      121
      "Nightmare Plague"
      "When an investigator would gain a clue, instead place that clue on this card and place one random set-aside Lodge monster on the bottom of the monster deck. Then if there are two clues on this card, discard those clues and flip this card.\nWhen there is three or more doom on the scenario sheet, add card 124 to the codex. (Do not remove this card from the codex.)"
      ( Just
          "Grim Foundations\nAdd card 122 to the codex. Take one card at random from among cards 131-134 and, without looking at it, place it facedown under card 122.\nWhen there is three or more doom on the scenario sheet, add card 124 to the codex and return this card to the archive."
      )
  , archiveCard
      122
      "Gathering Evidence"
      "Action: Present your evidence to Carl Sanford and flip this card. Perform this action only at the Silver Twilight Lodge. (The more clues on the scenario sheet when you perform this action, the better the resolution on the card under this one will be.)\nReckoning\8212Place one doom on this card. Then if there is three or more doom on this card, discard one clue from the scenario sheet and flip this card unless you resolve a gate burst."
      ( Just
          "Reveal the card under this card and add it to the codex.\nThen return this card to the archive."
      )
  , archiveCard
      123
      "Destroy the Seals"
      "Action: Test lore. If you pass, reveal a facedown marker in your space and move it to the scenario sheet. Then spawn one spirit monster in your space unless you spend one clue from the scenario sheet.\nWhen there are markers of three different colors on the scenario sheet, flip card 130."
      ( Just
          "A New Covenant\nAction: Spend one clue from the scenario sheet to choose and resolve one of the effects below:\n- Suffer four damage and four horror to move the blue marker from your space to the scenario sheet.\n- Discard one talent or two spells to move the red marker from your space to the scenario sheet.\n- Discard items with a total value of $5 or greater to move the green marker from your space to the scenario sheet.\nAfter the third marker is moved to the scenario sheet, flip card 128 or 129. (Only one of those cards will be in the codex.)"
      )
  , archiveCard
      124
      "Mounting Dread"
      "Place one white marker in each street space adjacent to the French Hill neighborhood.\nAfter an investigator resolves a street encounter, they may discard a white marker from their space and test will. If they fail, they suffer one horror.\nWhen there is six or more doom on the scenario sheet, flip this card."
      ( Just
          "Dead Menace\nFor each white marker, the nearest investigator may suffer two horror to remove that marker.\nThen place one doom in each space adjacent to each remaining white marker.\nThen discard all white markers from the board.\nWhen there is nine or more doom on the scenario sheet, add card 125 to the codex and return this card to the archive."
      )
  , archiveCard
      125
      "Nyogtha Awakes!"
      "If card 121 is in the codex, place two doom on the scenario sheet and shuffle all set-aside Lodge monsters into the monster deck.\nIf card 122 is in the codex, discard half of the clues on the scenario sheet (rounded down) and flip card 122.\nIf card 130 is in the codex, add card 127 to the codex, \"Stand Together\" side up. Otherwise, add card 127 to the codex, \"Stand Alone\" side up.\nReturn cards 121-123 to the archive and add card 126 to the codex.\nWhen there is thirteen or more doom on the scenario sheet, flip this card."
      (Just "The investigators lose the game!")
  , archiveCard
      126
      "Desperate Binding"
      "For each marker on the scenario sheet, place one clue on this card. Then discard all markers from the scenario sheet.\nWhile this card is in the codex, the Witch House is the unstable space instead of the normal unstable space.\nAction: You attempt to seal Nyogtha beneath the Witch House (lore -1). For each success, you may suffer one damage and one horror to move one clue from the scenario sheet to this card. Then if there are five clues on this card, flip it. Perform this action only at the Witch House."
      ( Just
          "The investigators win the game!\nEach investigator at the Witch House is devoured."
      )
  , archiveCard
      127
      "Stand Together"
      "Add five white markers to the mythos cup and return all mythos tokens to the cup.\nWhen you draw a white marker from the mythos cup, place it in your play area.\nOnce per round, during your turn, you may return a white marker from your play area to the game box to perform an additional action, even if you have already performed that action this round. (Do not return white markers in your play area to the mythos cup when it is empty.)"
      ( Just
          "Stand Alone\nAdd five white markers to the mythos cup and return all mythos tokens to the cup.\nWhen you draw a white marker from the mythos cup, place it in your space. Then suffer one damage and one horror for each white marker in your space.\nWhen the mythos cup is empty, return all white markers to the game box and return this card to the archive."
      )
  , archiveCard
      128
      "Open Hostility"
      "Spawn two random set-aside Lodge monsters and shuffle the rest into the monster deck.\nAdd one spawn monster token and one spread doom token to the mythos cup.\nIf card 125 is not in the codex, take three markers\8212one red, one blue, and one green\8212and place them at the Witch House. Then add card 123 to the codex, with the \"A New Covenant\" side up.\nReckoning\8212For each Lodge monster, place one doom in its space. Then spawn one Lodge monster."
      (Just "The investigators win the game!")
  , archiveCard
      129
      "Closed Doors"
      "Take three random set-aside Lodge monsters and shuffle them into the monster deck.\nIf card 125 is not in the codex, take three markers\8212one red, one blue, and one green\8212and place them at the Witch House. Then add card 123 to the codex, with the \"A New Covenant\" side up.\nReckoning\8212Place one doom at the unstable space unless you spawn one random set-aside Lodge monster."
      (Just "The investigators win the game!")
  , archiveCard
      130
      "By the Order"
      "Return all Lodge monsters from the board and monster deck to the game box.\nChoose an investigator to become the Steward of the Order.\nIf card 125 is not in the codex, take six markers\8212two red, two blue, and two green\8212and randomize them facedown. Place one facedown in Independence Square, the Unvisited Isle, the Graveyard, Bayfriar Gardens, Hangman's Hill, and the Historical Society. Then add card 123 to the codex, with the \"Destroy the Seals\" side up.\nReckoning\8212Discard one monster or one doom token from any space."
      (Just "The investigators win the game!")
  ]

{- | Cards 131-134. Each one weighs the clues on the scenario sheet differently, so
how good a hearing the Lodge gives depends on which was drawn as much as on the
evidence; none of them is read until card 122 turns over.
-}
appeals :: [CardDef]
appeals =
  [ appeal
      131
      "0: Add card 128 to the codex and resolve a gate burst.\n1-2: Add card 129 to the codex, resolve a gate burst, and place one clue on the scenario sheet.\n3: Add card 129 to the codex and place one clue on the scenario sheet.\n4+: Add card 130 to the codex and place two clues on the scenario sheet."
  , appeal
      132
      "0-2: Add card 128 to the codex.\n3-4: Add card 129 to the codex and place one clue on the scenario sheet.\n5: Add card 130 to the codex, place two clues on the scenario sheet, and choose an investigator to gain one spell.\n6+: Add card 130 to the codex, place three clues on the scenario sheet, and choose an investigator to gain two spells."
  , appeal
      133
      "0: Add card 128 to the codex and place one doom in the unstable space.\n1-2: Add card 128 to the codex.\n3: Add card 129 to the codex, place one clue on the scenario sheet, and spread doom once.\n4+: Add card 130 to the codex and place two clues on the scenario sheet."
  , appeal
      134
      "0-1: Add card 128 to the codex.\n2-4: Add card 129 to the codex and place one clue on the scenario sheet.\n5: Add card 130 to the codex, place two clues on the scenario sheet, and add card 25 to the codex with the \"Plumb the Void\" side up.\n6+: Add card 130 to the codex, place three clues on the scenario sheet, and add card 25 to the codex with the \"Plumb the Void\" side up."
  ]
 where
  appeal n bands =
    archiveCard
      n
      "Appeal to the Lodge"
      ( "Discard all clues from the scenario sheet and resolve the effect below based on the number of clues discarded in this way:\n"
          <> bands
          <> "\nThen return this card to the archive."
      )
      Nothing
