{- | Secrets of the Order's archive cards. An archive card is pure text: the side
showing and, for the ones that turn over, what is on the back. What each one does
lives in its scenario's codex behaviours.

Cards 121-134 belong to Bound to Serve. Cards 131-134 are the four versions of
Carl Sanford's answer, one of which is drawn at random and left facedown under
card 122 until the investigators present their evidence; they are printed on
headline backs, so neither side of one is ever read until then.

Cards 135-149 belong to The Dead Cry Out. Not all of them are text: 145 and 146 are
the two gug priests, and 147-149 are printed on Underworld backs and join that
neighborhood's encounter deck when the search for the phylactery begins.
-}
module AH3e.Content.SecretsOfTheOrder.Archive (cards) where

import AH3e.Content.Tiles (spaceIdFor)
import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Map.Strict qualified as Map

cards :: [CardDef]
cards =
  fromBox SecretsOfTheOrder (boundToServe <> appeals <> theDeadCryOut <> gugPriests <> underworldHunt)

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

{- | Cards 135-144, the codex of The Dead Cry Out: the gugs hunt the city's people
while the investigators work out what the invasion is for, and the way out runs
through whichever of the three threads the markers on the board turn out to name.
-}
theDeadCryOut :: [CardDef]
theDeadCryOut =
  [ archiveCard
      135
      "A Shifting Path"
      "When you draw a blank mythos token from the mythos cup, do the following:\n- Set aside all components on the hidden path tile, without changing their game state. (For example, delayed investigators remain delayed, and monsters keep all their wounds and remain ready or exhausted.)\n- Flip the hidden path tile and move it clockwise around the Underworld tile to the next corner. Place a random corner of the hidden path tile adjacent to the Underworld tile.\n- Return all set aside components to the hidden path tile."
      Nothing
  , archiveCard
      136
      "A Wave of Blood"
      "Monsters treat the closest bystander as their prey (instead of their normal prey) and engage bystanders as though they are investigators.\nAt the end of the monster phase, if a monster is engaged with a bystander, discard that bystander, exhaust that monster, and place one doom on the scenario sheet.\nAction: Flip one bystander in your space faceup to gain that ally card and place one clue from the token pool on the scenario sheet.\nWhen there are no bystanders on the board, add card 139 to the codex and flip this card."
      ( Just
          "Bound to Darkness\nMonsters treat the closest bystander as their prey (instead of their normal prey) and engage bystanders as though they are investigators.\nAt the end of the monster phase, if a monster is engaged with a bystander, discard that bystander, exhaust that monster, and place one doom on the scenario sheet.\nAction: Flip one bystander in your space faceup to gain that ally card.\nReckoning\8212Each investigator rolls one die. (This is not a test.) If your result is less than or equal to the number of allies you have, place one doom in the unstable space unless you discard one ally."
      )
  , archiveCard
      137
      "Dark Disciples"
      "When there is three doom on the scenario sheet, flip this card."
      ( Just
          "Ancient Hatred\nAdd one {monster} token and one blank token to the mythos cup.\nPlace one bystander in the unstable space.\nWhen there is nine doom on the scenario sheet, add card 138 to the codex and return this card to the archive."
      )
  , archiveCard
      138
      "A Dark Scion"
      "Place one bystander in the unstable space.\nIf the Mummified Gug epic monster is on the board, it deals one damage to each investigator engaged with it. Then discard that monster.\nTake card 146 (The Seer of Mnar epic monster) and spawn it at the City of the Gugs.\nWhen The Seer of Mnar is defeated, shuffle card 146 together with the top two cards of the headline deck and place them on top of that deck. When card 146 is drawn from the headline deck, spawn it at the unstable space.\nWhen there is thirteen doom on the scenario sheet, flip this card."
      (Just "The investigators lose the game!")
  , archiveCard
      139
      "A Strange Pantheon"
      "When there are three or more clues on the scenario sheet, flip this card."
      ( Just
          "If The Seer of Mnar is not in play, take card 145 (Mummified Gug epic monster) and spawn it at the unstable space.\nPlace one bystander in the space with the most doom.\nTake six markers\8212two green, two blue, and two red\8212and randomize them facedown.\nFor each Arkham neighborhood, place one facedown marker in the space in that neighborhood with the most doom.\nAdd card 140 to the codex and return this card to the archive."
      )
  , archiveCard
      140
      "A Dark Rite"
      "Action: Spend two clues from the scenario sheet to reveal a marker at your location. Then if markers of two different colors have been revealed, flip this card."
      ( Just
          "Discard all unrevealed markers and add cards to the codex based on the color of the revealed markers:\n- If there are one or more revealed red markers, add card 141 to the codex.\n- If there are one or more revealed blue markers, add card 142 to the codex.\n- If there are one or more revealed green markers, add card 143 to the codex.\nThen return this card to the archive."
      )
  , archiveCard
      141
      "Free the Vessels"
      "Move each revealed red marker to the City of the Gugs.\nAction: You may spend two clues from the scenario sheet to attempt to free the captured citizens ({observation}). If you pass, flip this card. If there are two red markers at the City of the Gugs, reduce the cost of this action to one clue. Perform this action only at the City of the Gugs."
      ( Just
          "Move one red marker to the scenario sheet and discard any other red markers.\nAdd card 144 to the codex. If that card is already in the codex, place one bystander in the unstable space instead.\nThen return this card to the archive."
      )
  , archiveCard
      142
      "The Phylactery"
      "Move each revealed blue marker to the Underworld neighborhood.\nTake one card at random from among cards 147-149. If there are two revealed blue markers, place that card on top of the Underworld encounter deck and discard one blue marker. Otherwise, shuffle that card together with the top two cards of the Underworld encounter deck and place them on top of that deck.\nAfter the blue marker is moved to the scenario sheet, unless the investigators spend one clue from the scenario sheet, each investigator suffers one damage and one horror. Then flip this card."
      ( Just
          "Add card 144 to the codex. If that card is already in the codex, place one bystander in the unstable space instead.\nThen return this card to the archive."
      )
  , archiveCard
      143
      "Reinforce the Seal"
      "Move each revealed green marker to this card.\nAfter you resolve a ward action in the unstable space, you may spend one clue from the scenario sheet to place a green marker on this card.\nWhen there are three green markers on this card, move one of them to the scenario sheet and discard the rest. Then flip this card."
      ( Just
          "Add card 144 to the codex. If that card is already in the codex, place one bystander in the unstable space instead.\nThen return this card to the archive."
      )
  , archiveCard
      144
      "At Last"
      "Place one bystander in the unstable space.\nAction: Spend two clues from the scenario sheet and draw and resolve two tokens from the mythos cup to place a white marker on the scenario sheet.\nPerform this action only in the hidden path space.\nWhen there are three markers (of any color) on the scenario sheet, flip this card."
      (Just "The investigators win the game!")
  ]

{- | Cards 145 and 146. Both are massive, so neither can be exhausted and both engage
everyone standing with them, and both make the party pay for walking away.
-}
gugPriests :: [CardDef]
gugPriests =
  [ epic
      145
      "Mummified Gug"
      ["Deathless", "Gug"]
      (Lurker (PlaceDoomAt TheUnstableSpace (N 1)))
      (4, 1)
      (-1, -1)
      "Elite 1 (The Mummified Gug has one additional health per investigator.) Massive (The Mummified Gug engages and attacks each investigator in its space. It cannot be exhausted.) After you disengage this monster, place one doom in the unstable space. Lurker\8212Place one doom in the unstable space."
  , epic
      146
      "The Seer of Mnar"
      ["Deathless", "Gug", "Herald"]
      (Lurker (PlaceDoomAt ScenarioSheet (N 1)))
      (6, 2)
      (-2, -1)
      "Elite 2 (The Seer of Mnar has two additional health per investigator.) Massive (The Seer of Mnar engages and attacks each investigator in its space. It cannot be exhausted.) After you disengage the Seer of Mnar, draw and resolve two mythos tokens. Lurker\8212Place one doom on the scenario sheet."
  ]
 where
  epic :: Int -> Text -> [Trait] -> Activation -> (Int, Int) -> (Int, Int) -> Text -> CardDef
  epic n title kinds act (hp, eliteN) (atk, evade) blurb =
    CardDef
      (CardCode ("archive-" <> tshow n))
      title
      CoreSet
      1
      ( MonsterCard
          MonsterDef
            { readyName = Nothing
            , spawn = CustomSpaceRule "Spawned by the codex of The Dead Cry Out"
            , activation = act
            , speed = 0
            , traits = kinds
            , health = hp
            , elite = eliteN
            , attackSkill = Strength
            , attackModifier = atk
            , evadeModifier = evade
            , damage = 2
            , horror = 2
            , remnant = True
            , keywords = [Massive]
            , epic = True
            , text = blurb
            }
      )

{- | Cards 147-149, the hunt for the Seer's phylactery. Each is printed on an Underworld
back and holds an entry for all three of its spaces, but only one of the three hides the
reliquary; the other two put the card back on top of the deck, so the search goes on
where it left off.
-}
underworldHunt :: [CardDef]
underworldHunt =
  [ hunt
      147
      [
        ( "City of the Gugs"
        , "Deep within a gug temple, you open a putrid reliquary. The unholy energy here is palpable ({will}). If you pass, you search through the ancient bones; gain one curio. Whether you pass or not, you find the Seer's phylactery; move the blue marker to the scenario sheet and return this card to the archive."
        , Seq [pass Will 0 curioItem, found]
        )
      , bonePit
      , deathFire
      ]
  , hunt
      148
      [ gugPatrol
      , bonePit
      ,
        ( "Vaults of Zin"
        , "Something within a side chamber calls to you, whispering predictions of the death of all humanity ({will}). If you pass, you command the voices to guide you; gain one spell. Whether you pass or not, you find the phylactery hidden in the cave; move the blue marker to the scenario sheet and return this card to the archive."
        , Seq [pass Will 0 spell, found]
        )
      ]
  , hunt
      149
      [ gugPatrol
      ,
        ( "Vale of Pnath"
        , "A fresh human corpse is entombed within a cage of bones, clutching something to their chest ({observation}). If you pass, you open the cage and recover their belongings; gain one common item. Whether you pass or not, you find the Seer's phylactery set into the bars of the cage; move the blue marker to the scenario sheet and return this card to the archive."
        , Seq [pass Observation 0 commonItem, found]
        )
      , deathFire
      ]
  ]
 where
  hunt :: Int -> [(Text, Text, Effect)] -> CardDef
  hunt n entries =
    CardDef
      (CardCode ("archive-" <> tshow n))
      ("The Underworld " <> tshow n)
      CoreSet
      1
      ( NeighborhoodCard
          (NeighborhoodId "the-underworld")
          (Map.fromList [(spaceIdFor place, Encounter txt eff) | (place, txt, eff) <- entries])
      )
  -- the phylactery, and the card with it, leave the Underworld for good
  found = Custom "dco-phylactery-found"
  -- the search carries on from the top of the deck rather than the bottom
  again = Custom "dco-search-goes-on"
  bonePit =
    ( "Vale of Pnath"
    , "The clatter of bones betrays something burrowing through the mountain of discarded skeletons. You may suffer two horror to brave the tunneling threat and search the bones. If you do, you find some abandoned valuables, but not the phylactery; gain $2. Whether you search or not, place this card on top of the Underworld encounter deck."
    , Seq [mayPay (CostHorror 2) (money 2), again]
    )
  deathFire =
    ( "Vaults of Zin"
    , "The pale green glow of the death-fire that lights this place casts deep shadows that hinder your search ({observation}). If you pass, you find a ghast's meal; gain one remnant. If you fail, you are ambushed; suffer one damage. Whether you pass or not, the phylactery is not here. Place this card on top of the Underworld encounter deck."
    , Seq [Test Observation 0 (remnants 1) (damage 1), again]
    )
  gugPatrol =
    ( "City of the Gugs"
    , "The acrid gug temple is quiet, save for the massive footfalls of a patrolling giant. You don't find the phylactery, but you may suffer one damage to brave the danger and gain one remnant from the temple. Whether you do this or not, place this card on top of the Underworld encounter deck."
    , Seq [mayPay (CostDamage 1) (remnants 1), again]
    )
