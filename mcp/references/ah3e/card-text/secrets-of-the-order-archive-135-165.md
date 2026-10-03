# Secrets of the Order — archive cards 135-165

Card text as the user transcribed it (2026-10-02), kept **verbatim**, OCR slips and
all, so the slips stay visible as slips. The expansion rules under
`../secrets-of-the-order.txt` reproduce a few of these cards in full and are the
cross-check of first resort before asking again.

Cards 121-134 are implemented (`AH3e.Content.SecretsOfTheOrder.Archive` and the codex
behaviours in `BoundToServeBehaviors`), so their text lives in the code and is not
repeated here. Everything else the user transcribed for this box — the special pile,
the player cards, the monsters, French Hill, the Underworld, the thresholds, the two
mystery decks, headlines 44-47, the sixteen neighborhood extras, Lost Souls, and Bound
to Serve's sheet and events — is likewise in `AH3e.Content.*`.

## Which scenario

All three scenarios are implemented, so every card's text lives in the code and this
transcription is kept only as the source it was read from: Bound to Serve holds cards 2
and 121-134, The Dead Cry Out 1 and 135-149, and The Key and the Gate 2 and 150-165.

## What these cards asked of the engine

All of it is built, in `TheDeadCryOutBehaviors` and `TheKeyAndTheGateBehaviors`.

- **bystanders** — in. `PlaceBystander`, `TakeBystander` and `DiscardBystander`, with
  `#bystanders` on the game and the piece drawn on the board (`SpaceChips.vue`). Monsters
  hunt them through `CodexBehavior.preyReplacement`, and `atEndOfMonsterPhase` is what
  takes the ones the gugs reach.
- **a tile that moves** — in. `moveCornerTile` walks a corner piece clockwise round a
  tile, re-deals its printed icons and turns the picture with them; the space keeps its
  identity, so everything standing on it comes along without changing state and the
  card's "set aside and return" needs no doing. Geometry is shared with setup through
  `Tiles.cornerSeat` and `Tiles.ringAround`.
- **hazardous borders** — in, for the Underworld tile (gugs/Pnath horror, gugs/Zin
  damage, Zin/Pnath focus) and for the hidden path (damage, horror and focus, dealt
  round its three borders at random when the tile is laid, as the rules ask). The
  derelict portal's own icons have not been transcribed; the rules' example shows a
  damage border out of it into the Vale of Pnath.
- **a card that re-enters the headline deck** — in. 138 shuffles the Seer of Mnar's own
  card into the top three of the headline deck, and `CodexBehavior.headlineReplacement`
  is what reads a monster out of there instead of a headline.
- **markers moved to a codex card** (143's green markers) — in, as a token on the entry
  (`MarkCodexToken`), the same shape Bound to Serve's seals use. `RevealMarkerAt` turns
  one face up where it lies.
- **an encounter card that goes back on top of its deck** — in, as
  `EncounterState.returnToTop`, which is what 147-149 use when the phylactery is not in
  that place.
- **a mid-scenario map addition** — in. 153 lays the Underworld and a derelict portal
  against French Hill with `AddToBoard`, which now also **turns** the threshold piece it
  puts down, the way setup does; the event-deck surgery is done in the card's own
  behaviour.
- **a defeated investigator who keeps playing** — in. A `Defeated` investigator may keep
  their `space`, which leaves their token on the board while every query that matters
  still passes them by, and card 160 remembers which one they are in its own tokens.
- **a card that passes from investigator to investigator** (158, 159) — in, built as a
  held condition rather than a codex entry, so `Asset.flipped` is its two sides and
  `Asset.tokens` is the doom on it.
- **a win for only some investigators** (155: "each investigator with a DARK PACT wins
  the game") — **not modelled**. `WinTheGame` has no payload and the engine has no
  per-investigator win, so the group wins and the log names who shares in it.
- **a reckoning that goes last** (155) — in, as `CodexBehavior.reckoningLast`. The leader
  picks the order of reckonings, so list order could not do it.

## Readings I would want confirmed before building

- 136's second line is garbled twice over ("treat the closest bystander as their brey",
  then "their Dyer anders dof the norey are and engage"). Both sides plainly say
  monsters prey on and engage bystanders as though they were investigators. Built that
  way, with "engaged with a bystander" read as a monster sharing its space and not
  holding an investigator -- so standing guard over one protects it.
- 138: "If the Mummified Gug epic monster is on the board, it deals one damage to each
  investigator engaged with it. Then discard that monster."
- 144's title reads "AT LAsT".
- 147-149 are three variants of the same Underworld hunt, and only one space on each
  holds the phylactery; the other two entries put the card back on top of the deck.
- 150's flavour names "Deputy Galeas Morgan" and "Baytriar Gardens" (Bayfriar).
- 158's flavour is badly mangled ("fills vit and musies. our stors to tensions"); the
  rules lines beneath it are clean.
- 161-165 each have three entries and each ends "you find an Elder", which is the phrase
  card 152 keys off.

## Verbatim transcription

135 (HEADLINE CARD SO HEADLINE BACK)
A SHIFTING PATH
Unstable portals appear throughout Arkham, disgorging unnatural beasts into the city streets.
Secret routes into the depths of a dark and unforgiving world form and then fade away as the energies that assault your reality rend new paths into that unnatural place.
When you draw a blank mythos token from the mythos cup, do the following:
• Set aside all components on the hidden path tile, without changing their game state. (For example, delayed investigators remain delayed, and monsters keep all their wounds and remain ready or exhausted.)
• Flip the hidden path tile and move it clockwise around the Underworld tile to the next corner. Place a random corner of the hidden path tile adjacent to the Underworld tile.
• Return all set aside components to the hidden path tile.

136
A WAVE OF BLOOD
The beasts that spill across Arkham seek easy prey. You cannot allow them to feed upon the unsuspecting populace. Once the imminent threat is clear, you can devote your energy to understanding where these terrible creatures came from.
Monsters treat the closest bystander as their brey (instead of their normal prey and engage oystanders as thougn they are investigators
At the end of the monster phase, if a monster is engaged with a bystander, discard that bystander, exhaust that monster, and place one doom on the scenario sheet.
Action: Flip one bystander in your space faceup to gain that ally card and place one clue from the token pool on the scenario sheet.
When there are no bystanders on the board, add card 139 to the codex and flip this card.

BACK
BOUND TO DARKNESS
The creatures that rampage through the streets are not the only souls bound to the sinister power that assaults you. Some dark energy has ensnared the minds of the city's residents, awakening aspects of the dead gods that lurk beyond Arkham's shadows.
Monsters treat the closest bystander as their Dyer anders dof the norey are and engage
At the end of the monster phase, if a monster is engaged with a bystander, discard that bystander, exhaust that monster, and place one doom on the scenario sheet.
Action: Flip one bystander in your space faceup to gain that ally card.
Reckoning — Each investigator rolls one die. (This is not a test.) If your result is less than or equal to the number of allies you have, place one doom in the unstable space unless you discard one ally.

137
DARK DISCIPLES
These attacks are not random. Something on the other side of the mystical portals that ripple through Arkham stirs these beasts into a savage frenzy. Each death fuels the dark rituals that are at work here.
When there is three doom on the scenario sheet, flip this card.

BACK
ANCIENT HATRED
The beasts and terrors that assault you serve the Great Ones who ruled Earth before humanity spread across the world. Driven by a dark priest, the creatures threaten to overwhelm the usurpers who drove their ancient gods into exile in Kadath, the unknown city.
Add one {monster} token and one blank token to the mythos cup.
Place one bystander in the unstable space.
When there is nine doom on the scenario sheet, add card 138 to the codex and return this card to the archive.

138
A DARK SCION
The mummified giant who leads the followers of the Great Ones ascends to its full power. The Seer of Mnar channels the might of the Great Ones to extinguish the light of humanity.
Place one bystander in the unstable space.
If the Mummified Gug epic monster is on the board, it deals one damage to each investigator engaged with it. Then discard that monster.
Take card 146 (The Seer of Mnar epic monster) and spawn it at the City of the Gugs.
When The Seer of Mnar is defeated, shuffle card 146 together with the top two cards of the headline deck and place them on top of that deck. When card 146 is drawn from the headline deck, spawn it at the unstable space.
When there is thirteen doom on the scenario sheet, flip this card.

BACK
The sky cracks apart. An unknown mountain looms on the horizon as an icy wind cascades down its slopes. The lost city of Kadath and the forgotten gods who rule it have come to Arkham, restored to the world of humanity by the Seer's machinations. The Great Ones have returned to seize the world from the humans who drove them into exile. Your world is theirs.
The investigators lose the game!

139
A STRANGE PANTHEON
The beasts that spill across Arkham-those that can speak anyway—evoke a litany of names while they hunt: Lobon, Oukranos, Zo-Kalar, and Tamash. While they are unfamiliar to you, it is clear that the monstrous invaders hold these entities in great reverence.
When there are three or more clues on the scenario sheet, flip this card

BACK
The forces marshaled against you are consecrated in the name of the Great Ones, the mythic deities worshiped in an ancient region called "Mnar." What you would once have discarded as odd superstition is evidently all too real. Your efforts have drawn the attention of a foul priest of the lost gods, a towering, undying giant. As horrific as that creature may be, stopping its rampage will not get you any closer to ending the greater threat.
If The Seer of Mnar is not in play, take card 145 (Mummified Gug epic monster) and spawn it at the unstable space.
Place one bystander in the space with the most doom.
Take six markers—two green, two blue, and two red—and randomize them facedown.
For each Arkham neighborhood, place one facedown marker in the space in that neighborhood with the most doom.
Add card 140 to the codex and return this card to the archive.

140
A DARK RITE
The monstrous invaders seek your extinction, though the ancient gods they serve bear only a crushing indifference toward humanity. The disciples of Mnar seek to perform a ritual to return their gods, the Great Ones of Kadath, to the prominence they lost when humanity spread across the world. You must search for more information about how they seek to do so.
Action: Spend two clues from the scenario sheet to reveal a marker at your location. Then if markers of two different colors have been revealed, flip this card.

BACK
While the Great Ones themselves bear no great animus toward humanity, and indeed, barely even acknowledge your existence, their monstrous followers view you as usurpers. They bear you a timeless hate.
The Seer of Mnar, the mummited gug priest that leads them, weaves an elaborate plot in its efforts to restore the Great Ones to their dominance of Earth. Severing the threads of that plot will leave the Seer vulnerable.
Discard all unrevealed markers and add cards to the codex based on the color of the revealed markers:
• If there are one or more revealed red markers, add card 141 to the codex.
• If there are one or more revealed blue markers, add card 142 to the codex.
• If there are one or more revealed green markers, add card 143 to the codex.
Then return this card to the archive.

141
FREE THE VESSELS
Ancient bloodlines persist within a few families in Arkham. Gug priests have abducted the descendants of Sarnath, the last human city to worship the Great Ones. They seek to use these captives as vessels to transfer their lost gods from distant Kadath into your own world. You must find and rescue the unfortunate prisoners before that can happen.
Move each revealed red marker to the City of the Gugs.
Action: You may spend two clues from the scenario sheet to attempt to free the captured citizens ({observation}). If you pass, flip this card. If there are two red markers at the City of the Gugs, reduce the cost of this action to one clue. Perform this action only at the City of the Gugs.

BACK
You find a clear path to lead the rescued aptives out of the City of the Gugs and back to the safety of your own world. You have robbed the Seer of Mnar of the sacrifices needed to call the Great Ones from the lost city of Kadath.
You will need to do more to stop the threats that face you, but you are one step closer to victory.
Move one red marker to the scenario sheet and discard any other red markers.
Add card 144 to the codex. If that card is already in the codex, place one bystander in the unstable space instead.
Then return this card to the archive.

142
THE PHYLACTERY
The high priest of the gugs cannot be killed. Its soul is bound within a potent artifact that lies hidden somewhere within the Underworld.
Until you find it and destroy that reliquary, the evil that threatens you will always return.
Move each revealed blue marker to the Underworld neighborhood.
Take one card at random from among cards 147-149. If there are two revealed blue markers, place that card on top of the Underworld encounter deck and discard one blue marker. Otherwise, shuffle that card together with the top two cards of the Underworld encounter deck and place them on top of that deck.
After the blue marker is moved to the scenario sheet, unless the investigators spend one clue from the scenario sheet, each investigator suffers one damage and one horror. Then flip this card.

BACK
The phylactery is a gruesome totem constructed of human bones, still wet with sinew and gore.
A searing light seeps out of the construct like an oily, iridescent miasma. Tamping down the visceral disgust this monstrosity evokes, you smash it and release the energy stored within.
The gug high priest is no longer protected by this dark magic. You will need to do more to stop the threats that face you, but you are one step closer to victory.
Add card 144 to the codex. If that card is already in the codex, place one bystander in the unstable space instead.
Then return this card to the archive.

143
REINFORCE THE SEAL
An ancient seal protects your world from alien threats, but after millennia, that shield has grown weak. The assault from the Underworld threatens to destroy those protections, unless you work quickly to reinforce the old magics.
Move each revealed green marker to this card.
After you resolve a ward action in the unstable space, you may spend one clue from the scenario sheet to place a green marker on this card.
When there are three green markers on this card, move one of them to the scenario sheet and discard the rest. Then flip this card.

BACK
The mystical protections that shield your world from otherworldly attacks stand strong once again. You will need to do more to stop the threats that face you, but you are one step closer to victory.
Add card 144 to the codex. If that card is already in the codex, place one bystander in the unstable space instead.
Then return this card to the archive.

144
AT LAsT
Your efforts have left the Seer of Mnar that leads this incursion into your world vulnerable.
If you move quickly, it may be possible to sever the Seer from its connection to both Arkham and the Great Ones. The creature will finally die, and its attempts to overthrow humanity will come to naught.
Place one bystander in the unstable space.
Action: Spend two clues from the scenario sheet and draw and resolve two tokens from the mythos cup to place a white marker on the scenario sheet.
Perform this action only in the hidden path space.
When there are three markers (of any color) on the scenario sheet, flip this card.

BACK
With a feral, rasping howl, the Seer of Mnar rages at the sudden loss of the power of the Great Ones. Your efforts and your sacrifice have severed the Seer's link to the powers that grant it eternal life. Its wordless rattle grows silent and its towering, desiccated figure turns to black ash before crumbling away to nothingness. The wild, crackling portal roaming through Arkham flares brightly and then blinks out of existence. Arkham is quiet once more.
The investigators win the game!

145 (SEE THE OTHER EPIC MONSTERS)
Mummified Gug
Epic Monster — Deathless Gug
Movement: -
Massive. Lurker-Place one doom in the unstable space.
1 remnant
health 4+, strength - 1, observation - 1
Elite 1 (The Mummified Gug has one additional health per investigator.)
Massive (The Mummified Gug engages and attacks each investigator in its space. It cannot be exhausted.) After you disengage this monster, place one doom in the unstable space.
2dmg, 2hrr

146 (SEE THE OTHER EPIC MONSTERS)
The Seer of Mnar
Epic Monster — Deathless Gug Herald
Movement: -
Massive. Lurker—Place one doom on the scenario sheet.
1 remnant
health 6+, strength - 2, observation - 1
Elite 2 (The Seer of Mnar has two additional health per investigator.)
Massive (The Seer of Mnar engages and attacks each investigator in its space. It cannot be exhausted.)
After you disengage the Seer of Mnar, draw and resolve two mythos tokens.
2dmg, 2hrr

147. (The Underworld back)
City of the Gugs
Deep within a gug temple, you open a putrid reliquary. The unholy energy here is palpable ({will}).
If you pass, you search through the ancient bones; gain one curio. Whether you pass or not, you find the Seer's phylactery; move the blue marker to the scenario sheet and return this card to the archive.
Vale of Pnath
The clatter of bones betrays something burrowing through the mountain of discarded skeletons.
You may suffer two horror to brave the tunneling threat and search the bones. If you do, you find some abandoned valuables, but not the phylactery; gain $2. Whether you search or not, place this card on top of the Underworld encounter deck.
Vaults of Zin
The pale green glow of the death-fire that lights this place casts deep shadows that hinder your search ({observation}). If you pass, you find a ghast's meal; gain one remnant. If you fail, you are ambushed; suffer one damage. Whether you pass or not, the phylactery is not here. Place this card on top of the Underworld encounter deck.

148. (The Underworld back)
City of the Gugs
The acrid gug temple is quiet, save for the massive footfalls of a patrolling giant. You dont find the phylactery, but you may suffer one damage to brave the danger and gain one remnant from the temple.
Whether you do this or not, place this card on top of the Underworld encounter deck.
Vale of Pnath
The clatter of bones betrays something burrowing through the mountain of discarded skeletons.
You may suffer two horror to brave the tunneling threat and search the bones. If you do, you find some abandoned valuables, but not the phylactery; gain $2. Whether you search or not, place this card on top of the Underworld encounter deck.
Vaults of Zin
Something within a side chamber calls to you, whispering predictions of the death of all humanity ({will}). It you pass, you command the voices to guide you; gain one spell. Whether you pass or not, you find the phylactery hidden in the cave; move the blue marker to the scenario sheet and return this card to the archive.

149. (The Underworld back)
City of the Gugs
The acrid gug temple is quiet, save for the massive footfalls of a patrolling giant. You don't find the phylactery, but you may suffer one damage to brave the danger and gain one remnant from the temple.
Whether you do this or not, place this card on top of the Underworld encounter deck.
Vale of Pnath
A fresh human corpse is entombed within a cage of bones, clutching something to their chest ({observation}).
If you pass, you open the cage and recover their belongings; gain one common item. Whether you pass or not, you find the Seers phylactery set into the bars of the cage; move the blue marker to the scenario sheet and return this card to the archive.
Vaults of Zin
The pale green glow of the death-fire that lights this place leaves deep shadows that hinder your search ({observation}). If you pass, you find a ghast's meal; gain one remnant. If you fail, you are abushed; suffer one damage. Whether you pass or not, the phylactery is not here. Place this card on top of the Underworld encounter deck.

150.
AT THE THRESHOLD
Something beyond the limits of your perception calls to you. Judging by the strange behavior you've seen among your friends and neighbors, they feel it too. You watch the peddlers in Independence Square wordlessly arrange the benches into oddly specific configurations. The students in the Orne Library reshelve books in an arbitrary sequence. Deputy Galeas Morgan carves an odd glyph into a series of telephone poles, but cannot explain what it means or why he did it.
It is clear, though, that this erratic behavior has shifted from merely baffling to outright dangerous when the first body appears, dead by his own hand in Baytriar Gardens. What-or who—is controlling these people?
When there is four or more doom on the scenario sheet, flip this card.

BACK
THE BEYOND ONE
Your fitful sleep brings only a siren call from bevond the veil that bounds your world. The wordless voice promises you rest—release— resurrection—if only you'll succumb to the temptation of forbidden knowledge.
Each investigator tests {will}. Each investigator that fails places one doom in their space unless they become FATIGUED.
When there is eight or more doom on the scenario sheet, add card 156 to the codex.
(Do not return this card to the archive.)

151
POSSESSION
Carl Sanford has summoned you with a cryptic message, sealed with the crest of the Order of the Silver Twilight: "The Lurker at the Threshold stirs. The Lodge has marshaled our efforts to obstruct its influence, but I fear our defenses are faltering. Regretfully, I need the assistance of the uninitiated. Tonight."
The mystery only deepens when you arrive at the Lodge to find that Sanford has gone on without you. Ihe message he lett behind offers little explanation: "The Elders of the Silver Twilight meet tonight where the veil is weak. Join us at the Unnamable."
Action: Suffer two horror to find Carl Sanford and flip this card. Reduce the cost of this action by one horror for each clue on the scenario sheet. (Do not spend or discard those clues.) Perform this action only at the Unnamable.

BACK
You find Carl Sanford in the basement, collapsed within an incomplete ritual circle.
When you rouse him, he looks wearily around the room.
"We were too slow to act; I should
have called upon you earlier. Yog-Sothoth works a grand design, dominating the minds of Arkham in its attempts to breach our reality.
He says there may be a way to shield this world from the Ancient One's influence using something called "the Key of Zagan." He tells you that the Key is not in Arkham, but rather than answering your appeals for greater specificity, he simply suggests that you locate the missing Elders of the Lodge.
"The Elders I came
here with have been ensnared. They each hold a portion of the ritual I need to open the way, but I fear they ve begun listening too closely to the Lurker at the Ihreshold. They must be returned to the fold."
Take cards 161-165 from the archive.
Place each card facedown on top of the corresponding neighborhood deck. Place one white marker in the central area of each neighborhood.
Add card 152 to the codex and return this card to the archive.

152
THE MISSING ELDERS
Carl Sanford's missing colleagues are scattered throughout the city. When you press him for more information, he remains frustratingly obtuse and merely assures you that you'll get the answers you seek by finding the wayward Elders of the Silver Twilight.
"When you ve
found them, we shall assemble in Bayfriars' to continue our work."
After you "find an Elder" as part of an encounter, move the white marker from your neighborhood to the scenario sheet.
When there is one white marker on the scenario sheet, add card 157 to the codex.
When there are three white markers on the scenario sheet, add card 153 to the codex.
When there are five white markers on the scenario sheet, flip this card.

BACK
With their senior membership restored, the Order of the Silver Twilight sets about conducting rituals to keep the worst of the threat from reaching Arkham.
"Thank you for
your timely assistance, friend," Sanford intones.
"The secrets my colleagues hold are vital to the operation of the Order, and I shudder to think what calamity would befall us if the things they knew fell into the hands of outsiders.
He offers a reassuring hand, and you sense a rare moment of openness from the normally inscrutable man.
"The knowledge we bear is a
terrible burden, and I hoped to spare you some of the risk. Every bit of intormation we carry about the Lurker at the Threshold offers it a greater opportunity to invade our thoughts, and so we have learned to compartmentalize— to insulate ourselves from carrying more knowledge than is wise. I swear to you that we shall not betray the trust you ve shown in us.
Remove one {doom} token from the game and add one blank token to the mythos cup.
Then discard all white markers from the scenario sheet and return this card to the archive.

153
With the ritual completed by the Elders of the Lodge, Carl Sanford opens the way to you.
"The
Key of Zagan rests beyond the ken of mortal men, but we cannot claim it. That task falls to you.
Add the Underworld and the Derelict Portal tiles to the board as shown below. Place one doom in each space of the Underworld and spawn one monster in the indicated space.
Remove the top four cards of the event deck from the game.
Shuffle two of the set aside Underworld event cards into the event deck, and discard the other two into the event discard pile.
Then flip this card.

French Hill - derelict portal - The Underworld
Each location in the underworld gets 1 doom
City of the gugs: monster

BACK
FIND THE KEY
The Key of Zagan waits for you, lost somewhere in the depths of the Underworld. If Sanford can be trusted, the Key is the means by which the Lurker's sinister influence can be forever sealed off from your world.
Take three markers—two red and one green-and randomize them facedown.
Place one marker facedown at each location in the Underworld.
Action: Reveal a facedown marker at your location and suffer two damage unless you discard one clue from the scenario sheet.
Then resolve the effect below based on that marker's color.
• When you reveal a red marker, the dangers of the Underworld yield no answers; discard that marker.
• When you reveal the green marker, you find the Key of Zagan; add card 154 to the codex. Then discard all remaining markers in the Underworld and return this card to the archive.

154
LOCK THE GATE
You have been told that with the Key of Lagan in hand, you can seal away the lurking evil of Yog-Sothoth and prevent it from contaminating your world. Sanford and your allies in the Lodge have shown you the way, but you know they keep some secrets for themselves.
Throughout this entire ordeal, Sanford has told you only a part of the story and expected you to tollow his lead. What else has he retused to tell you? Have you truly been fighting Yog-Sothoth's evil, or have you instead been serving the Order of the Silver Twilight?
Add card 155 to the codex.
Action: Test {lore}-1. For each success that
you roll, move one clue from the scenario sheet to this card. If you fail, suffer two horror. You may perform this action only at the unstable space.
When there are four clues on this card, discard those clues and flip this card.

BACK
The whispering temptation of Yog-Sothoth falls suddenly silent, and in the sweet, comfortable quiet, you realize just how much that murmuring had dominated your mind-how distracted you had become. You are finally free of the presence that once lurked at the edge of your reality.
With the Gate firmly sealed, you place the Key of Zagan into a silver coffer. At Carl Sanford's urging, you tell no one—not even your allies within the Lodge— where you have hidden it away. The Lurker at the Ihreshold will gain no purchase here.
The investigators win the game!

155
CONTROL THE GATE
Can you trust Carl Sanford? You have his word that the rites you are to perform with the Key will bar the Lurker at the Threshold trom your world, but thanks to his obfuscation, you don't fully understand the magic at work here. Is he playing you for a fool, using your efforts to secure Yog-Sothoth's power for himself? What if you were to seize that power instead? Surely Arkham is safer with you in control.
Action: Discard one clue from the scenario sheet to gain a DARK PACT. You can use this action to gain a DARK PACT even if you already have one or more conditions with the same name. (Keep all of them.)
Reckoning — Resolve this effect after all other reckoning effects. If the investigators have a total of four DARK PACT conditions, flip this card.

BACK
The Key of Zagan offers a way to control and observe the influence the Lurker at the Threshold can exert on your world. Keeping your eye on the great enemy that looms at the edge of reality will leave you better equipped to face it. Carl Sanford tried to warn you against this course of action, but wouldn't he have done the same? Haven't you succeeded where he failed? While you have struggled to save Arkham, what has he done?
Only a fool would cast aside an opportunitya tool-perfect knowledge-when it is so neatly presented. Yog-Sothoth is the Key and the Gate, and the Ancient One promises the security of power and a complete knowledge of all realities.
Sanford and his pathetic Lodge are cowards— afraid to master themselves or the true secrets of the universe. You will not be so weak.
Each investigator with a DARK PACT wins the game!

156
UPON THE THRESHOLD
The gate opens-just a crack-and the whispered lure of power grows unbearably incessant. At all times, the Lurker at the Ihreshold hums in your ear, the promise of forbidden knowledge buzzing like a gnat at the edge of your perception.
Each investigator tests {will}-1. Each investigator that fails places two doom in their space unless they become CURSED.
When there is thirteen or more doom on the scenario sheet, flip this card.

BACK
With a shudder and a shriek, the gate opens fully. The inconceivable mass of Yog-Sothoth spills across the barrier that once kept it from our world. With whispers of power and promises of eternity, the Lurker at the Threshold has compelled its thralls to open the way. The shapeless, impossible form of the Outer God fills the sky, blotting out the sun and subsuming all of existence. In moments, the Beyond One merges with all of reality, and all that you know
— all that ever was and all that will ever be—
is no more.
There is only Yog-Sothoth. (The investigators lose the game.)

157
SCRAPING AT THE DOOR
Your effort to stop the malign influence that seeks to dominate the people of Arkham has drawn the attention of the Lurker at the Threshold. You feel an alien presence probing the edges of your mind for signs of weakness, and tempting you always with the lure of power and a joyful damnation.
Should you falter, you are certain the Lurker will claim you for its own.
If there are 1-3 investigators, the lead investigator gains card 158.
If there are 4 or more investigators, the lead investigator gains card 159.
When an investigator is defeated, flip this card.

BACK
Clinging to life, your trusted comrade gives in to Yog-Sothoth's relentless whispers of resurrection-and of power. A wordless voice offers them a place within the greater whole of the One-in-All. Broken, they join the chant.
"Y'AI'NG'NGAH,
YOG-SOTHOTH
H'EE-L'GEB
FAI THRODOG
UAAAH"
Add card 160 to the codex with a random side up.
Instead of returning the defeated investigator's sheet and token to the box, place their sheet under card 160 and place their token in the unstable space.
Return this card to the archive.

158
LURE OF POWER
The voice of the Lurker at the Threshold fills vit and musies. our stors to tensions
against this evil go awry, sabotaged by your own subconscious actions.
After you perform a focus action, place one doom on this card unless you suffer one direct horror.
After you perform a ward action, place one doom on this card unless you suffer one direct horror.
Reckoning — Move all doom from this card to your space. Then the investigator nearest to the unstable space gains this card and flips it.
If you are defeated, gain this card after you select a new investigator.

BACK
LURE OF SLUMBER
The voice of the Lurker at the Threshold fills your mind, pushing against your intentions with dark impulses. Your efforts to work against this evil go awry, sabotaged by your own subconscious actions.
After you perform a research action, place one doom on this card unless you suffer one direct horror.
After you perform an attack action, place one doom on this card unless you suffer one direct horror.
Reckoning — Move all doom from this card to your space. Then the investigator nearest to the unstable space gains this card and flips it.
If you are defeated, gain this card after you select a new investigator.

159
THE BEYOND ONE
The voice of the Lurker at the Threshold fills your mind, pushing against your intentions
with dark impulses. Your efforts to work against this evil go awry, sabotaged by your own subconscious actions.
After you perform a ward action, place one doom on this card unless you suffer one direct horror.
After you perform a focus action, place one doom on this card unless you suffer one direct horror.
After you perform a component action, place one doom on this card unless you suffer one direct horror.
Reckoning — Move all doom from this card to your space. Then the investigator nearest to the unstable space gains this card and flips it.
If you are defeated, gain this card after you select a new investigator.

BACK
THE LURKER'S WILL
The voice of the Lurker at the Threshold fills your mind, pushing against your intentions with dark impulses. Your efforts to work against this evil go awry, sabotaged by your own subconscious actions.
After you pertorm a research action, place one doom on this card unless you sutter one direct horror.
After you perform an attack action, place one doom on this card unless you suffer one direct horror.
After you perform a gather resources action, place one doom on this card unless you suffer one direct horror.
Reckoning — Move all doom from this card to your space. Then the investigator nearest to the unstable space gains this card and flips it.
If you are defeated, gain this card after you select a new investigator.

160
SERVE THE DARKNESS
Your former comrade, broken to the will of an ancient and unknowable evil, has now become a dire threat. Bolstered with magical rites, relentless endurance, and an unholy purpose, the lost one labors endlessly to evoke the coming of Yog-Sothoth.
The investigator token matching the sheet under this card is the "fallen one.
Reckoning — Roll one die and resolve the effect below:
1-3: Place two doom in the fallen one's space.
4-5: The investigator closest to the fallen one suffers one damage and one horror.
6: The investigator closest to the fallen one discards one focus, one clue, or one item (of their choice).
Then move the fallen one to the unstable space and flip this card.

BACK
HUNT THE LIGHT
Your former comrade, broken to the will of an ancient and unknowable evil, has now become a dire threat. A grim and hunting shadow, your one-time friend dogs your every step, seeking a chance to strike at the enemies of the Lurker at the Threshold.
The investigator token matching the sheet under this card is the "fallen one.
Reckoning — Roll one die and resolve the effect below:
1: Place two doom in the fallen one's space.
2-4: The investigator closest to the fallen one suffers one damage and one horror.
5-6: The investigator closest to the fallen one discards one focus, one clue, or one item (of their choice).
Then move the fallen one to the unstable space and flip this card.

161 (Easttown Back)
Hibb's Roadhouse
You savor a discreet drink; you or an ally may recover two sanity. You follow a man in the robes of the Order out the side entrance ({will}). If you fail, his sorcery overwhelms you; suffer two damage.
Whether you pass or not, you clear his mind and find an Elder. Return this card to the archive.
Police Station
Deputy Morgan seeks your help with a confused man who looks past you, over your shoulder ({influence}).
If you pass, he tells you how to find a lost object; gain one common item. If you fail, he reveals a grim truth; suffer one horror. Whether you pass or not, you recognize him as one of Sanford's lost allies; you find an Elder. Return this card to the archive.
Velma's Diner
You take a seat at the counter and chat with the staff over pie and coffee. You may spend $1 for you or an ally to recover two health. The waitress tells you that one of her regulars has been acting oddly. When she points him out, you recognize him as one of Sanford's missing colleagues; you find an Elder. Return this card to the archive.

162 (French Hill Back)
Bayfriar Gardens
Scraps of paper dot the crisp leaves, creating a trail that leads into the hedge maze ({influence}). If you pass, you follow the trail to the end; gain one remnant.
Whether you pass or not, you locate a woman with the signet of the Order, whispering to the sky; you find an Elder. Return this card to the archive.
Duterte Funeral Home
Samuel points out a circle carved into the cold dirt.
"Animals avoid it for some reason.
" You may spend
a remnant to help him disrupt the glyph. If you do, you may remove one doom from any space.
Whether you do or not, nearby you find a woman from the Lodge, drawing another such circle; you find an Elder. Return this card to the archive.
Silver Twilight Lodge
Within the Lodge, you lose your way in twisting hallways that double back on themselves impossibly ({lore}). It you pass, you find the library;
gain one spell. It you tail, you wander aimlessly; become FATIGUED. Whether you pass or not, you find an Elder, trapped in the same maze.
Return this card to the archive.

163 (Merchant District Back)
River Docks
Joey "the Rat" is looking to make a deal. You may spend one remnant to gain $3. As you finish with him, you both notice a man mumbling to himself and staring into a street lamp. "Friend of yours?" Joey asks, as you recognize the Lodge member; you find an Elder. Return this card to the archive.
Tick-Tock Club
A relaxing evening in the club gives you time to think. You may spend $1 for you or an ally to recover one health and one sanity. One of the musicians tells you he saw somebody painting a crude door on a wall in the alley outside. You take a look and see one of Carl Sanford's missing allies; you find an Elder. Return this card to the archive.
Unvisited Isle
A man draws the sigil of the Order in the air and begins to invoke the Lurker at the Threshold ({will}).
If you pass, you disrupt the ritual and confiscate his materials; gain one remnant. If you fail, become CURSED. Whether you pass or not, you know him to be a member of the Lodge; you find an Elder. Return this card to the archive.

164 (Rivertown back)
Black Cave
You recognize the woman in the back of the cave as a member of the Order of the Silver Twilight, and try to interpret her nonsensical rhyming ({lore}). If you pass, you find the trinket she is looking for; gain one curio. Whether you pass or not, you find an Elder. Return this card to the archive.
General Store
Nathan the delivery boy tells you that a woman defaced some of the merchandise with scrawled messages about "the Opener of the Way." You may buy one common item from the display for half price (rounded up). Whether you buy an item or not, you locate the woman and find an Elder.
Return this card to the archive.
Graveyard
A woman from the Lodge stumbles about in a daze, and you see the lurking ghoul before it can strike ({will}). If you pass, you subdue the creature; gain one remnant. If you fail, it lashes out at you as well; suffer two damage. Whether you pass or not, you snap the woman out of her stupor and find an Elder. Return this card to the archive.

165 (Uptown back)
Hangman's Hill
Something glistens within the thick patch of witchweed. Before you can look closer, a woman charges you with a shovel ({will}). If you pass, you hold your ground and return for the item; gain one curio. Whether you pass or not, you subdue her and find an Elder. Return this card to the archive.
St. Mary's Hospital
Nurse Sharon has a little time. You may spend $1 for you or an ally to recover two health. She tells you there's a woman in one of the wards who keeps tracing the sigil of the Order of the Silver Twilight on the walls. When you speak to the woman, her mind clears a bit, and you find an Elder. Return this card to the archive.
Ye Olde Magick Shoppe
A woman stares at the sundial in front of the shop, murmuring softly ({observation}). If you pass, you can tell she's reciting magical incantations; gain one spell. If you fail, her unknown words send a shiver up your spine; suffer one horror. Whether you pass or not, you gently get her attention; you find an Elder. Return this card to the archive.
