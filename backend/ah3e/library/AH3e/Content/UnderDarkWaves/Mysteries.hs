{- | Mystery encounter decks. A mystery card reads its top effect, which names
two courses of action in bold, and then one of the two sections below it; the
secondary effects are not read before the choice is made (Under Dark Waves, p. 8).
-}
module AH3e.Content.UnderDarkWaves.Mysteries (cards) where

import AH3e.Content.Tiles
import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Text qualified as T

cards :: [CardDef]
cards = fromBox UnderDarkWaves (devilReef <> strangeHighHouse)

card :: Text -> Int -> (Text, Effect) -> [(Text, Text, Effect)] -> CardDef
card place n (intro, introEffect) branches =
  CardDef
    { code = CardCode (coerce sid <> "-" <> T.justifyRight 2 '0' (tshow n))
    , name = place <> " " <> tshow n <> "/8"
    , expansion = CoreSet
    , copies = 1
    , kind =
        MysteryCard
          MysteryDef
            { space = sid
            , opening = Encounter intro introEffect
            , branches = [(header, Encounter txt eff) | (header, txt, eff) <- branches]
            }
    }
 where
  sid = spaceIdFor place

devilReef :: [CardDef]
devilReef =
  [ card
      "Devil Reef"
      1
      ( "As you near the reef, you spot a gathering of dark figures clustered around a twisted coral altar. Gain one remnant. As their chanting intensifies, the cold wind whipping off of the eager waves intensifies. You may stay hidden to watch and learn what they're up to or stop the ritual."
      , remnants 1
      )
      [
        ( "Watch and Learn"
        , "You crouch behind a rocky outcropping and study the group's machinations (observation). If you pass, you listen in on their chanting and conversation, and learn where they plan to focus their efforts; spawn one clue. If you fail, the swirling lights they summon dance before your eyes, lulling you into a slack-jawed trance; become TAINTED."
        , Test Observation 0 SpawnOneClue tainted
        )
      ,
        ( "Stop the Ritual"
        , "Convinced that this ritual cannot be allowed to continue, you rush the cultists and kick over their altar (strength). If you pass, your sudden attack scatters the robed figures and you pick over their leavings; gain one curio. If you fail, the figures rally while you strain to topple their altar, and fall upon you en masse; suffer two damage."
        , Test Strength 0 curioItem (damage 2)
        )
      ]
  , card
      "Devil Reef"
      2
      ( "You search through some flotsam; gain one curio. In the wreck below, partially enveloped by jagged coral, you spot an old metal lockbox. The long, lean, shadow of a pair of sharks glides menacingly through the choppy water. You may dive in and brave the sharks, or wait for them to move on."
      , curioItem
      )
      [
        ( "Dive In"
        , "You try to get in and out before the predators know you're there (strength). If you pass, you glide swiftly through the cold water and return to the surface with the battered lockbox in hand; gain $4. If you fail, the shock of the cold water disorients you for a moment too long, and you only narrowly escape the sharks' jaws; suffer two damage."
        , Test Strength 0 (money 4) (damage 2)
        )
      ,
        ( "Wait"
        , "You watch from your boat and wait for the sharks to move on to other hunting grounds (will). If you pass, they eventually swim away to look for prey further down the reef and you swim down to claim your prize; gain $4. If you fail, you focus on their jagged teeth and relentless circling; suffer two horror."
        , Test Will 0 (money 4) (horror 2)
        )
      ]
  , card
      "Devil Reef"
      3
      ( "A small sailboat has run aground on the jagged reef, where a traveler, his wool suit damp with sea spray, nervously offers to pay for a rescue. Gain $2. You may interrupt your search to help him to shore, or let him wait while you pick through the larger sunken wrecks that surround the treacherous reef."
      , money 2
      )
      [
        ( "Help Him"
        , "It quickly becomes clear that the trip to the docks will take more time than you'd hoped. You may become delayed to bring him into the harbor. If you do, he passes the time and tells you about the harrowing things he saw while he was stuck out on the reef; spawn one clue. If you do not, his face falls and he waits for an actual rescue."
        , mayPay CostDelayed SpawnOneClue
        )
      ,
        ( "Let Him Wait"
        , "You wave off the water-logged traveler and set about scouring the wreck under the reef for valuables (observation). If you pass, you find a handful of coins and jewels scattered through the wreckage; gain $4. If you fail, when you emerge from the water, only the man's battered bowler hat is left behind; spread doom once."
        , Test Observation 0 (money 4) SpreadDoomOnce
        )
      ]
  , card
      "Devil Reef"
      4
      ( "A dark thunderhead bears down upon you. Spawn one clue. Weathering the sudden storm in your small craft will be dangerous, but you aren't certain you'll make it into the harbor before you get caught in the maelstrom. You may race into port or batten down the hatches to outlast the storm."
      , SpawnOneClue
      )
      [
        ( "Race into Port"
        , "You open the sail as wide as you dare and ride a dangerous tailwind towards the harbor (will). If you pass, you tie up your boat before the storm crashes over you; exhilarated by your mad dash to the dock, you or an ally may recover three sanity. If you fail, the line slips from your grasp and you are caught in the storm; suffer one damage and one horror."
        , Test Will 0 (sanity 3) (harm 1 1)
        )
      ,
        ( "Batten Down the Hatches"
        , "You take in your sail and anchor your craft away from the stony reef, praying the storm doesn't dash you about too terribly (will). If you pass, you ride out the storm and search through the flotsam it left on the rocks; gain one curio. If you fail, you strike your head when the storm tosses you around and lose consciousness; become delayed."
        , Test Will 0 curioItem delayed
        )
      ]
  , card
      "Devil Reef"
      5
      ( "A small boat lists awkwardly in the water just beyond the reef, ominous smoke drifting from below decks. You find gold coins strewn across the deck. Gain $3. There isn't much time before the ship sinks completely. You may search the cabin for survivors or search the hold for useful cargo."
      , money 3
      )
      [
        ( "Search the Cabin"
        , -- "open the it" is the card's own typo, kept as printed
          "The cabin door is stuck shut, but you can hear a feeble call from the other side (observation). If you pass, the door springs open and you free a woman who is only too happy to pay you for the timely rescue; gain one curio. If you fail, the seawater swells over the deck and swallows the door before you can open the it; suffer two horror."
        , Test Observation 0 curioItem (horror 2)
        )
      ,
        ( "Search the Hold"
        , "Wading in knee-deep water in the leaking hold, you spot a crate covered in a spatter of dark, oily blood (lore). If you pass, you recognize the corrupted viscera and wisely take the time to clean it away before you touch the crate; gain one curio. If you fail, you fear the rising water and rashly seize the marked goods; become TAINTED."
        , Test Lore 0 curioItem tainted
        )
      ]
  , card
      "Devil Reef"
      6
      ( "Something knocks against the hull of your boat. When you peer over the edge of your ship, you spot a dozen finned, humanoid shapes headed towards Innsmouth harbor. Spawn one clue. You may fight the Deep Ones to drive them back, or attempt to lure them away from the town."
      , SpawnOneClue
      )
      [
        ( "Drive Them Back"
        , "Striking out with an oar, you goad some of the creatures to turn away from their course to come after you instead (strength). If you pass, you swing your weapon again and again, driving the beasts away; gain one curio from the clawed grasp of a fallen Deep One. If you fail, there are more of the creatures than you anticipated; suffer two damage."
        , Test Strength 0 curioItem (damage 2)
        )
      ,
        ( "Lure Them Away"
        , "You shudder to think what havoc these beasts could wreak on the unsuspecting townsfolk (will). If you pass, you call out to the Deep Ones and let them chase you instead; become BLESSED. If you fail, your voice catches in your throat and you try not to think about the suffering these creatures will cause; spread doom once."
        , Test Will 0 blessed SpreadDoomOnce
        )
      ]
  , card
      "Devil Reef"
      7
      ( "With a crunch, your boat runs aground, scraping red coral from the jagged reef. Gain one remnant. As water leaks up through the splintered hull, you scramble up onto the rocks and look nervously out over the choppy water to the distant shoreline. You may attempt to swim to safety or signal for help."
      , remnants 1
      )
      [
        ( "Swim For It"
        , "As you plunge into the darkening sea, you spot a huge shadow lurking beneath you (strength). If you pass, you keep ahead of the huge beast, avoiding its grasping arms and bringing news of the creature to your colleagues; spawn one clue. If you fail, it pulls you beneath the waves and tears at your flesh before you wriggle free; suffer two damage."
        , Test Strength 0 SpawnOneClue (damage 2)
        )
      ,
        ( "Signal For Help"
        , "A boat with black sails appears behind you. The shrewd woman with dark glasses offers you transport and a gift in exchange for a small favor. You may gain a DARK PACT to gain two curios. If you refuse, you wait through the dark night for a more mundane vessel to bring you to safety; suffer one horror."
        , MayPay (CostCondition "DARK PACT") (Seq [curioItem, curioItem]) (horror 1)
        )
      ]
  , card
      "Devil Reef"
      8
      ( "When you pull up alongside the small struggling boat, you see the lone occupant clutching a wound in his belly. He gestures wildly with a small knife, and rants about \"sea-devils.\" Spawn a clue. His eyes flick towards a badly-hidden foot locker. You may attempt to help the smuggler or overpower him."
      , SpawnOneClue
      )
      [
        ( "Help the Smuggler"
        , "You try to calm the wounded man so that you can approach him safely, but he seems truly panicked by the things he's seen (influence). If you pass, after you persuade him to set down the knife, you treat his wounds and ask him about the thing that attacked him; spawn one clue. If you fail, he lashes out and drives you away; suffer two damage."
        , Test Influence 0 SpawnOneClue (damage 2)
        )
      , -- the curio comes whether the test passed or not, so it rides behind the test

        ( "Overpower Him"
        , "You board the small vessel and move to wrench the weapon out of his hand (strength). If you fail, he slashes wildly with the damaged weapon, and rakes the blade heavily across your arms and hands; suffer two damage. Whether you pass or not, you are able to subdue the wounded man and open the battered locker; gain one curio."
        , Seq [Test Strength 0 NoEffect (damage 2), curioItem]
        )
      ]
  ]

strangeHighHouse :: [CardDef]
strangeHighHouse =
  [ card
      "Strange High House"
      1
      ( "Your attempt to scale the stone ledges of Kingsport Head is stymied by the wind, but something is reacting to your presence. Remove one doom from any space. The wind threatens to tear you from the hard gray stone. You may find a cave to take shelter or put your head down and power through."
      , RemoveDoomFrom AnySpaceWithDoom (N 1)
      )
      [
        ( "Find a Cave"
        , "Safe from the wind, you find strange text scrawled over the interior of this cave by an unknown hand. You may become delayed to stay in this cavern and decode the script. If you do, gain one spell. If you refuse, you leave the cave when the wind dies down, only to find no trace of it when you turn to look back again."
        , mayPay CostDelayed spell
        )
      ,
        ( "Power Through"
        , "Undeterred by the trial before you, you crawl through the wind toward the beckoning fire that lights the High House (strength). If you pass, William Bain is impressed that you managed to reach the place; become BLESSED. If you fail, a sudden gust rips you from the stone and into the foggy air; move directly to the unstable space."
        , Test Strength 0 blessed (MoveDirectlyTo TheUnstableSpace)
        )
      ]
  , card
      "Strange High House"
      2
      ( "As you scale the treacherous rocks of Kingsport Head, you see a softly glowing lantern bobbing along through the mist. Spawn one clue. You hear soft voices, but cannot see very far through the fog. You may attempt to follow the light, or stay quiet until it passes you by."
      , SpawnOneClue
      )
      [
        ( "Follow the Light"
        , "You carefully follow the light, but the swirling fog threatens to disorient you (will). If you pass, you calmly greet three tall priests in long blue robes; become BLESSED when the servants of Nodens bestow their master's favor upon you. If you fail, you lose your way completely, and the fog deposits you far away; move directly to the unstable space."
        , Test Will 0 blessed (MoveDirectlyTo TheUnstableSpace)
        )
      ,
        ( "Stay Quiet"
        , "With stories of will-o'-the-wisps in mind, you stay quiet until the lights move past (observation). If you pass, you watch a trio of tall, blue-robed figures glide harmlessly past you in the fog; remove one doom from any space. If you fail, the figures find you lurking in the fog, and pronounce their judgment upon you; become CURSED."
        , Test Observation 0 (RemoveDoomFrom AnySpaceWithDoom (N 1)) cursed
        )
      ]
  , card
      "Strange High House"
      3
      ( "William Bain is collapsed on the floor of the odd old house. As you rush to his aid, you see deep claw marks across his back. \"It might still be here,\" he warns, \"learn these words before it returns.\" Gain one spell. You may treat his wounds or drive off the shadow that attacked him."
      , spell
      )
      [
        ( "Treat His Wounds"
        , "The curious old man waves off your attempts to treat his wounds with modern medicine, and instead requests an ancient remedy. You may spend one remnant to apply the poultice he describes to his wounds. If you do, Bain recovers swiftly, and bestows upon you the favor of the White Ship; become BLESSED."
        , mayPay (SpendRemnants 1) blessed
        )
      ,
        ( "Drive Off The Shadow"
        , "Standing guard over the fallen man, you try to recall the ancient words that will force his attacker into the light (lore). If you pass, the beast howls with rage and dives away through the open door; remove one doom from any space. If you fail, it surprises and overruns you in a rush of black feathers; suffer one damage and one horror."
        , Test Lore 0 (RemoveDoomFrom AnySpaceWithDoom (N 1)) (harm 1 1)
        )
      ]
  , card
      "Strange High House"
      4
      ( "The woman on the path with you tells you all about the things she saw in the town below. Spawn one clue. As you climb, you are both harried by shadowy creatures swooping out of the dark night. You may attempt to fight the beasts or shield the woman with your own body while she gets to safety."
      , SpawnOneClue
      )
      [
        ( "Fight The Beasts"
        , "With a shout, you attempt to startle the nightgaunts and force them to flee (strength). If you pass, you land one solid blow and they take wing to find easier prey; remove one doom from any space. If you fail, they get past you and seize the woman, dragging her out over the bay far below; suffer two horror as you watch her fall."
        , Test Strength 0 (RemoveDoomFrom AnySpaceWithDoom (N 1)) (horror 2)
        )
      ,
        ( "Shield the Woman"
        , "The nightgaunts swarm around you while you buy the woman time to escape. Suffer one damage. When you see she is free, you look for an opportunity to get away yourself (will). If you pass, the grateful and learned traveler teaches you well, gain one spell. If you fail, you don't find an opening; suffer two additional damage."
        , Seq [damage 1, Test Will 0 spell (damage 2)]
        )
      ]
  , card
      "Strange High House"
      5
      ( "You send an offering to Nodens while you wait for William Bain to return. Remove one doom from any space. You hear a loud rasping shriek as something scrapes its heavy claws across the wooden door. You may throw open the door and drive it away or remain hidden until Bain returns."
      , RemoveDoomFrom AnySpaceWithDoom (N 1)
      )
      [
        ( "Drive it Away"
        , "You place your hand on the door's handle and steel your nerves (will). If you pass, you startle the nightgaunt that lurks beyond the portal and drive it off with a blow to the head; become BLESSED. If you fail, you hesitate, and the nightgaunt seizes you roughly by the arm before flying into the mist-shrouded night; move directly to the unstable space."
        , Test Will 0 blessed (MoveDirectlyTo TheUnstableSpace)
        )
      ,
        ( "Remain Hidden"
        , "You slide behind a heavy desk and keep still when the door swings open (observation). If you pass, you stay quiet when the intruder quickly searches the room; gain one spell when it knocks a pile of papers to the floor and reveals an arcane diagram. If you fail, the nightgaunt spots you with a blood-curdling screech and flies off; suffer two horror."
        , Test Observation 0 spell (horror 2)
        )
      ]
  , card
      "Strange High House"
      6
      ( "From outside the house, you see William Bain through an old and warped window, speaking to a woman concealed by a dark hood. Spawn one clue. You may attempt to surreptitiously listen in on their conversation or knock on the glass and get their attention."
      , SpawnOneClue
      )
      [
        ( "Listen In"
        , "Bain converses with his mysterious guest in a language you cannot hope to understand, but something in the rhythm of her words stands out to you (observation). If you pass, you mouth the words only to find that they have power; gain one spell. If you fail, her words conjure images of your own grisly demise; suffer two horror."
        , Test Observation 0 spell (horror 2)
        )
      ,
        ( "Knock on the Glass"
        , "At your knock, the woman locks her golden eyes upon you and bids you speak (influence). If you pass, the Priestess of Bast returns your courtesy and tells you a little of your fate; remove one doom from any space. If you fail, the woman hisses in an unknown tongue and your mind grows cloudy before she vanishes; become CURSED."
        , Test Influence 0 (RemoveDoomFrom AnySpaceWithDoom (N 1)) cursed
        )
      ]
  , card
      "Strange High House"
      7
      ( "On the high rocks, enveloped in otherworldly fog, you may focus two skills of your choice, even if it exceeds your limit. You see the sails and rigging of a ship just below you. You've climbed so high; surely the water cannot be so near? You may call out to the crew of the vessel or attempt to sneak aboard."
      , May "Focus two skills, even beyond your limit" (Seq [focusExceed, focusExceed])
      )
      [
        ( "Call Out"
        , "When the sailors react to your call, you take stock of their odd clothing and introduce yourself (influence). If you pass, the crew of the White Ship welcomes you aboard and offers their help; remove one doom from any space. If you fail, the crew casts you back onto the rocks and the ship heaves out of view; suffer two damage."
        , Test Influence 0 (RemoveDoomFrom AnySpaceWithDoom (N 1)) (damage 2)
        )
      ,
        ( "Sneak Aboard"
        , "You drop onto the deck as quietly as you can, and are startled to see the crew conversing wordlessly with a nightgaunt (will). If you pass, you watch quietly and learn how the crew of the White Ship manipulates reality; gain one spell. If you fail, the nightgaunt howls when it hears your sharp intake of breath; suffer two horror."
        , Test Will 0 spell (horror 2)
        )
      ]
  , card
      "Strange High House"
      8
      ( "While you rest at the warm hearth in the house atop Kingsport Rock, a gust of cold wind slams the heavy door open and snuffs out the fire. Spawn one clue. You suddenly feel particularly vulnerable in this cold and drafty house. You may rush to close the door or attempt to relight the fire."
      , SpawnOneClue
      )
      [
        ( "Close the Door"
        , -- the card prints "in any space" here, where its siblings print "from"
          "You run to close the door, and feel something pushing against the old, scarred wood (strength). If you pass, you force the door closed and drop the heavy iron bar, preventing the beast from reaching your world; remove one doom in any space. If you fail, heavy black limbs snake around the edges of the door and rend your skin; suffer two damage."
        , Test Strength 0 (RemoveDoomFrom AnySpaceWithDoom (N 1)) (damage 2)
        )
      ,
        ( "Relight the Fire"
        , "You hurry to the fireplace to hold the dark and chill at bay (lore). If you pass, you read the incantation over the hearth and the fire blazes to life; gain one spell when the text dances before your eyes, revealing a new incantation. If you fail, the darkness surrounds you; become CURSED."
        , Test Lore 0 spell cursed
        )
      ]
  ]
