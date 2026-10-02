-- | The neighborhood encounter decks for the tiles Secrets of the Order adds.
module AH3e.Content.SecretsOfTheOrder.NeighborhoodCards (cards) where

import AH3e.Content.Tiles
import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

cards :: [CardDef]
cards = fromBox SecretsOfTheOrder (frenchHill <> theUnderworld)

card :: NeighborhoodId -> Int -> [(Text, Text, Effect)] -> CardDef
card nid n encounters =
  CardDef
    { code = CardCode (coerce nid <> "-" <> T.justifyRight 2 '0' (tshow n))
    , name = (tile nid).name <> " " <> tshow n <> "/8"
    , expansion = CoreSet
    , copies = 1
    , kind =
        NeighborhoodCard
          nid
          (Map.fromList [(spaceIdFor place, Encounter txt eff) | (place, txt, eff) <- encounters])
    }

-- | "Remove one doom from any space", which only offers the spaces holding any.
anywhere :: Int -> Effect
anywhere n = RemoveDoomFrom AnySpace (N n)

frenchHill :: [CardDef]
frenchHill =
  map
    (uncurry (card "french-hill"))
    [
      ( 1
      ,
        [
          ( "Bayfriar Gardens"
          , "You stroll through the serene conservatory gardens and suddenly find yourself at the center, facing the eerie countenance of a regal, unnamed statue (observation). If you pass, you notice something at the statue's feet; gain one curio. If you fail, you feel unnerved by the statue's cold, pupilless eyes."
          , pass Observation 0 curioItem
          )
        ,
          ( "Duterte Funeral Home"
          , "Madeline Duterte greets you at the door, saying, \"I imagine I could spare a few minutes.\" She shares some of her knowledge about current events. You may remove one doom from any space. A bell rings from the front and a stranger steps in, recognizes you, and asks about your recent discoveries. You may spend one remnant to gain an ally."
          , Seq [May "Remove one doom from any space" (anywhere 1), mayPay (SpendRemnants 1) ally]
          )
        ,
          ( "Silver Twilight Lodge"
          , "A Lodge member greets you as you approach the door, and asks you to prove that you are worthy of entry (lore). If you pass, she nods at your words and hands you a ring; gain INNER SANCTUM ACCESS. If you fail, the woman grins menacingly and you flee, your heart pounding; become FATIGUED."
          , Test Lore 0 (named "INNER SANCTUM ACCESS") fatigued
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Bayfriar Gardens"
          , "Enjoying the sweet scent of the rose bushes, you notice something hastily buried under a loose pile of dirt; gain one curio. As you reach down, you prick your finger on a thorn (will). If you fail, you watch as the blood swells and lose yourself in a morbid trance; become delayed."
          , Seq [curioItem, Test Will 0 NoEffect delayed]
          )
        ,
          ( "Duterte Funeral Home"
          , "You wander the grounds, following the old iron fence (observation). If you pass, you uncover a rusty hatch in the ground; gain HIDDEN ROUTES. You jump at a sudden voice. \"S'from the old days, when people tried to beat down their brothers,\" Samuel says sullenly. If you fail, you find a half-faced specter that wails at you to leave it alone; suffer one horror."
          , Test Observation 0 (named "HIDDEN ROUTES") (horror 1)
          )
        ,
          ( "Silver Twilight Lodge"
          , "While you inspect several old tomes, one of them flies to the ground and a glowering ghastly figure appears (influence). If you pass, you apologize for disturbing the apparition before it nods its approval and leaves something behind; gain one curio. If you fail, the creature's wails set your ears ringing for hours; become FATIGUED."
          , Test Influence 0 curioItem fatigued
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Bayfriar Gardens"
          , "A hooded woman inspects the dark blooms. \"I have answers, if you would like.\" She offers you a breathtaking, vibrant rose. You may gain a DARK PACT to gain the UNKNOWN LITURGY. If you do not, she rakes the rose's thorns across her palms, her dark blood dripping to the ground before she vanishes."
          , mayPay (CostCondition "DARK PACT") (named "UNKNOWN LITURGY")
          )
        ,
          ( "Duterte Funeral Home"
          , "Madeline Duterte's assistant, Samuel, is a burly, quiet man. He hoists a heavy-looking box onto his shoulder (observation). If you pass, you notice the box is from the library and he lets you take a quick peek at the information his employer was researching; remove one doom from any space. If you fail, you marvel at his strength and decide not to bother him."
          , pass Observation 0 (anywhere 1)
          )
        ,
          ( "Silver Twilight Lodge"
          , "The Lodge's library is extensive, you could easily spend months combing through the texts (lore). If you pass, you narrow your search and find something useful; gain one spell. If you fail, you come across an odd book and realize it's bound in pulsating flesh; suffer one horror."
          , Test Lore 0 spell (horror 1)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Bayfriar Gardens"
          , "In the hedge maze, you discover a grotesque, rotted tooth. Gain one remnant. You look around for the source (observation). If you pass, you follow a trail of damp spots to find a slender, ethereal figure dancing between the hedges, casting teeth across the dirt like seeds; gain one additional remnant."
          , Seq [remnants 1, pass Observation 0 (remnants 1)]
          )
        ,
          ( "Duterte Funeral Home"
          , "Madeline Duterte is a gracious host, pouring the tea while you ask her about the manor's history. She sighs, \"This house has many secrets, but uncovering them could leave a stain on your heart, with all the sadness in these old stones. Ghosts walk these halls, if you listen for them.\" You may become CURSED to remove up to three doom from any space."
          , mayPay (CostCondition "CURSED") (anywhere 3)
          )
        ,
          ( "Silver Twilight Lodge"
          , "You walk down a narrow corridor and are suddenly gripped by a bony hand jutting from the wall. \"Name me, name me...\" whispers a hoarse echo (lore). If you pass, you recall a worker who famously went missing during the construction of the Lodge and whisper his name; \"Thank you...\" it croaks, as you become BLESSED."
          , pass Lore 0 blessed
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Bayfriar Gardens"
          , "You walk around the ruins of the old church and encounter a screeching, ghostly figure (will). If you pass, you reach out toward the wraith, but all you find there is a still-beating heart; gain a remnant. If you fail, the phantom's wail remains with you long after you leave; become CURSED."
          , Test Will 0 (remnants 1) cursed
          )
        ,
          ( "Duterte Funeral Home"
          , "Samuel paces around the visitation room, muttering about seeing an ornery ghost on the grounds. You may spend one remnant to show him you know how to deal with them. If you do, Samuel's eyes light up and he whistles, \"I'm not too good at singin', but my mama taught me a song for good luck;\" become BLESSED."
          , mayPay (SpendRemnants 1) blessed
          )
        ,
          ( "Silver Twilight Lodge"
          , "You walk down a vacant hallway and notice someone left something peculiar behind; gain one curio. Just as you take it, another member walks in. \"You! What are you doing?\" they shout (influence). If you pass, you explain that the item was yours all along. If you fail, the member gives you a thorough thrashing; become delayed."
          , Seq [curioItem, Test Influence 0 NoEffect delayed]
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Bayfriar Gardens"
          , "The guests at a small, well-to-do wedding reception mingle in the gardens (observation). If you pass, you cross paths with a hearty woman in blue robes, handing out good-luck charms; become BLESSED. If you fail, you find a servant with a tray full of canapés, but she says they are for guests only."
          , pass Observation 0 blessed
          )
        ,
          ( "Duterte Funeral Home"
          , "A mournful Dunwich family is holding a visitation for a murdered relative. The family is asking for any information that could help them find the killer. You may spend one remnant to show them what sort of thing is responsible. If you do, one of the guests thanks you profusely and swears to help you; gain an ally."
          , mayPay (SpendRemnants 1) ally
          )
        ,
          ( "Silver Twilight Lodge"
          , "You watch as several Lodge members perform a strange ritual. Gain one spell. Afterward, you sneak in and study their texts (lore). If you pass, the knowledge is astounding; you may focus two skills of your choice. If you fail, you realize too late what they were up to when a gaunt phantasm lunges at you; suffer one damage."
          , Seq [spell, Test Lore 0 (Seq [focusAny, focusAny]) (damage 1)]
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Bayfriar Gardens"
          , "Something wraps itself around your ankle from under a nearby bush (will). If you pass, you fling yourself away, tearing free from the ghastly, still-moving fingers; gain one remnant. If you fail, you freeze as a fiendish apparition skitters out from the bush, dragging you along; suffer one damage."
          , Test Will 0 (remnants 1) (damage 1)
          )
        ,
          ( "Duterte Funeral Home"
          , "A priest speaks softly to a small group of mourners, one of whom recognizes you with a rueful smile; gain an ally. Something about the grim scene feels familiar (observation). If you pass, you recognize the deceased among the guests, and they smile softly in thanks for your visit; remove one doom from any space."
          , Seq [ally, pass Observation 0 (anywhere 1)]
          )
        ,
          ( "Silver Twilight Lodge"
          , "Carl Sanford speaks to a Lodge member in a strange tongue (lore). If you pass, you recognize the words from an old, dead language and realize that Sanford seems to be describing one of the Lodge's secret caches; gain one curio. If you fail, their esoteric rambling goes right over your head."
          , pass Lore 0 curioItem
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Bayfriar Gardens"
          , "\"Know any riddles?\" an older man in a dusty, timeworn suit asks (will). If you pass, he grins toothlessly, \"Here, for making an old, dead man smile.\" He hands you a dirty parcel and begins to fade away, chuckling; gain one curio. If you fail, he frowns, gestures rudely, and vanishes before your eyes."
          , pass Will 0 curioItem
          )
        ,
          ( "Duterte Funeral Home"
          , "A small dog yaps at you from the manor's porch. \"Hush now; hush you,\" Madeline coos. \"I found her wandering the grounds. Could you help me find her owner?\" (observation). If you pass, you find the dog's collar snagged on an iron fence and locate her grateful family, who ask if there is anything they can do to repay you; gain one ally."
          , pass Observation 0 ally
          )
        ,
          ( "Silver Twilight Lodge"
          , "The head of the Lodge, Carl Sanford, begins his speech, \"A member of the Lodge must pledge themself fully to our cause...\" You may gain a DARK PACT to demonstrate your loyalty. If you do, you are taught some of their secrets; gain two spells. If you do not, you feel numerous eyes on you and get a really bad feeling."
          , mayPay (CostCondition "DARK PACT") (Seq [spell, spell])
          )
        ]
      )
    ]

theUnderworld :: [CardDef]
theUnderworld =
  map
    (uncurry (card "the-underworld"))
    [
      ( 1
      ,
        [
          ( "City of the Gugs"
          , "You bolt across the room as the gug lets loose an unholy screech, slashing at you with a quartet of taloned arms. You could escape, or you could attempt to fight the creature with the hulking hammer that lies on a cracked stone altar. You may suffer two damage to gain the CYCLOPEAN HAMMER."
          , mayPay (CostDamage 2) (named "CYCLOPEAN HAMMER")
          )
        ,
          ( "Vale of Pnath"
          , "The dim gray of the Vale shows the mountainous, unending pile of bones at its heart, flanked by the Peaks of Thok. If you take a moment to look at the nearby bodies, you might find something useful amidst the carrion. You may suffer one horror to gain $3. Whether you do or not, something shifting under the bones prompts you to hurry away."
          , mayPay (CostHorror 1) (money 3)
          )
        ,
          ( "Vaults of Zin"
          , "You find yourself at the mouth of the Vaults, peering into its lightless depths. Your foot touches something wet: the head of a ghoul. Gain one remnant from the scattered pieces of the beast (will). If you pass, the sight terrifies and energizes you; become DRIVEN. If you fail, hopelessness overwhelms you; become FATIGUED."
          , Seq [remnants 1, Test Will 0 driven fatigued]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "City of the Gugs"
          , "A gug slumps over to rest after devouring a ghoul. Something gleams inside the gug's mouth. Test will to keep your breath calm or suffer two damage to brashly reach between its teeth. If you pass or suffer the damage, gain one curio. If you fail, the sight stains your mind; suffer one horror."
          , Choose
              [ ("Test will", Test Will 0 curioItem (horror 1))
              , ("Suffer two damage", Pay (CostDamage 2) curioItem)
              ]
          )
        ,
          ( "Vale of Pnath"
          , "A few ghouls toss gnawed corpses into a massive pit of bones, and you duck out of sight (observation). If you pass, you wait for the ghouls to leave and rummage through the tattered pockets of their victims; gain one common item. If you fail, they spot you; become FATIGUED."
          , Test Observation 0 commonItem fatigued
          )
        ,
          ( "Vaults of Zin"
          , "A group of pale, hooved ghasts rip a gug apart. They have noseless, alien faces, and make terrible, guttural noises (observation). If you pass, you realize they are performing some sort of ritual; gain one spell. If you fail, the memory of the tormented gug haunts you; become CURSED."
          , Test Observation 0 spell cursed
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "City of the Gugs"
          , "You slip into one of the cyclopean towers and find a strange, runic stone; gain one remnant. As you touch it, your mind is filled with unholy screeching (will). If you pass, you shake off the sound and keep searching; gain one curio. If you fail, the noise is inescapable; become CURSED."
          , Seq [remnants 1, Test Will 0 curioItem cursed]
          )
        ,
          ( "Vale of Pnath"
          , "You wade through a river of bones and flesh (observation). If you pass, you find a bundle of blood-soaked papers that depict, in gruesome detail, many of the monstrous terrors that face you; gain the CRYPTIC SKETCHES. If you fail, the whispers of the dead call to you, describing your transformation into a terrible creature; become CURSED."
          , Test Observation 0 (named "CRYPTIC SKETCHES") cursed
          )
        ,
          ( "Vaults of Zin"
          , "You wake in a damp, lightless cave. The unsettling chanting of the ghasts echoes around you, and you mouth the words to try to understand them. Gain one spell. There must be something here to help you get out (observation). If you pass, you feel around blindly until your hand rests on something sharp; gain one remnant."
          , Seq [spell, pass Observation 0 (remnants 1)]
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "City of the Gugs"
          , "You find yourself in a massive field of huge stone monoliths (will). If you pass, you realize this is some sort of graveyard and spot the detached talon of a gug in the ash-black dirt; gain one remnant. If you fail, you realize too late that you are not alone when a ghoul slashes at your leg; suffer one damage."
          , Test Will 0 (remnants 1) (damage 1)
          )
        ,
          ( "Vale of Pnath"
          , "You find a half-dead man moaning on the ground. He throws something at you. Gain $2. He rasps at you, \"Blood...need blood. Mine's all...gone.\" You may suffer one horror to find the man some blood. If you do, he slurps it up noisily and tosses more money at you; gain an additional $2."
          , Seq [money 2, mayPay (CostHorror 1) (money 2)]
          )
        ,
          ( "Vaults of Zin"
          , "Two ghasts hold a Nightgaunt at bay with long spears and screech unknowable words. The faceless creature tries to grab at one of the tools and a ghast lets loose something horrible upon it (will). If you pass, you think you can replicate the gestures and most of the words; gain one spell. If you fail, you flee before you see the confrontation end."
          , pass Will 0 spell
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "City of the Gugs"
          , "An impossibly large spire stretches up beyond your eyesight. You manage to make out the forms of several gugs attempting to scale the sides of the tower; one falls, landing with a heavy crack. You may become FATIGUED to run up and search the body. If you do, gain one curio."
          , mayPay (CostCondition "FATIGUED") curioItem
          )
        ,
          ( "Vale of Pnath"
          , "Something massive sends ripples through the piles of bodies, like waves on a morbid ocean. You fear that there is more to the motion than just your mind playing tricks on you, but you might be able to find something useful amongst the bodies. You may suffer two horror to gain one common item."
          , mayPay (CostHorror 2) commonItem
          )
        ,
          ( "Vaults of Zin"
          , "The cavern is devoid of light, and you worry a torch might attract your pursuers. You feel against the wall, hoping to find something, anything. You may become delayed. If you do, you eventually recognize the runic symbols that are etched into the wall and study them with your hands; gain one spell. If you do not, you run blindly."
          , mayPay CostDelayed spell
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "City of the Gugs"
          , "You see a massive, one-eyed gug pin a noseless ghast to a wall with its horrible, vertical mouth. Gain one remnant as you drop down to search through the gore it leaves behind (will). If you pass, the sight of the carnage steels your resolve; become DRIVEN. If you fail, the savagery leaves you numb."
          , Seq [remnants 1, pass Will 0 driven]
          )
        ,
          ( "Vale of Pnath"
          , "You find a bat-winged, faceless Nightgaunt struggling to free itself from under a landslide of corpses. It turns to you and waits patiently. You may gain a DARK PACT to help the beast get free. If you do, the barb-tailed monster rips itself into the air and drops something at your feet; gain one common item with a value of four or more."
          , mayPay (CostCondition "DARK PACT") (GainE (AnItemValued (Just "Common") (AtLeast 4)))
          )
        ,
          ( "Vaults of Zin"
          , "A dim glow illuminates the way through the labyrinthine cavern (will). If you pass, you cautiously approach and take the source of the light; gain the CHTHONIAN STONE. If you fail, you see visages of grotesque, thousand-eyed creatures clawing their way to the surface and flee in a blind panic; place one doom in your space."
          , Test Will 0 (named "CHTHONIAN STONE") (PlaceDoomAt YourSpace (N 1))
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "City of the Gugs"
          , "Something catches your eye on the lip of one of the towers, about twenty feet above the ground. You could climb up to see what it is, but getting down might be hard. You may suffer one damage to gain one curio. If you do not, you hurry away before one of the prowling giants spots you."
          , mayPay (CostDamage 1) curioItem
          )
        ,
          ( "Vale of Pnath"
          , "The way forward is covered in thick, viscous slime. Test observation to look for clear handholds or become delayed to muddle through. If you pass or become delayed, you manage to move ahead and find the remains of a lost traveler; gain $4. If you fail, wading through the mucus-like fluid taxes your muscles and your resolve; become FATIGUED."
          , Choose
              [ ("Test observation", Test Observation 0 (money 4) fatigued)
              , ("Become delayed", Pay CostDelayed (money 4))
              ]
          )
        ,
          ( "Vaults of Zin"
          , "You nearly slip on a piece of rough, knotted skin. Gain one remnant. You look around for its source (observation). If you pass, you find the body of a ghast, pierced through with a jagged slab of rock, and you wonder if you could make something of it."
          , remnants 1
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "City of the Gugs"
          , "You hide as a group of gugs stand together, their vertical, fanged mouths and eyestalks twitching in silent speech (will). If you pass, you decipher some of their conversation and take notes; gain one remnant. If you fail, the gathering lasts an eternity and you lose feeling in your legs; become FATIGUED."
          , Test Will 0 (remnants 1) fatigued
          )
        ,
          ( "Vale of Pnath"
          , "After being dropped in a heap of bodies by a Nightgaunt, you have finally made it to the edge of this horrific valley. Become DRIVEN. You can make out several giant pits in front of you through the dim murk. You may suffer one horror to inspect one such pit. If you do, you find something at the edge; gain one common item."
          , Seq [driven, mayPay (CostHorror 1) commonItem]
          )
        ,
          ( "Vaults of Zin"
          , "You stumble down a rough stone corridor in the gargantuan, lightless cavern. You wonder if you will ever find a way out. Test will to stay calm or become FATIGUED to push yourself onward. If you pass or gain the condition, when you finally find the dim gloom of the outside, you see awful symbols etched across your arms; gain one spell."
          , Choose
              [ ("Test will", pass Will 0 spell)
              , ("Become FATIGUED", Pay (CostCondition "FATIGUED") spell)
              ]
          )
        ]
      )
    ]
