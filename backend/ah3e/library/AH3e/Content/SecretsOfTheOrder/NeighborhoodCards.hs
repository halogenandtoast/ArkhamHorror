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
cards = fromBox SecretsOfTheOrder (frenchHill <> theUnderworld <> concat extras)

-- | A card of one of the two decks this box brings, both of which hold eight.
card :: NeighborhoodId -> Int -> [(Text, Text, Effect)] -> CardDef
card nid n = cardOf nid n (tshow n <> "/8")

{- | A card added to a deck Arkham already had. The box numbers its own pair 1/2 and
2/2, as its cards are printed, while their codes carry on from the core set's eight.
-}
extra :: NeighborhoodId -> Int -> [(Text, Text, Effect)] -> CardDef
extra nid n = cardOf nid (8 + n) (tshow n <> "/2")

cardOf :: NeighborhoodId -> Int -> Text -> [(Text, Text, Effect)] -> CardDef
cardOf nid n printed encounters =
  CardDef
    { code = CardCode (coerce nid <> "-" <> T.justifyRight 2 '0' (tshow n))
    , name = (tile nid).name <> " " <> printed
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

{- | The two cards this box adds to each of Arkham's own neighborhoods, numbered on
from the eight the core set deals.
-}
extras :: [[CardDef]]
extras =
  [ downtown
  , easttown
  , merchantDistrict
  , miskatonicUniversity
  , northside
  , rivertown
  , southside
  , uptown
  ]

downtown :: [CardDef]
downtown =
  map
    (uncurry (extra "downtown"))
    [
      ( 1
      ,
        [
          ( "Arkham Asylum"
          , "Wandering the halls, you come across a dusty, serene chapel. Become BLESSED. Resting on the altar is an ornate, gold-bound book (will). If you pass, the strange passage offers you hope; you or an ally may recover two sanity. If you fail, the alien language drains you; become FATIGUED."
          , Seq [blessed, Test Will 0 (sanity 2) fatigued]
          )
        ,
          ( "Independence Square"
          , "An ephemeral man in a cheap, moldy-smelling suit waves you over. \"Looks like you could use a bit of help, friend.\" His yellowed teeth look like they are about to fall out of his head. You may buy any number of common items from the display. If you buy anything, the man's smile becomes unsettlingly wide, \"Don't forget this;\" gain the NINE OF RODS."
          , BuyFromDisplay (Just "Common") FullPrice Nothing (named "NINE OF RODS")
          )
        ,
          ( "La Bella Luna"
          , "After a streak of good luck, you decide to quit while you are ahead. As you try to leave, one of the patrons from whom you won money confronts you with a pit boss. \"You cheated me, and I want my money back,\" the man growls (influence). If you pass, the bouncer believes your version of events; gain $3 and become DRIVEN."
          , pass Influence 0 (Seq [money 3, driven])
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Arkham Asylum"
          , "The nurse speaks to you soothingly, \"I have your medication, dear. Make sure you drink every drop.\" You may spend $1 for you or an ally to recover two sanity. If you do, the concoction has lasting effects; become DRIVEN."
          , mayPay (SpendMoney 1) (Seq [sanity 2, driven])
          )
        ,
          ( "Independence Square"
          , "As you pass by the shops you see a little girl with a Dunwich accent darting through the crowd. She seems to be hiding from something (will). If you pass, you recognize the game she is playing and decide to play with her. She is delighted and hands you a gift before giggling and running off; gain one curio and become DRIVEN."
          , pass Will 0 (Seq [curioItem, driven])
          )
        ,
          ( "La Bella Luna"
          , "The dice are hot tonight and you're on a roll. Peter Clover cocks an eyebrow from the corner, seemingly interested in whether you have the guts to keep going. You may spend $1 to gamble. If you do, roll four dice; gain $1 for each odd number you roll. If you do not spend the money, he shakes his head in disappointment."
          , mayPay (SpendMoney 1) (Custom "la-bella-luna-dice")
          )
        ]
      )
    ]

easttown :: [CardDef]
easttown =
  map
    (uncurry (extra "easttown"))
    [
      ( 1
      ,
        [
          ( "Hibb's Roadhouse"
          , "An old woman starts to sing a jaunty tune, and soon most of the patrons have joined in, filling the old barn with song. You or an ally may recover two sanity. As you sing, you start to lose track of time (observation). If you fail, before you know it, it's already morning; become FATIGUED."
          , Seq [sanity 2, Test Observation 0 NoEffect fatigued]
          )
        ,
          ( "Police Station"
          , "As you pass by a holding cell you see a woman with long black hair frantically scrawling strange words onto the floor (influence). If you pass, she rushes over to you and says in a hurried whisper, \"It's to keep Arkham safe. Here, I know a rune that can help.\" She scribbles black smudges onto the back of your hand; become BLESSED."
          , pass Influence 0 blessed
          )
        ,
          ( "Velma's Diner"
          , "You arrive at Velma's two minutes before close and she tuts at your haggard appearance, \"I suppose I can't turn you away looking like that, but it'll cost you.\" You may spend $1. If you do, Velma makes some hot turkey sandwiches and warm tea; you become DRIVEN and you or an ally recovers two health."
          , mayPay (SpendMoney 1) (Seq [driven, health 2])
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Hibb's Roadhouse"
          , "The woman behind the counter quietly suggests a unique vintage she thinks you might like to try. You may spend $2 for you or an ally to recover two sanity. If you do, she commends your taste; become DRIVEN. If you do not, she tuts her disappointment at you and sets the bottle back on the top shelf."
          , mayPay (SpendMoney 2) (Seq [sanity 2, driven])
          )
        ,
          ( "Police Station"
          , "You notice a sleek, dark figure lurking in the reception area (observation). If you pass, you realize it's just a coat rack, and you find something in the pocket of one of the coats; gain one common item. If you fail, you see red eyes peering at you from beneath the wide brim of a hat and the vision haunts your dreams; become FATIGUED."
          , Test Observation 0 commonItem fatigued
          )
        ,
          ( "Velma's Diner"
          , "You have a delicious fish dinner as you chat up the other patrons. You or an ally may recover two health. You notice a tired-looking man sitting alone (influence). If you pass, you listen to the man's woes and he introduces himself as Jasper Best, a local cab driver; gain the CABBIE'S FAVOR."
          , Seq [health 2, pass Influence 0 (named "CABBIE'S FAVOR")]
          )
        ]
      )
    ]

merchantDistrict :: [CardDef]
merchantDistrict =
  map
    (uncurry (extra "merchant-district"))
    [
      ( 1
      ,
        [
          ( "River Docks"
          , "Johnny \"the Don\" Valone looms out of the dark. \"Yeah, you. I heard you got something I might be wantin'. Let's trade, yeah?\" You may spend a remnant to gain one common item. If you do not, he whistles and a crew of guys step out of the shadows; become FATIGUED when you run."
          , MayPay (SpendRemnants 1) commonItem fatigued
          )
        ,
          ( "Tick-Tock Club"
          , "The smooth music and hot food are filling. You or an ally may recover one health and one sanity. As you tap your foot to the rhythm, something sounds...off (observation). If you pass, you find the source of the noise is a small clock. \"Take it,\" calls out \"Dainty\" Donohue, \"the thing has never worked right;\" gain THE RED CLOCK."
          , Seq [RecoverBoth YouOrAlly (N 1) (N 1), pass Observation 0 (named "THE RED CLOCK")]
          )
        ,
          ( "Unvisited Isle"
          , "A dozen yellow eyes peer at you from the trees (will). If you pass, you refrain from showing your fear and call out calmly, and after a moment of unsettling chittering, something sleek and translucent thuds to the ground; gain one remnant and become DRIVEN. If you fail, you flee from the unblinking sentries."
          , pass Will 0 (Seq [remnants 1, driven])
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "River Docks"
          , "Some shady individuals are loading heavy crates into the back of a car. One of them calls out to you, \"Hey, you, wanna make a buck?\" Gain $3. As you heft a box into the trunk, a thick slime seeps out onto your hands (will). If you pass, you steel yourself and finish the job; become DRIVEN."
          , Seq [money 3, pass Will 0 driven]
          )
        ,
          ( "Tick-Tock Club"
          , "You treat yourself to a fine meal and a cold drink. You or an ally may recover two health. \"Hey, you,\" the bartender calls, \"My busboy called in sick. You do some work for me, and I'll keep the food and booze coming. How about it?\" You may become FATIGUED for you or an ally to recover two sanity."
          , Seq [health 2, mayPay (CostCondition "FATIGUED") (sanity 2)]
          )
        ,
          ( "Unvisited Isle"
          , "The air feels thick as you reach the base of an impossibly massive tree, covered from top to bottom in carved phrases in a dozen languages from this world and the next (lore). If you pass, you find a pattern and feel rejuvenated as you recite the words; become BLESSED. If you fail, the hot air weighs on you; become FATIGUED."
          , Test Lore 0 blessed fatigued
          )
        ]
      )
    ]

miskatonicUniversity :: [CardDef]
miskatonicUniversity =
  map
    (uncurry (extra "miskatonic-university"))
    [
      ( 1
      ,
        [
          ( "Observatory"
          , "As you sift through the notes from the last few weeks, you discover that the stars are aligned over a specific place in a very specific configuration. You see a way to protect that spot from eldritch threats. You may become FATIGUED to remove one doom from any space."
          , mayPay (CostCondition "FATIGUED") (anywhere 1)
          )
        ,
          ( "Orne Library"
          , "You page through the library's newspaper archive from the last several years. You notice a pattern of letters in the corners of some of the pages (lore). If you pass, you are able to decipher the secret text; gain one spell and become DRIVEN."
          , pass Lore 0 (Seq [spell, driven])
          )
        ,
          ( "Science Building"
          , "Scanning the laboratory, you find the cabinet you were looking for, but it is heavily padlocked. It will take you a long time to break through it or to find a key. You may become FATIGUED to find a way in. If you do, the notes you uncover give you hope; become BLESSED."
          , mayPay (CostCondition "FATIGUED") blessed
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Observatory"
          , "As you peer through the lens you see something falling from the sky (observation). If you pass, you are able to watch closely enough to perform the necessary calculations and find the site of a fallen comet; gain one remnant and become DRIVEN."
          , pass Observation 0 (Seq [remnants 1, driven])
          )
        ,
          ( "Orne Library"
          , "You find a book wedged behind a shelf. Gain the BOOK OF SHADOWS. You open to the first page (will). If you pass, you realize the foreword has a hidden incantation; gain one spell. If you fail, you spend all night searching for answers in the mysterious book; become FATIGUED."
          , Seq [named "BOOK OF SHADOWS", Test Will 0 spell fatigued]
          )
        ,
          ( "Science Building"
          , "A group of students exiting a lab pass by you. One stays back a bit and nervously whispers to you, \"I heard you might have something I could use for my project, is that true?\" You may spend one remnant to gain $2 and become DRIVEN."
          , mayPay (SpendRemnants 1) (Seq [money 2, driven])
          )
        ]
      )
    ]

northside :: [CardDef]
northside =
  map
    (uncurry (extra "northside"))
    [
      ( 1
      ,
        [
          ( "Arkham Advertiser"
          , "The Advertiser is looking for an errand-runner. Gain $2. As you bring her a coffee, Minnie Klein flashes you a smile. \"Hey there, you look sharp. Would you take a look at my notes (will)?\" If you pass, the notes include details that give you a renewed sense of urgency; become DRIVEN."
          , Seq [money 2, pass Will 0 driven]
          )
        ,
          ( "Curiositie Shoppe"
          , "\"I have a few items on sale today,\" Oliver Thomas whispers. You may buy one curio from the display for half price (rounded up). As you leave the shoppe, you spot a small but interesting brass statuette (will). If you pass, the trinket fills you with energy; become DRIVEN. If you fail, you have terrible visions as it clatters to the ground; become FATIGUED."
          , Seq [buyOneHalf "Curio", Test Will 0 driven fatigued]
          )
        ,
          ( "Train Station"
          , "Some important files are supposed to arrive on the next train, but it is nearly thirty minutes late (will). If you pass, you decide to pass time by making small talk with a stranger; gain one ally. If you fail, the train takes an eternity to arrive and you fidget on the uncomfortable bench for hours; become FATIGUED."
          , Test Will 0 ally fatigued
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Arkham Advertiser"
          , "A woman stumbles into the Advertiser claiming to have seen an apparition, but Editor Doyle Jefferies thinks she's delirious. You may spend one remnant to back up her claims. If you do, she thanks you profusely; become BLESSED. If you do not, her sobs weigh heavily upon you; become FATIGUED."
          , MayPay (SpendRemnants 1) blessed fatigued
          )
        ,
          ( "Curiositie Shoppe"
          , "You step into the shop to get out of the rain and are greeted by a soft voice. \"Welcome, is there anything I can help you find?\" A slight man waves his hands at the shelves of trinkets. You may spend $3. If you do, you find a particularly intriguing object; gain one curio and become DRIVEN."
          , mayPay (SpendMoney 3) (Seq [curioItem, driven])
          )
        ,
          ( "Train Station"
          , "An old woman lets out a grunt of frustration. A crossword sits on her lap (will). If you pass, you help the woman with the questions she was struggling with and another passenger appreciates your efforts; gain one ally. If you fail, listening to the woman complain about the puzzle is mind-numbing; become FATIGUED."
          , Test Will 0 ally fatigued
          )
        ]
      )
    ]

rivertown :: [CardDef]
rivertown =
  map
    (uncurry (extra "rivertown"))
    [
      ( 1
      ,
        [
          ( "Black Cave"
          , "A cold wind suddenly extinguishes your lantern. You could turn around and leave the way you came, but a tiny light in the distance seems to call to you. You may become FATIGUED to travel through the cave and gain a GUIDING SPIRIT."
          , mayPay (CostCondition "FATIGUED") (named "GUIDING SPIRIT")
          )
        ,
          ( "General Store"
          , "The delivery boy, Nathan, greets you as you enter. \"Mr. Schoffner is sick today, but he said that ain't an excuse to close.\" You may buy any number of common items from the display. If you buy anything, Nathan quietly thanks you for all of your hard work; become DRIVEN."
          , BuyFromDisplay (Just "Common") FullPrice Nothing driven
          )
        ,
          ( "Graveyard"
          , "You come across a damaged monument. You spend the time to heft the heavy pieces up and attempt to fix it (strength). If you pass, a withered visage appears as you finish, nods its eyeless head to you and extends a gnarled hand; gain $3. If you fail, you forget to lift with your legs and hear an unfortunate pop; become FATIGUED."
          , Test Strength 0 (money 3) fatigued
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Black Cave"
          , "As you make your way quickly through the cave, you notice hastily-sketched symbols scattered across the ceiling (lore). If you pass, you realize the drawings point to a secret cache with a few ancient tomes inside; gain one spell and become DRIVEN."
          , pass Lore 0 (Seq [spell, driven])
          )
        ,
          ( "General Store"
          , "Davy Schoffner waves you over when he sees you enter the shop. \"Nathan is bogged down with orders today. If you have a bit of time to help out around the place, I can make it worth your while.\" You may become FATIGUED to gain one common item."
          , mayPay (CostCondition "FATIGUED") commonItem
          )
        ,
          ( "Graveyard"
          , "You hear noises that are a cross between whimpering and gagging coming from behind a headstone (will). If you pass, you avoid looking at the cause of the sounds and instead recite a lullaby and the sobs soon turn to gentle laughter; become BLESSED. If you fail, the weeping specter vanishes as soon as you look at it."
          , pass Will 0 blessed
          )
        ]
      )
    ]

southside :: [CardDef]
southside =
  map
    (uncurry (extra "southside"))
    [
      ( 1
      ,
        [
          ( "Historical Society"
          , "You participate in a riveting debate and feel exhilarated. Become DRIVEN. One of the debaters refutes your claim and declares your point moot without proof. You may spend a remnant. If you do, another patron is impressed and asks if you want to collaborate; gain one ally."
          , Seq [driven, mayPay (SpendRemnants 1) ally]
          )
        ,
          ( "Ma's Boarding House"
          , "Ma hums a gruff tune as she works in the kitchen. Just the smell of the food makes you feel better. You or an ally may recover two health. \"Food's only for those who got a room,\" she snaps. You may spend $1 for you or an ally to recover two health. If you do, it tastes as good as it smells; become DRIVEN."
          , Seq [health 2, mayPay (SpendMoney 1) (Seq [health 2, driven])]
          )
        ,
          ( "South Church"
          , "Father Michael begins his sermon (will). If you pass, a hunched, shockingly pale old woman approaches you, saying \"It's good to see there is still faith in this city\8212you stay safe,\" gain THE HIEROPHANT. She nods approvingly and walks away\8212straight through a wall. If you fail, the sermon brings you little comfort in these dire times."
          , pass Will 0 (named "THE HIEROPHANT")
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Historical Society"
          , "A wet-eyed boy whines from behind his mother's skirt (influence). If you pass, you cleverly entertain him and a woman in a white robe nods at you with approval; become BLESSED. If you fail, the boy begins to cry and wail; become FATIGUED when his squalling triggers a migraine."
          , Test Influence 0 blessed fatigued
          )
        ,
          ( "Ma's Boarding House"
          , "Ma stirs the massive stew pot and adds a bit more salt, the delicious aroma filling the room. \"You want some, don't you? Well, you best get those chores done quick. We work for our meals here!\" she laughs. You may become FATIGUED for you or an ally to recover four health."
          , mayPay (CostCondition "FATIGUED") (health 4)
          )
        ,
          ( "South Church"
          , "Father Michael looks at you warmly. \"You are doing the works of a virtuous person, my child. We are truly grateful to have you among us.\" Become DRIVEN. \"May I ask,\" he continues, \"that you remember the church, should those good works bear fruit?\" You may spend $1 for you or an ally to recover two sanity."
          , Seq [driven, mayPay (SpendMoney 1) (sanity 2)]
          )
        ]
      )
    ]

uptown :: [CardDef]
uptown =
  map
    (uncurry (extra "uptown"))
    [
      ( 1
      ,
        [
          ( "Hangman's Hill"
          , "A morose, ghostly child draws your attention to a tight nook in the roots of a gnarled and twisted tree (strength). If you pass, you break away the thick bark and cold earth to find a long-forgotten book; gain the LOST JOURNAL. If you fail, the girl giggles as you search fruitlessly before disappearing."
          , pass Strength 0 (named "LOST JOURNAL")
          )
        ,
          ( "St. Mary's Hospital"
          , "Doctor Mortimore walks into the waiting room, whistling a cheerful tune and gestures for you to follow him. \"Banged up a bit, aren't you? I have a new medication\8212cheers you right up. Let's give it a try?\" You may spend $1 to become DRIVEN and for you or an ally to recover two health."
          , mayPay (SpendMoney 1) (Seq [driven, health 2])
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "\"No, no, that isn't right!\" Miriam Beecher rushes over. \"Here, let me show you before you get yourself into trouble I can't fix.\" Gain one spell. Miriam continues her lesson, but she speaks very quickly (will). If you pass, you manage to keep up and the newfound knowledge invigorates you; become DRIVEN."
          , Seq [spell, pass Will 0 driven]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Hangman's Hill"
          , "A flash of dry lightning cuts across the sky, illuminating swaying bodies attached to the boughs of the hunched and shrivelled tree (will). If you pass, you shake away the vision and find something discarded on the ground; become DRIVEN and gain one common item."
          , pass Will 0 (Seq [driven, commonItem])
          )
        ,
          ( "St. Mary's Hospital"
          , "\"Oh, the doctor hardly needs to see you for a few scrapes. Here, I have some ointment for that cut,\" Nurse Sharon coos. You or an ally may recover two health. Something about the nurse seems different (observation). If you pass, you notice a new crucifix around her neck and she offers you her old one; become BLESSED."
          , Seq [health 2, pass Observation 0 blessed]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "You search through the stacks of books until you come across an old mystic journal. It will take some time to parse the faded words, or you could ask Miriam. Reveal the top three spells in the deck. You may buy one of them or become FATIGUED to gain one of them. Return the rest to the bottom of the deck."
          , Custom "magick-shoppe-spells"
          )
        ]
      )
    ]
