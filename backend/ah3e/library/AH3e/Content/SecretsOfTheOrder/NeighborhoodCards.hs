-- | The neighborhood encounter deck for the tile Secrets of the Order adds.
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
cards = fromBox SecretsOfTheOrder frenchHill

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
