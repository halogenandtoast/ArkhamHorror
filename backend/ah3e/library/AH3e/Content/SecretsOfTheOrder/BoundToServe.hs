{- | Bound to Serve.

A plague of unquiet spirits washes over Arkham. The spirits are the scenario: the
reckoning feeds them doom where they stand and puts another on the board when the
last is gone, and the Lost Souls anomaly deck is what an overrun neighborhood
offers instead of its own encounters.
-}
module AH3e.Content.SecretsOfTheOrder.BoundToServe (code, scenario, cards) where

import AH3e.Content.Core.VeilOfTwilight (lodgeMonsters)
import AH3e.Content.Tiles
import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Map.Strict qualified as Map

code :: ScenarioCode
code = "bound-to-serve"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Bound to Serve"
    , expansion = SecretsOfTheOrder
    , startingSpace = spaceIdFor "The Witch House"
    , reckoningText =
        "For each spirit monster, place one doom in its space. If there are no spirit monsters on the board, spawn one spirit monster."
    , reckoning = Custom "bound-to-serve-reckoning"
    , setupMap =
        buildMapWith
          [ nb "Downtown"
          , nb "Merchant District"
          , nb "Rivertown"
          , nb "French Hill"
          , nb "Uptown"
          , nb "Southside"
          ]
          [ StreetDef (nb "Downtown") BottomLeft (nb "Merchant District") Bridge
          , StreetDef (nb "Downtown") BottomRight (nb "Rivertown") Bridge
          , StreetDef (nb "Merchant District") BottomRight (nb "French Hill") Residential
          , StreetDef (nb "Rivertown") BottomLeft (nb "French Hill") Residential
          , StreetDef (nb "French Hill") BottomLeft (nb "Uptown") Scenic
          , StreetDef (nb "French Hill") BottomRight (nb "Southside") Residential
          , StreetDef (nb "Uptown") SideRight (nb "Southside") Scenic
          ]
          []
          [MysteryTile (nb "French Hill") SideRight "The Witch House"]
    , monsters =
        [ ("flesh-eater", 2)
        , ("high-priest", 1)
        , ("lupine-thrall", 1)
        , ("taloned-cannibal", 2)
        , ("tindalos-alpha", 1)
        , -- every spirit monster: the three Haunting Dead, the three Raging
          -- Poltergeists and the two Stalking Wraiths, named by their engaged faces
          ("screaming-haunt", 1)
        , ("weeping-haunt", 1)
        , ("cacophonous-haunt", 1)
        , ("commanding-specter", 1)
        , ("confounding-specter", 1)
        , ("crashing-specter", 1)
        , ("sanguinous-wraith", 1)
        , ("vomitous-wraith", 1)
        ]
    , -- both are shrouded, so the sheet names the face they show while ready
      startingMonsters =
        [ ("raging-poltergeist", spaceIdFor "Independence Square")
        , ("haunting-dead", spaceIdFor "Hangman's Hill")
        ]
    , mythosCup =
        [ (SpreadDoomToken, 3)
        , (SpawnMonsterToken, 2)
        , (SpawnClueToken, 2)
        , (ReadHeadlineToken, 2)
        , (GateBurstToken, 1)
        , (ReckoningToken, 1)
        , (BlankToken, 3)
        ]
    , startingDoom =
        map
          spaceIdFor
          [ "Independence Square"
          , "Unvisited Isle"
          , "Black Cave"
          , "Duterte Funeral Home"
          , "Hangman's Hill"
          , "Historical Society"
          ]
    , startingMarkers = []
    , startingBystanders = []
    , eventCards = [CardCode ("bound-to-serve-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , setAside = lodgeMonsters
    , codex = [2, 121]
    , anomalySet = Just "Lost Souls"
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = fromBox SecretsOfTheOrder events

-- | Zero padded, so the card codes sort the way the cards are numbered.
pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

-- | "Remove one doom from any space", which only offers the spaces holding any.
anywhere :: Int -> Effect
anywhere n = RemoveDoomFrom AnySpace (N n)

event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("bound-to-serve-event-" <> pad n))
    ("Event " <> tshow n <> "/24")
    CoreSet
    1
    ( EventCard
        EventDef
          { scenario = code
          , neighborhood = nb hood
          , encounters =
              Map.fromList [(spaceIdFor place, Encounter txt eff) | (place, txt, eff) <- encounters]
          , doomSpaces = map spaceIdFor dooms
          }
    )

events :: [CardDef]
events =
  [ event
      1
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "One of the counselors helps you get your head straight. You or an ally may recover two sanity. Something is off about the orderly in the next ward (observation). If you pass, you realize the man hasn't touched anything and you're the only one who can see him; gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Independence Square"
        , "A young man is selling some of his late grandfather's things to pay for a move out west. You may buy any number of common items from the display. If you buy anything, he says, \"I saw grand-dad in my dreams last week. He said something was keeping him here in Arkham, even after he died;\" gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") FullPrice Nothing clue
        )
      ,
        ( "La Bella Luna"
        , "This early in the morning, both the restaurant and the Clover Club that it conceals should be quiet, but you can hear a pleading voice from around the back (observation). If you pass, you see a ghostly shade reliving its death, begging an unseen mobster for mercy; gain one clue from your neighborhood and become DRIVEN."
        , pass Observation 0 (Seq [clue, driven])
        )
      ]
  , event
      2
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "Dr. Badoe has an opening in his schedule and is happy to see you. You may pay $1 for you or an ally to recover two sanity. If you do, after your appointment, he tells you about overhearing Dr. Mintz in the basement, arguing with a long-dead patient; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Independence Square"
        , "A woman in a wispy, old-fashioned dress floats between pools of electric lamp-light in the park. Gain one clue from your neighborhood. When she beckons, you calm your beating heart and attempt to follow her (will). If you pass, she leads you to a long-forgotten parcel; gain one curio. If you fail, become FATIGUED."
        , Seq [clue, Test Will 0 curioItem fatigued]
        )
      ,
        ( "La Bella Luna"
        , "The woman at the craps table is on a hot streak. You see a person reflected in the mirror behind the bar, but don't see them standing near the table (observation). If you pass, you realize that she's being coached by a lingering spirit; gain one clue from your neighborhood and $3 when you follow her bet and hit it big."
        , pass Observation 0 (Seq [clue, money 3])
        )
      ]
  , event
      3
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "The lamps on the grounds take on a sickly green hue as writhing shadows ebb and flow like the breath of a great, dark beast. Gain one clue from your neighborhood. You hustle inside and hope Nurse Sharon can help you. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Independence Square"
        , "One can usually find something useful at the flea market. You may buy one common item from the display for half price (rounded up). You do your best to listen in on the vendors' whispers (observation). If you pass, you hear them discuss a cluster of inky black tendrils erupting from the ground near Founder's Rock; gain one clue from your neighborhood."
        , Seq [buyOneHalf "Common", pass Observation 0 clue]
        )
      ,
        ( "La Bella Luna"
        , "The streetlights flicker and go dark. When a cluster of black tendrils reaches out, you hurry into the club behind the restaurant. Gain one clue from your neighborhood. Inside, you take a seat at a low-stakes card table to calm your mind. You bluff on the worst hand you've ever seen (influence). If you pass, your opponent folds; gain $2."
        , Seq [clue, pass Influence 0 (money 2)]
        )
      ]
  , event
      4
      "French Hill"
      ["Duterte Funeral Home"]
      [
        ( "Bayfriar Gardens"
        , "The twists and turns of the hedge maze are dizzying, but you feel certain that something waits for you within (observation). If you pass, your path leads you to the leonine statue of a guardian beast, where you see the traces of a worn-away inscription; gain one clue from your neighborhood and become DRIVEN."
        , pass Observation 0 (Seq [clue, driven])
        )
      ,
        ( "Duterte Funeral Home"
        , "Madeline Duterte's assistant, Samuel, calls out for your help. You find him on the grounds trying to escape a tendril of inky blackness that reaches out from under the earth. You may discard one remnant to distract the grasping darkness and rescue Samuel. If you do, gain one clue from your neighborhood and remove one doom from any space."
        , mayPay (SpendRemnants 1) (Seq [clue, anywhere 1])
        )
      ,
        ( "Silver Twilight Lodge"
        , "A Lodge librarian tells you she has found references to an accord the Lodge's founders reached with an unknown partner. Gain one clue from your neighborhood. You ask if she's got any other records you could see (influence). If you pass, she shows you an occultist's journals; gain one spell. If you fail, her glare is withering; become CURSED."
        , Seq [clue, Test Influence 0 spell cursed]
        )
      ]
  , event
      5
      "French Hill"
      ["Duterte Funeral Home"]
      [
        ( "Bayfriar Gardens"
        , "A spectral couple strolls into the hedge maze, arm in arm and dressed in last century's Sunday best. You follow them until they suddenly cry out and vanish (observation). If you pass, you find a circle of dark soil surrounding a discarded object; gain one clue from your neighborhood and one curio."
        , pass Observation 0 (Seq [clue, curioItem])
        )
      ,
        ( "Duterte Funeral Home"
        , "Madeline Duterte tells you about the runaways that her mother helped on the Underground Railroad. \"Good people have always battled the mistakes of the past,\" she says. \"You remind me of her.\" Become DRIVEN. You may spend one remnant to show her and one of her visitors that you're still fighting; gain one clue from your neighborhood and one ally."
        , Seq [driven, mayPay (SpendRemnants 1) (Seq [clue, ally])]
        )
      ,
        ( "Silver Twilight Lodge"
        , "A handful of Lodge officers are arguing about something in the library (observation). If you pass, you listen in from behind a heavy wooden bookcase while the younger member demands answers about something she calls \"the covenant;\" gain one clue from your neighborhood and one spell. If you fail, they stare at you until you leave."
        , pass Observation 0 (Seq [clue, spell])
        )
      ]
  , event
      6
      "French Hill"
      ["Bayfriar Gardens"]
      [
        ( "Bayfriar Gardens"
        , "You see the bloom of a dark rose spin across the path, carried on an impossible wind. It settles near one of the large stone pillars. Gain one remnant before you study the stone (observation). If you pass, you see the worn lines of an ancient inscription in the stone; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "Duterte Funeral Home"
        , "Madeline Duterte tells you what she has learned about the spirits from her allies and contacts throughout the city. Gain one clue from your neighborhood. With your help, she can have her friends work to aid you in return. You may spend one remnant to remove one doom from any space."
        , Seq [clue, mayPay (SpendRemnants 1) (anywhere 1)]
        )
      ,
        ( "Silver Twilight Lodge"
        , "A woman with a Lodge pin slips out of a side door and smoothly shuts it behind her. Gain one clue from your neighborhood as you blink and the spirit vanishes. The door is locked (observation). If you pass, you easily spring the decades-old lock and search the room; gain one curio. If you fail, you struggle with the lock; become delayed."
        , Seq [clue, Test Observation 0 curioItem delayed]
        )
      ]
  , event
      7
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "The electric lamps do nothing to dispel the inky shadows that lurk beneath the bridge. As you hustle past, they seem to reach out to you. Gain one clue from your neighborhood. You meet up with a nervous-looking Joey Vigil and get down to business. You may spend one remnant to gain $2."
        , Seq [clue, mayPay (SpendRemnants 1) (money 2)]
        )
      ,
        ( "Tick-Tock Club"
        , "The warm lighting and hot food in the club offer a welcome respite from the gloom that pervades Arkham these days. You may pay $2 for you or an ally to recover three health. If you do, the server tells you that the band won't play tonight because they saw something strange reflected in the green room's mirrors; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [health 3, clue])
        )
      ,
        ( "Unvisited Isle"
        , "A perfect sphere of midnight blackness hangs in the air between a pair of weathered standing stones. With a shaky hand, you reach out to find what waits at the center of the dark space (will). If you pass, you stumble through the dark to find a carved stone seal; gain one clue from your neighborhood and one remnant."
        , pass Will 0 (Seq [clue, remnants 1])
        )
      ]
  , event
      8
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "The wispy spirit of a young man calls on the citizens of Arkham to stand against tyranny. Become DRIVEN. While the revolution is long finished, you study the spirit carefully (observation). If you pass, you find the crest of the Order of the Silver Twilight pinned to his lapel; gain one clue from your neighborhood."
        , Seq [driven, pass Observation 0 clue]
        )
      ,
        ( "Tick-Tock Club"
        , "The band plays a lively number and the floor fills with energetic dancers. You or an ally may recover two sanity. One couple draws all eyes in the room as they twirl and glide through the crowd (observation). If you pass, you see that both dancers bear fatal wounds and float an inch above the parquet floor; gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Unvisited Isle"
        , "A spirit in a long, tattered robe floats in the center of a gloomy clearing. When it sees you, it begins to chant a rasping, breathless hex at you (will). If you pass, you withstand its magics and claim something left by the shade's last victim; gain one clue from your neighborhood and one curio. If you fail, the magic binds you; become delayed."
        , Test Will 0 (Seq [clue, curioItem]) delayed
        )
      ]
  , event
      9
      "Merchant District"
      ["River Docks"]
      [
        ( "River Docks"
        , "A stranger in a wool cap greets you with an offer to trade hard cash for anything unusual you might have found. You may spend one remnant to gain $2. If you do, while you finish your transaction he describes some of the strange things he's seen himself; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 2, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "The main room of the club is abuzz with talk of the spirits people have sighted throughout Arkham. You may pay $1 to join the crowd and learn from the experiences the others have to share; gain one clue from your neighborhood and focus one skill of your choice."
        , mayPay (SpendMoney 1) (Seq [clue, focusAny])
        )
      ,
        ( "Unvisited Isle"
        , "The wind whistling through the ancient and weathered stones carries the musty scent of a decrepit tomb. Gain one clue from your neighborhood. The sound of the wind almost sounds like a language (lore). If you pass, you follow the directions to the remains of a ritual; gain one remnant."
        , Seq [clue, pass Lore 0 (remnants 1)]
        )
      ]
  , event
      10
      "Merchant District"
      ["River Docks"]
      [
        ( "River Docks"
        , "A muffled curse draws your attention to a shadowy alley, where you find Johnny \"the Don\" Valone face to face with a vengeful spirit (observation). If you pass, you help him escape and he bribes you to never speak of what you saw; gain one clue from your neighborhood and $3."
        , pass Observation 0 (Seq [clue, money 3])
        )
      ,
        ( "Tick-Tock Club"
        , "The doorman is pale as a sheet. When you press him, he describes the shrieking figure that just burst into flames and abruptly vanished. Gain one clue from your neighborhood. He lets you inside, where you can relax for a moment with a drink. You may pay $1 for you or an ally to recover one health and one sanity."
        , Seq [clue, mayPay (SpendMoney 1) (RecoverBoth YouOrAlly (N 1) (N 1))]
        )
      ,
        ( "Unvisited Isle"
        , "The robed figure slumped in the middle of the stone circle clutches something in her shriveled hand. Gain one curio. The air grows still, like the moments before a thunderclap (observation). If you pass, you swiftly search the dead arcanist; gain one clue from your neighborhood. If you fail, you flee the coming darkness; become FATIGUED."
        , Seq [curioItem, Test Observation 0 clue fatigued]
        )
      ]
  , event
      11
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The dark, cold air hums with magic, filling your mind with new possibilities. Gain one spell. As the ancient knowledge pours into you, you realize the darkness is writhing closer like a living thing (will). If you pass, you escape and gain one clue from your neighborhood. If you fail, become CURSED."
        , Seq [spell, Test Will 0 clue cursed]
        )
      ,
        ( "General Store"
        , "You browse the shelves. You may buy any number of common items from the display. While you shop, Mr. Hatle has an uncharacteristically quiet conversation with his checkers opponent (observation). If you pass, you listen in on his whispered story about a trio of ghosts that pestered the old man all night; gain one clue from your neighborhood."
        , Seq [buyAny "Common", pass Observation 0 clue]
        )
      ,
        ( "Graveyard"
        , "You find a dead humanoid creature among the tombstones. Gain one remnant. You study the dead ghoul to determine what happened to it (observation). If you pass, the withered beast looks like it has been drained of its energy by one of the enervating black tendrils that lurk in the shadows; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ]
  , event
      12
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The air pressure in the cave drops suddenly, making your ears pop. A rush of adrenaline fills your mouth with a metallic tang. Become DRIVEN. You are not alone here (observation). If you pass, you see the coiling black tendrils in the shadows; gain one clue from your neighborhood and one spell."
        , Seq [driven, pass Observation 0 (Seq [clue, spell])]
        )
      ,
        ( "General Store"
        , "Davy Schoffner is happy to see any customers at all, given the nightmares swarming through the city. You may buy one common item from the display for half price (rounded up). If you buy anything, Davy tells you about the trio of faceless children that were waiting for him this morning; gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") HalfPrice (Just 1) clue
        )
      ,
        ( "Graveyard"
        , "The wispy shade of a woman in a green dress floats morosely among the gravestones. Her head floats a few inches above the stump of her neck. You may become delayed to watch her until she comes to rest. If you do, she pauses before an ancient stone engraving; gain one clue from your neighborhood and one remnant."
        , mayPay CostDelayed (Seq [clue, remnants 1])
        )
      ]
  , event
      13
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "The shade's twisted, ragged lips move swiftly as it spits ancient curses through its crooked maw (lore). If you pass, you counter its magics and search the cairn it guards; gain one clue from your neighborhood and one curio. If you fail, you flee before the withering malediction takes hold."
        , pass Lore 0 (Seq [clue, curioItem])
        )
      ,
        ( "General Store"
        , "Davy Schoffner presses an item into your hands. \"This crazy thing won't stay on the shelf. Get it out of here.\" Gain one common item. You study the object carefully (observation). If you pass, you don't see anything wrong with it, but you do notice a laughing, child-like spirit behind Davy's back; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Graveyard"
        , "The door of a nearby mausoleum has been torn open. Something glints within, as inky black tendrils snake out of the ground to seize both you and it (will). If you pass, you brave the shadows and recover a cracked stone seal and a jeweled crest from the Order of the Silver Twilight; gain one clue from your neighborhood and $3."
        , pass Will 0 (Seq [clue, money 3])
        )
      ]
  , event
      14
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "The hair on the back of your neck rises and your breath clouds the air. The temperature in the cave drops sharply (observation). If you pass, you follow a weeping shadow to a well-concealed hiding spot that has lain undisturbed for years; gain one clue from your neighborhood and one curio."
        , pass Observation 0 (Seq [clue, curioItem])
        )
      ,
        ( "General Store"
        , "The store is dark and the door is locked (observation). If you pass, you hear a clatter from the trash bins in the alley behind the shop, where you find Nathan, the delivery boy, cowering from something you can't see; gain one clue from your neighborhood and one common item when he offers you a gift for helping him. If you fail, you wait for a while to no avail."
        , pass Observation 0 (Seq [clue, commonItem])
        )
      ,
        ( "Graveyard"
        , "Arms of inky shadow coil around several of the older monuments. Gain one clue from your neighborhood. One of the grave markers shifts, falls, and cracks when one of the tendrils suddenly withdraws (strength). If you pass, gain one remnant from the broken stone. If you fail, you become FATIGUED trying to right the stone."
        , Seq [clue, Test Strength 0 (remnants 1) fatigued]
        )
      ]
  , event
      15
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "Tonight's guest speaker never left the train station. Terrified by a lurking shadow on the platform, he fled back to Boston. Gain one clue from your neighborhood. You take some time to reassure the disgruntled attendees (influence). If you pass, you build a rapport with a capable stranger; gain one ally."
        , Seq [clue, pass Influence 0 ally]
        )
      ,
        ( "Ma's Boarding House"
        , "There is always room at Ma's table for a hearty supper. You or an ally may recover two health. You may pay $1 and take a room for the night for you or an ally to recover two additional health. If you do, you are jolted awake when a spirit leaps out of your third floor window with a mournful scream; gain one clue from your neighborhood."
        , Seq [health 2, mayPay (SpendMoney 1) (Seq [health 2, clue])]
        )
      ,
        ( "South Church"
        , "Father Michael sweeps up scattered votives and tattered hymnals in the apse of the church. You may pay $1 to help replace the damaged material and for you or an ally to recover two sanity. If you do, he describes the way the candles and books launched themselves through the air; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ]
  , event
      16
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "The curator tells you that several of the collection's prized displays were damaged when the cases spontaneously shattered. You may spend one remnant to help him rebuild. If you do, he shows you to the room where it happened; gain one clue from your neighborhood and become DRIVEN."
        , mayPay (SpendRemnants 1) (Seq [clue, driven])
        )
      ,
        ( "Ma's Boarding House"
        , "Ma Matheson tells you about the heavy footfalls that wake the whole household every night at 1:13 in the morning. There is never anyone in the front hall, but everyone hears the noisy footsteps. Gain one clue from your neighborhood. You may pay $1 and join her bleary-eyed guests for breakfast for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "South Church"
        , "A blue-tinged light bobs through the empty churchyard, like a lantern carried by an unseen watchman. It floats away from you like it is leading you somewhere (observation). If you pass, you follow the ghost-lamp to a patch of lacy, pale-white flowers; gain one clue from your neighborhood and you or an ally may recover two sanity."
        , pass Observation 0 (Seq [clue, sanity 2])
        )
      ]
  , event
      17
      "Southside"
      ["South Church"]
      [
        ( "Historical Society"
        , "Mr. Peabody anxiously shows you to the gallery where he saw the ghostly woman (influence). If you pass, he introduces you to another witness who can help; gain one clue from your neighborhood and one ally. If you fail, he becomes flustered and his stammering is interrupted by a keening shriek; become CURSED."
        , Test Influence 0 (Seq [clue, ally]) cursed
        )
      ,
        ( "Ma's Boarding House"
        , "The pathway to Ma's front door is shrouded in writhing shadows (observation). If you pass, you find a clear trail through them and join the lodgers for a hearty meal; gain one clue from your neighborhood and you or an ally may recover two health. If you fail, the coiled shadows reach out for you, and you flee before they can seize you."
        , pass Observation 0 (Seq [clue, health 2])
        )
      ,
        ( "South Church"
        , "The head of the ladies' quilting circle offers you a soothing cup of tea. You or an ally may recover two sanity. You may become delayed to tell the old woman what you've witnessed. If you become delayed, she pulls the names and stories of the spirits you have seen from her impressive memory; gain one clue from your neighborhood."
        , Seq [sanity 2, mayPay CostDelayed clue]
        )
      ]
  , event
      18
      "Southside"
      ["South Church"]
      [
        ( "Historical Society"
        , "The north gallery is as still as the grave (observation). If you pass, you watch a black tendril snake through the room, knocking an old and forgotten wooden crate from a high shelf; gain one clue from your neighborhood and one curio. If you fail, you are seized by a coil of inky blackness; become FATIGUED."
        , Test Observation 0 (Seq [clue, curioItem]) fatigued
        )
      ,
        ( "Ma's Boarding House"
        , "Ma's eyes grow wide when you hear someone moving in the attic. \"There oughtn't be anyone up there!\" You are filled with adrenaline and become DRIVEN as you stealthily climb the back steps (observation). If you pass, the young woman skulking there turns her mouthless face toward you before she fades into vapor; gain one clue from your neighborhood."
        , Seq [driven, pass Observation 0 clue]
        )
      ,
        ( "South Church"
        , "Father Michael explains that while he was hearing confession, the room grew dark and the woman he was helping vanished. He wonders aloud whether she was ever truly there. Gain one clue from your neighborhood. You may pay $1 to tithe to the church. If you do, the priest thanks you warmly; you or an ally recovers three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ]
  , event
      19
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "A woman recoils from an unseen attack. You watch the looping vision of her death repeat, over and over (observation). If you pass, the vision fades away when you find her remains; gain one clue from your neighborhood and one common item. If you fail, the image stays with you; become CURSED."
        , Test Observation 0 (Seq [clue, commonItem]) cursed
        )
      ,
        ( "St. Mary's Hospital"
        , "Dr. Maheswaran looks tired, but she welcomes you into her office for treatment. You may pay $1 for you or an ally to recover two health. If you do, you get her talking, and she tells you she has been seeing visions of all the patients who have died under her care in her career in Arkham; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam is nowhere to be seen, but an odd song fills the air of the shop. Gain one spell. You search for the source (observation). If you pass, you locate a woman's spirit, little more than a wisp of light, among the cluttered shelves; gain one clue from your neighborhood. If you fail, the incessant music drives you out in a daze; become FATIGUED."
        , Seq [spell, Test Observation 0 clue fatigued]
        )
      ]
  , event
      20
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "A wraith-like figure stares sadly at the old, rotted gallows. His cracked lips smile in a rueful greeting, revealing jagged teeth (will). If you pass, he grimly tells you that after his execution, he was unable to leave this place; gain one clue from your neighborhood and one remnant."
        , pass Will 0 (Seq [clue, remnants 1])
        )
      ,
        ( "St. Mary's Hospital"
        , "Dr. Maheswaran is happy to see you, but urges you to take better care of yourself. You or an ally may recover two health. As you leave, you notice a flickering light from within an empty operating theater (observation). If you pass, you find a melancholy apparition floating sulkily at the site of its death; gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The air is still and quiet. You look over the shop shelves with a sense of anticipation. Reveal the top three spells from the deck. You may buy any number of them. Place the rest on the bottom of the deck. If you buy anything, you feel like you are being watched and head for the door; gain one clue from your neighborhood."
        , Custom "bound-to-serve-spell-market"
        )
      ]
  , event
      21
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "The body on the path is clutching something. Gain one common item. As you inspect the corpse, inky black tendrils snake out of the cold ground all around you (observation). If you pass, you elude their chilling grip; gain one clue from your neighborhood. If you fail, become FATIGUED."
        , Seq [commonItem, Test Observation 0 clue fatigued]
        )
      ,
        ( "St. Mary's Hospital"
        , "As you approach the main entrance, you see writhing, inky tendrils rising from the earth around the building's foundation. Gain one clue from your neighborhood. Safely inside, you are free to seek out treatment from the hospital's diligent staff. You may pay $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "A chaotic swirl of parchment and small oddities moves through the back of the store, knocking old books and other merchandise aside. \"A spirit,\" Miriam Beecher says reverently. \"She is trying to show you something.\" You may become FATIGUED to listen to the lost soul and gain one clue from your neighborhood and one spell."
        , mayPay (CostCondition "FATIGUED") (Seq [clue, spell])
        )
      ]
  , event
      22
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "Dark tendrils of inky blackness writhe amidst the witchweed. Gain one clue from your neighborhood. A body lies slumped within the vines, but you must be careful, lest your stirring attract the reaching black arms (will). If you pass, you find a Lodge signet ring on his finger; gain one remnant."
        , Seq [clue, pass Will 0 (remnants 1)]
        )
      ,
        ( "St. Mary's Hospital"
        , "The lights in the records room abruptly go dark, and you struggle to find your way out (observation). If you pass, they blink on again just as you put your hand on the doorknob and you hear a mischievous giggle; gain one clue from your neighborhood before you return to Nurse Sharon, where you or an ally may recover two health. If you fail, become FATIGUED."
        , Test Observation 0 (Seq [clue, health 2]) fatigued
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "You step over a thick line of salt across the store's threshold, and Miriam Beecher explains that it keeps away unwanted visitors. Gain one clue from your neighborhood. She's ready to talk business. Reveal the top three spells from the deck. You may buy one of them for half price (rounded up). Place the rest on the bottom of the deck."
        , Seq [clue, spells 3 (Just 1) HalfPrice]
        )
      ]
  , event
      23
      "French Hill"
      ["The Witch House"]
      [
        ( "The Witch House"
        , "The attic hallway is littered with scraps of paper covered in occult ravings. Gain one remnant. From the abandoned apartment under one of the steep gables, you hear the piteous sobbing of a lost child echoing under the warped door. You may call out to the child or open the door to investigate.\nCall Out: At your call, the hallway goes deathly quiet. As you exhale a nervous breath, the door swings open to reveal an empty room (will). If you pass, you approach cautiously to find an abandoned journal; gain one clue from your location and one spell. If you fail, the creaking of the old house overwhelms you and you flee; become FATIGUED.\nOpen the Door: The doorknob is icy cold, but it yields under your grip and the door swings open with a creak (observation). If you pass, you see the shadows pooled in the corners of the room shrink from your lamp; gain one clue from your location and become DRIVEN. If you fail, the shadows ooze toward you like a viscous film of liquid; become CURSED."
        , Seq
            [ remnants 1
            , Choose
                [ ("Call Out", Test Will 0 (Seq [clue, spell]) fatigued)
                , ("Open the Door", Test Observation 0 (Seq [clue, driven]) cursed)
                ]
            ]
        )
      ]
  , event
      24
      "French Hill"
      ["The Witch House"]
      [
        ( "The Witch House"
        , "The deep shadow in the cellar whispers to you, promising power, if only you'll let it in. Gain one clue from your location. As the whispered temptation becomes too much to bear, you hear a second voice calling out to you. You may block out the voices or focus on the cry for help.\nBlock Out the Voices: The room's shadows have crept in close upon you, and the voice refuses to let you go (observation). If you pass, you hone your attention upon the light streaming from the top of the stairs and clear your mind; become DRIVEN. If you fail, the susurration wears you down until you accept the offered bargain; gain a DARK PACT and one spell.\nFocus on the Cry: You realize that the whispering darkness has claimed someone, but you can reach out to them and lead them out of the shadows (lore). If you pass, you feel your hand seize upon another, and the shared touchstone brings you both out of the darkness; MICHAEL LEIGH joins you and explains that he first faced this darkness in Salem."
        , Seq
            [ clue
            , Choose
                [
                  ( "Block Out the Voices"
                  , Test Observation 0 driven (Seq [GainE (Condition "DARK PACT"), spell])
                  )
                , ("Focus on the Cry", pass Lore 0 (named "MICHAEL LEIGH"))
                ]
            ]
        )
      ]
  ]
