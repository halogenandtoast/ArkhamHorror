{- | The Key and the Gate.

Yog-Sothoth's thralls are at work on the gate, and the Lurker's pull is the
scenario: its reckoning drags everyone it is not holding a monster against one
space nearer the unstable space unless they pay in horror.

The Underworld is not on the board at setup. Its tile, its encounter deck, a
derelict portal and the threshold encounter deck are all set aside for card 153,
and so are the four Underworld event cards (18-21), which that card shuffles in
two at a time.
-}
module AH3e.Content.SecretsOfTheOrder.TheKeyAndTheGate (code, scenario, cards) where

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
code = "the-key-and-the-gate"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "The Key and the Gate"
    , expansion = SecretsOfTheOrder
    , startingSpace = spaceIdFor "Silver Twilight Lodge"
    , reckoningText =
        "Each investigator that is not engaged with one or more monsters moves one space toward the unstable space unless they suffer one horror."
    , reckoning = Custom "the-key-and-the-gate-reckoning"
    , setupMap =
        buildMapWith
          [ nb "Rivertown"
          , nb "Easttown"
          , nb "Merchant District"
          , nb "French Hill"
          , nb "Uptown"
          ]
          [ StreetDef (nb "Rivertown") SideRight (nb "Easttown") Bridge
          , StreetDef (nb "Rivertown") BottomLeft (nb "Merchant District") Scenic
          , StreetDef (nb "Rivertown") BottomRight (nb "French Hill") Residential
          , StreetDef (nb "Easttown") BottomLeft (nb "French Hill") Bridge
          , StreetDef (nb "Merchant District") BottomRight (nb "Uptown") Residential
          , StreetDef (nb "French Hill") BottomLeft (nb "Uptown") Scenic
          ]
          []
          [MysteryTile (nb "Merchant District") SideLeft "The Unnamable"]
    , monsters =
        [ ("bloody-titan", 1)
        , ("corpse-taker", 1)
        , ("coursing-hound", 1)
        , ("crazed-fiend", 1)
        , ("gluttonous-giant", 1)
        , ("keening-hound", 1)
        , -- the three Raging Poltergeists and the two Stalking Wraiths, which are
          -- shrouded and so named on the sheet by the face they show while ready
          ("commanding-specter", 1)
        , ("confounding-specter", 1)
        , ("crashing-specter", 1)
        , ("sanguinous-wraith", 1)
        , ("vomitous-wraith", 1)
        , ("tunneling-dhole", 1)
        , ("whippoorwill", 2)
        , -- every thrall monster
          ("lupine-thrall", 1)
        , ("avian-thrall", 1)
        , ("hulking-thrall", 1)
        , ("void-touched", 1)
        , ("altered-servant", 1)
        , ("icebound-captive", 1)
        ]
    , startingMonsters =
        [ ("tunneling-dhole", spaceIdFor "Graveyard")
        , ("whippoorwill", spaceIdFor "Hangman's Hill")
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
          [ "Graveyard"
          , "Hibb's Roadhouse"
          , "Bayfriar Gardens"
          , "River Docks"
          , "The Unnamable"
          , "Hangman's Hill"
          ]
    , startingMarkers = []
    , startingBystanders = []
    , eventCards = [eventCode n | n <- [1 .. 17] <> [22 .. 28]]
    , {- The four Underworld event cards wait for card 153, which shuffles two of them
      into the deck and discards the other two. The Lurker's two cards (158, 159) and the
      five Elders (161-165) wait as well: 157 hands out one of the first pair, and 151
      lays an Elder facedown on top of each neighborhood's deck. -}
      setAside =
        [eventCode n | n <- [18 .. 21]]
          <> [CardCode ("archive-" <> tshow n) | n <- [158, 159] <> [161 .. 165 :: Int]]
    , codex = [2, 150, 151]
    , anomalySet = Just "Fractured Reality"
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = fromBox SecretsOfTheOrder events

eventCode :: Int -> CardCode
eventCode n = CardCode ("the-key-and-the-gate-event-" <> pad n)

-- | Zero padded, so the card codes sort the way the cards are numbered.
pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

-- | "Remove one doom from any space", which only offers the spaces holding any.
anywhere :: Int -> Effect
anywhere n = RemoveDoomFrom AnySpace (N n)

-- | Miriam's shelves: three spells, and a clue for anything bought (card 26 asks none).
shoppeAny, shoppeOneHalf :: Effect
shoppeAny = Custom "soto-spell-market:any"
shoppeOneHalf = Custom "soto-spell-market:one-half"

event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (eventCode n)
    ("Event " <> tshow n <> "/28")
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
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "The waiter hands you a cold glass of water. You or an ally may recover two sanity (observation). If you pass, you notice a dead look in the server's eyes; gain one clue from your neighborhood. If you fail, you begin to realize most of the patrons are staring at you intently and, uncomfortable, you decide to leave."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Police Station"
        , "One of the patrol officers stands facing a blank wall (observation). If you pass, you see the drooling officer is holding something in their hand while mumbling incoherently; gain one clue from your neighborhood and one common item. If you fail, the officer suddenly turns to you and screams, \"Y'AI'NG'NGAH!\"; suffer one horror."
        , Test Observation 0 (Seq [clue, commonItem]) (horror 1)
        )
      ,
        ( "Velma's Diner"
        , "Velma smiles apologetically as you enter, \"Afraid the cook isn't feeling well. How about pie?\" You may spend $1 for you or an ally to recover two health. If you do, you glimpse into the kitchen through the service window and see the cook furiously cleaning the same spot on the wall over and over; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ]
  , event
      2
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "A patron with bloodshot eyes points a long finger at you and murmurs, \"The endless one knows who you were and where you will be.\" Gain one clue from your neighborhood. A server comes to calm the man and tells you to order or leave; you may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Police Station"
        , "Chief Nichols commends you for helping the city. Become DRIVEN. You thank him (influence). If you pass, your humble response wins the chief's trust and he shares some valuable information and hands you something useful; gain one clue from your neighborhood and one common item. If you fail, the chief scoffs at you and tells you to leave."
        , Seq [driven, pass Influence 0 (Seq [clue, commonItem])]
        )
      ,
        ( "Velma's Diner"
        , "You are greeted with complimentary pancakes. You or an ally may recover two health. You study a patron nervously stacking and unstacking coffee cups (observation). If you pass, gain one clue from your neighborhood. If you fail, they catch you staring and chase you down the street lobbing mugs at you; become FATIGUED."
        , Seq [health 2, Test Observation 0 clue fatigued]
        )
      ]
  , event
      3
      "Easttown"
      ["Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "You take a seat and a pale woman approaches you, \"I've got a song if you've got a buck,\" she rasps. You may spend $1 to gain one clue from your neighborhood and for you or an ally to recover two sanity. If you do, she slowly climbs onto the stage and fills the barn with a haunting, alien song."
        , mayPay (SpendMoney 1) (Seq [clue, sanity 2])
        )
      ,
        ( "Police Station"
        , "You follow a trail of strange sigils to an interrogation room and happen across an abandoned object. Gain one common item. You try to find the source (observation). If you pass, you discover an officer huddled in a dark corner scratching the marks into the wall; gain one clue from your neighborhood. If you fail, the symbols lead in every direction."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Velma's Diner"
        , "\"Can you hear it?\" a short, pudgy patron asks you nervously, \"Can you hear him?\" Velma calls out, \"Oh, don't mind him any; he's just tired. You, though, you look like you could use some warm food.\" You may spend $1 to gain one clue from your neighborhood and for you or an ally to recover three health."
        , mayPay (SpendMoney 1) (Seq [clue, health 3])
        )
      ]
  , event
      4
      "Easttown"
      ["Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "A crowd moves across the barn with fervor (observation). If you pass, you join in and find it soothing, but notice several of the dancers are saying strange things; gain one clue from your neighborhood and you or an ally may recover two sanity. If you fail, you stand and watch, dumbstruck."
        , pass Observation 0 (Seq [clue, sanity 2])
        )
      ,
        ( "Police Station"
        , "You see several glowing orbs disappear behind a door; gain one clue from your neighborhood. You investigate and run into a rookie officer (influence). If you pass, the officer asks if you were looking for this; gain one common item. If you fail, the officer, suspicious of your snooping around the station, locks you up for the night; become FATIGUED."
        , Seq [clue, Test Influence 0 commonItem fatigued]
        )
      ,
        ( "Velma's Diner"
        , "A boy scribbles on a napkin in the corner booth (observation). If you pass, you glance over and copy down the weird symbols and feel hopeful; gain one clue from your neighborhood and become DRIVEN. If you fail, you realize the boy has vanished and you hear a giggle in your ear; suffer one horror."
        , Test Observation 0 (Seq [clue, driven]) (horror 1)
        )
      ]
  , event
      5
      "Easttown"
      ["Hibb's Roadhouse", "Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "You or an ally may recover two sanity. As you sip your drink you notice the bartender nod at his scant tip jar. You may spend $1. If you do, he smiles and leans over to tell you some of the rumors he's heard recently; gain one clue from your neighborhood. If you don't, the man grunts and points to the door."
        , Seq [sanity 2, mayPay (SpendMoney 1) clue]
        )
      ,
        ( "Police Station"
        , "You take a look at the overflowing notice board in the lobby (observation). If you pass, you notice a pattern in some of the more recent disappearances; gain one clue from your neighborhood and become DRIVEN. If you fail, you glean no useful information from the countless missing and wanted posters."
        , pass Observation 0 (Seq [clue, driven])
        )
      ,
        ( "Velma's Diner"
        , "You walk in just as a server dumps a bottle of mustard on the floor and begins painting with it. The smeared markings are oddly menacing; gain one clue from your neighborhood. Velma rushes over to stop her, and tells you she'll take your order soon; you may spend $1 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ]
  , event
      6
      "French Hill"
      ["Silver Twilight Lodge"]
      [
        ( "Bayfriar Gardens"
        , "You take a look at the place a poor soul who committed suicide was found; gain one clue from your neighborhood (observation). If you pass, you find something in the dirt; gain one curio. If you fail, you find yourself convinced there is knowledge below, and you must dig for it; become FATIGUED."
        , Seq [clue, Test Observation 0 curioItem fatigued]
        )
      ,
        ( "Duterte Funeral Home"
        , "Several ravens caw at you incessantly from their perch. You may spend one remnant to feed them. If you do, the hungry creatures swarm your offering, and a haggard voice calls from behind you, \"A friend of the birds is a friend of mine, and you seem like you might need help;\" gain one clue from your neighborhood and one ally."
        , mayPay (SpendRemnants 1) (Seq [clue, ally])
        )
      ,
        ( "Silver Twilight Lodge"
        , "You gain entry into an intimate meeting of Lodge scholars; gain one spell. One of the members questions your resolve (influence). If you pass, the woman nods and shares with you several interesting arcane facts about the city; gain one clue from your neighborhood. If you fail, she frowns deeply and walks away."
        , Seq [spell, pass Influence 0 clue]
        )
      ]
  , event
      7
      "French Hill"
      ["Duterte Funeral Home", "Silver Twilight Lodge"]
      [
        ( "Bayfriar Gardens"
        , "Something wet and fleshy falls out of a gardener's pocket. Gain one remnant. The man shapes the hedges into disturbing silhouettes (will). If you pass, you study them and gain a little insight; gain one clue from your neighborhood. If you fail, you blink and are sure one of them moved; suffer one horror."
        , Seq [remnants 1, Test Will 0 clue (horror 1)]
        )
      ,
        ( "Duterte Funeral Home"
        , "A stranger stands in front of a casket, slowly opening and closing it repeatedly (observation). If you pass, you rouse them from the trance with a gentle shake of their shoulder; gain one clue from your neighborhood and one ally. If you fail, you stare too deeply into their eyes and glimpse something terrible; become CURSED."
        , Test Observation 0 (Seq [clue, ally]) cursed
        )
      ,
        ( "Silver Twilight Lodge"
        , "A Lodge member paces around, locking and unlocking things at random (observation). If you pass, you watch carefully and riffle through a promising drawer when they aren't looking; gain one clue from your neighborhood and one curio. If you fail, the maniacal gleam in their eye shakes you; suffer one horror."
        , Test Observation 0 (Seq [clue, curioItem]) (horror 1)
        )
      ]
  , event
      8
      "French Hill"
      ["Duterte Funeral Home"]
      [
        ( "Bayfriar Gardens"
        , "\"You dropped this,\" a gaunt man wheezes weakly. Gain one curio (will). If you pass, the man looks fairly harmless, but his eyes stare off into space oddly; gain one clue from your neighborhood. If you fail, the man's face twists in agony and his teeth fall out as he whimpers, \"S-sorry\"; become CURSED."
        , Seq [curioItem, Test Will 0 clue cursed]
        )
      ,
        ( "Duterte Funeral Home"
        , "A procession makes its way toward the graveyard (observation). If you pass, you realize that some of the pallbearers look confused and begin to lose their grip, but you call out just in time to avert disaster; gain one clue from your neighborhood and become DRIVEN. If you fail, the corner of the casket hits the pavement with a chilling thud."
        , pass Observation 0 (Seq [clue, driven])
        )
      ,
        ( "Silver Twilight Lodge"
        , "You overhear a member of the Lodge ask Carl Sanford about a missing key. Gain one clue from your neighborhood. As the small group leaves for another part of the Lodge, you see they left an open book on a side table (lore). If you pass, you find something useful amongst the bizarre incantations; gain one spell."
        , Seq [clue, pass Lore 0 spell]
        )
      ]
  , event
      9
      "French Hill"
      ["Bayfriar Gardens"]
      [
        ( "Bayfriar Gardens"
        , "A glowing silhouette of pulsating orbs disappears behind some bushes (observation). If you pass, you track it, but it vanishes suddenly, leaving something strange behind; gain one clue from your neighborhood and one remnant. If you fail, you lose the orb-creature in the vast gardens and are left wondering."
        , pass Observation 0 (Seq [clue, remnants 1])
        )
      ,
        ( "Duterte Funeral Home"
        , "You spot someone walking around the grounds in a daze, clicking their tongue and spinning in circles. You try to snap them out of their trance; gain one ally. They show you strange marks on their arms and ask if you recognize the pattern. You may spend one remnant to gain one clue from your neighborhood."
        , Seq [ally, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Silver Twilight Lodge"
        , "You sneak into a meeting for new initiates, and a senior member hands you something they call an \"initiation gift.\" Gain one curio before you settle in for the session (observation). If you pass, you notice several initiates have taken off their shoes during the lecture and placed them on their laps; gain one clue from your neighborhood."
        , Seq [curioItem, pass Observation 0 clue]
        )
      ]
  , event
      10
      "French Hill"
      ["Bayfriar Gardens"]
      [
        ( "Bayfriar Gardens"
        , "You find yourself back at the founder's statue, over and over (observation). If you pass, you find a hidden cache under a plaque that reads, \"Eternity stalks the Gate;\" gain one clue from your neighborhood and one curio. If you fail, you lose time in the endless loop and begin to go mad; suffer two horror."
        , Test Observation 0 (Seq [clue, curioItem]) (horror 2)
        )
      ,
        ( "Duterte Funeral Home"
        , "Madeline Duterte pulls you aside. \"A man was in, and he wanted to change his will to leave it all to a statue. I don't do wills, I told him. He left notes.\" Gain one clue from your neighborhood (observation). If you pass, you notice a warning in the writing; remove one doom from any space. If you fail, the scrawlings baffle you; become FATIGUED."
        , Seq [clue, Test Observation 0 (anywhere 1) fatigued]
        )
      ,
        ( "Silver Twilight Lodge"
        , "Five members silently pore over a set of ancient tomes (lore). If you pass, you quietly join them and study a particularly old text about inter-dimensional passageways; gain one clue from your neighborhood and one spell. If you fail, you are overtaken by an urge to read and cannot stop; become FATIGUED."
        , Test Lore 0 (Seq [clue, spell]) fatigued
        )
      ]
  , event
      11
      "Merchant District"
      ["Unvisited Isle", "Unvisited Isle"]
      [
        ( "River Docks"
        , "A burly woman steps in front of you and growls, \"Joey said you sell weird things. I'm looking fer something that'll scare a friend.\" You may spend one remnant to gain $3. If you do, the woman whistles at your offer, \"That's better'n that other stuff I found;\" gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "You realize the normal ticking is absent as you arrive at the club. Most of the clocks are upside down and several servers are taking turns flipping and unflipping them; gain one clue from your neighborhood. The bartender sighs and asks for your order. You may spend $1 for you or an ally to recover one health and one sanity."
        , Seq [clue, mayPay (SpendMoney 1) (RecoverBoth YouOrAlly (N 1) (N 1))]
        )
      ,
        ( "Unvisited Isle"
        , "A tree is etched with odd messages such as, \"The mists watch the lurker in oblivion\" (lore). If you pass, you parse some of the meaning, and realize an object has been left behind in the bark; gain one clue from your neighborhood and one curio. If you fail, the maddening words eat at you; suffer two horror."
        , Test Lore 0 (Seq [clue, curioItem]) (horror 2)
        )
      ]
  , event
      12
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "You hear a noise coming from the choppy water below (observation). If you pass, you spot a dazed man trying to climb down the uneven bricks and grab him before he falls; gain one clue from your neighborhood and become DRIVEN. If you fail, you hear someone nearby call, \"Papa, where are you?\""
        , pass Observation 0 (Seq [clue, driven])
        )
      ,
        ( "Tick-Tock Club"
        , "A velvety-voiced singer serenades the smoky club; you or an ally may recover two sanity (observation). If you pass, you realize the song is jumbled up, with the words out of order; gain one clue from your neighborhood. If you fail, the soft tones ease your mind, but too late you realize the song is firmly stuck in your head; become FATIGUED."
        , Seq [sanity 2, Test Observation 0 clue fatigued]
        )
      ,
        ( "Unvisited Isle"
        , "Even the frogs are acting strangely, hopping in obsessive circles and croaking in tandem; gain one clue from your neighborhood. You try to figure out their pattern (observation). If you pass, you watch as one of the frogs breaks the fervent rhythm and follow it to a glowing fragment of pronged, angular metal; gain one remnant."
        , Seq [clue, pass Observation 0 (remnants 1)]
        )
      ]
  , event
      13
      "Merchant District"
      ["River Docks"]
      [
        ( "River Docks"
        , "The bootlegger glowers, \"Here's payment. Where's the goods?\" Gain $2. Then, you may spend one remnant. If you do, she looks relieved, \"Just like the old guy was mumbling about;\" gain one clue from your neighborhood. If you don't, her hard face twists into a frown; suffer one damage."
        , Seq [money 2, MayPay (SpendRemnants 1) clue (damage 1)]
        )
      ,
        ( "Tick-Tock Club"
        , "\"Cover charge tonight, folks,\" a burly bouncer demands. You may spend $2. If you do, you enter the club and are greeted by an attractive patron who presses a bucket of ice into your hands with an imploring stare before they move wordlessly out the door; gain one clue from your neighborhood and you or an ally may recover two sanity."
        , mayPay (SpendMoney 2) (Seq [clue, sanity 2])
        )
      ,
        ( "Unvisited Isle"
        , "A small, sharp set of talons digs into your shoulder (will). If you pass, you glance back to find a carrier pigeon with a note and a grotesque, pungent-smelling eyeball attached to its leg; gain one clue from your neighborhood and one remnant. If you fail, you hesitate and are met with a piercing shriek; suffer one horror."
        , Test Will 0 (Seq [clue, remnants 1]) (horror 1)
        )
      ]
  , event
      14
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "Several corvids writhe on the ground. One of the birds vomits something at your feet; gain one curio. The sight is disturbing (will). If you pass, you manage to force yourself to observe them; gain one clue from your neighborhood. If you fail, their movements are just so unnatural; suffer one horror."
        , Seq [curioItem, Test Will 0 clue (horror 1)]
        )
      ,
        ( "General Store"
        , "Davy, the owner, grins as you enter, \"You're the hundredth customer today, here!\" Gain one common item (observation). If you pass, you realize Davy is saying this to every customer, his eyes glazed over; gain one clue from your neighborhood. If you fail, Davy has a long-winded, irrelevant story for every item you peruse; become FATIGUED."
        , Seq [commonItem, Test Observation 0 clue fatigued]
        )
      ,
        ( "Graveyard"
        , "You find some sticky, luminescent residue on a monument; gain one remnant (observation). If you pass, you see a group of boys going grave to grave, touching headstones and whispering \"Never mind the weather in the dark;\" gain one clue from your neighborhood. If you fail, the cold breeze urges you on your way."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ]
  , event
      15
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "You find rough etchings on a crumbling stone altar. Gain one clue from your neighborhood. Test lore to study the sketches or spend one remnant to make an offering. If you pass or spend the remnant, you call upon the shrine's spirit and appease it; gain one curio. If you fail, you find your time wasted."
        , Seq [clue, orPay Lore "Spend one remnant" (SpendRemnants 1) curioItem]
        )
      ,
        ( "General Store"
        , "The delivery boy, Nathan, tells you that his boss is acting really weird today and has taken six baths in the last handful of hours; gain one clue from your neighborhood. Nathan shrugs and says he can still help you with your shopping, though. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "You see the groundskeeper, Leonard Coburn, lifting up a toppled-over headstone and you try to help (strength). If you pass, Leonard thanks you gruffly and mentions this has been happening a lot lately, but only to the graves of people who have last names starting with the letter \"L;\" gain one clue from your neighborhood and $2."
        , pass Strength 0 (Seq [clue, money 2])
        )
      ]
  , event
      16
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The ceiling is covered in thick webbing (observation). If you pass, you find words woven into the spider silk; you gain one clue from your neighborhood and one spell. If you fail, you realize that you are in a nesting ground when dozens of adult spiders drop on you; suffer one horror."
        , Test Observation 0 (Seq [clue, spell]) (horror 1)
        )
      ,
        ( "General Store"
        , "You notice several other customers acting suspiciously and warn Nathan about the possible shoplifters (observation). If you pass, you soon realize that they are not planning on taking anything\8212rather they are leaving random items, like acorns and bottle caps, in odd places; gain one clue from your neighborhood and become DRIVEN."
        , pass Observation 0 (Seq [clue, driven])
        )
      ,
        ( "Graveyard"
        , "The groundskeeper thanks you for your help; gain $2. You ask him what he means, since you only just arrived (observation). If you pass, you notice several half-dug graves, and Leonard seems a bit dazed; gain one clue from your neighborhood. If you fail, Leonard pats your shoulder and tells you that you're a hard worker."
        , Seq [money 2, pass Observation 0 clue]
        )
      ]
  , event
      17
      "Rivertown"
      ["General Store"]
      [
        ( "Black Cave"
        , "Echoes bounce off the cavernous walls (observation). If you pass, you follow the noise to discover a tiny, weathered old woman singing, and she tosses you something; gain one clue from your neighborhood and one curio. If you fail, you search the cave for hours and find nothing; become FATIGUED."
        , Test Observation 0 (Seq [clue, curioItem]) fatigued
        )
      ,
        ( "General Store"
        , "\"Sale today,\" Nathan, the delivery boy smiles. You may buy one common item from the display for half price (rounded up). If you buy something, you hear another customer say very seriously, \"I need eight, not seven; the crickets know where I live. I have to protect my family;\" gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") HalfPrice (Just 1) clue
        )
      ,
        ( "Graveyard"
        , "You see a man nervously drawing on the side of a crypt; gain one clue from your neighborhood (will). If you pass, you hide quietly, and when he leaves you find a bloody lump on the ground; gain one remnant. If you fail, the man notices your nervous breathing and gives chase, frothing in his mania; become FATIGUED."
        , Seq [clue, Test Will 0 (remnants 1) fatigued]
        )
      ]
  , event
      18
      "The Underworld"
      ["Vale of Pnath"]
      [
        ( "City of the Gugs"
        , "The strange etching reads, \"It has no name, but is called all things.\" Gain one clue from your neighborhood. You hear a gug approaching (will). If you pass, you hold your breath until it's gone and find something left behind; gain one curio. If you fail, you gasp and are attacked; suffer two damage."
        , Seq [clue, Test Will 0 curioItem (damage 2)]
        )
      ,
        ( "Vale of Pnath"
        , "The rippling waves of bones alert you to an approaching dhole. You may suffer one horror to ride the wave closer to the edge of the valley. If you do, you find a faded pocketbook filled with pictures of dozens of mundane garden gates; gain one clue from your neighborhood and $2."
        , mayPay (CostHorror 1) (Seq [clue, money 2])
        )
      ,
        ( "Vaults of Zin"
        , "The ghasts that guard the entryway to the Vaults screech and claw at each other (will). If you pass, you tune out the carnage and observe another group also acting strangely, almost as if dancing; gain one clue from your neighborhood and one remnant. If you fail, the creatures stop for a moment, then all of their heads snap in your direction."
        , pass Will 0 (Seq [clue, remnants 1])
        )
      ]
  , event
      19
      "The Underworld"
      ["Vaults of Zin"]
      [
        ( "City of the Gugs"
        , "You find a massive room filled with numerous bizarre relics and manage to fit one in your bag; gain one remnant. A fleshy painting of beings doing strange things hangs high on a wall. You may suffer one damage to get the painting down and gain one clue from your neighborhood."
        , Seq [remnants 1, mayPay (CostDamage 1) clue]
        )
      ,
        ( "Vale of Pnath"
        , "A Nightgaunt circling overhead drops something into the mountains of bones (observation). If you pass, you locate the object easily; gain one clue from your neighborhood and one common item. If you fail, a grotesque creature seizes the parcel before you reach it and burrows into the moldering skeletons; suffer one horror."
        , Test Observation 0 (Seq [clue, commonItem]) (horror 1)
        )
      ,
        ( "Vaults of Zin"
        , "You find strange symbols carved into the leathery flesh of a ghoul; gain one spell (observation). If you pass, you hide when another ghoul arrives and begins to idly pluck out the dead one's fingernails; gain one clue from your neighborhood. If you fail, you don't hear the second ghoul until it falls upon you; suffer one damage."
        , Seq [spell, Test Observation 0 clue (damage 1)]
        )
      ]
  , event
      20
      "The Underworld"
      ["Vaults of Zin"]
      [
        ( "City of the Gugs"
        , "The hairs on the back of your neck stand on end (observation). If you pass, you spot a pale and noseless ghast climbing up the side of one of the towers just before it is crushed by the talons of a gug and plummets to the ground; gain one clue from your neighborhood and one remnant."
        , pass Observation 0 (Seq [clue, remnants 1])
        )
      ,
        ( "Vale of Pnath"
        , "You make it to the edge of the valley and hear an alien voice whispering in your ear, \"It was the gift of the father that gave the fog, and the fog birthed eternity.\" Gain one clue from your neighborhood. You think the sound is coming from a nearby hole in the rocks; you may suffer one horror to reach inside and gain one common item."
        , Seq [clue, mayPay (CostHorror 1) commonItem]
        )
      ,
        ( "Vaults of Zin"
        , "A pale-fleshed ghast chitters in an unsettling way, but it almost sounds like...singing; gain one clue from your neighborhood (will). If you pass, you manage to stay silent through the whole song and find something when the creature leaves; gain one remnant. If you fail, the noises unnerve you; suffer one horror."
        , Seq [clue, Test Will 0 (remnants 1) (horror 1)]
        )
      ]
  , event
      21
      "The Underworld"
      ["Vale of Pnath"]
      [
        ( "City of the Gugs"
        , "Gugs congregate outside one of the massive, circular towers (observation). If you pass, you notice the crude key-like shape that one of them scribbles into the dirt; gain one clue from your neighborhood and become DRIVEN. If you fail, the guttural, throaty noises bring on a migraine; become FATIGUED."
        , Test Observation 0 (Seq [clue, driven]) fatigued
        )
      ,
        ( "Vale of Pnath"
        , "You find a bloody backpack; gain $2. Nearby, a group of Nightgaunts is doing something odder than usual (observation). If you pass, you witness them trying to build some sort of archway; gain one clue from your neighborhood. If you fail, you become entranced by their movements and join in, building for hours; become FATIGUED."
        , Seq [money 2, Test Observation 0 clue fatigued]
        )
      ,
        ( "Vaults of Zin"
        , "You find a patch of disturbed earth (observation). If you pass, you discover a half-buried, flesh-bound text filled with crude drawings of clouds with dozens of eyes; gain one clue from your neighborhood and one spell. If you fail, you sink your hands into the earth to search and find nothing."
        , pass Observation 0 (Seq [clue, spell])
        )
      ]
  , event
      22
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "You find a strange key-shaped object made of mossy, flexible fibers; gain one remnant. A thick mist settles around you (observation). If you pass, you find a message carved into the twisted, gnarled roots of the tree that reads, \"It lurks but does not lie;\" gain one clue from your neighborhood. If you fail, you lose your way."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "St. Mary's Hospital"
        , "The nurse suggests a new over-the-counter drug; you may spend $1 for you or an ally to recover two health. If you do, as you wait, you notice a patient carefully search under each chair in the lobby and trace a pattern there with a piece of chalk; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher, the owner, tosses random books and charms to the ground. You take a closer look. Reveal the top three spells in the deck. You may buy any number of them. Place the rest on the bottom of the deck. If you buy anything, it jolts Miriam out of her trance; gain one clue from your neighborhood."
        , shoppeAny
        )
      ]
  , event
      23
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "The silhouette of a glimmering, veiled man passes behind a tree; gain one clue from your neighborhood (will). If you pass, you steel yourself and follow, only to find something he left behind; gain one common item. If you fail, something about the visage shakes you and you hastily leave."
        , Seq [clue, pass Will 0 commonItem]
        )
      ,
        ( "St. Mary's Hospital"
        , "\"Not too much longer,\" says the nurse while you wait for your doctor to arrive (observation). If you pass, you see the man through the window, splashing in the rain puddles; gain one clue from your neighborhood and you or an ally may recover two health when the nurse calls in another doctor. If you fail, the wait takes hours; become FATIGUED."
        , Test Observation 0 (Seq [clue, health 2]) fatigued
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "A customer huddles in a back corner, writing furiously (observation). If you pass, you manage to glimpse the message that they carve into their forearm: \"All for the price of One gains bliss in nothingness;\" you notice that Miriam seems remarkably blas\233 about the whole thing and gain one clue from your neighborhood and one spell."
        , pass Observation 0 (Seq [clue, spell])
        )
      ]
  , event
      24
      "Uptown"
      ["St. Mary's Hospital", "St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "Something odd protrudes from the ground (strength). If you pass, you dig and find dozens of bones melded together in the shape of a horrific archway; gain one clue from your neighborhood and one remnant. If you fail, you dig for hours and the intense labor drains you severely; become FATIGUED."
        , Test Strength 0 (Seq [clue, remnants 1]) fatigued
        )
      ,
        ( "St. Mary's Hospital"
        , "You sit in the examination room and notice all of the posters have extra eyes drawn on the faces, ringed in unknown words and symbols; gain one clue from your neighborhood. The doctor arrives and tells you the treatment plan; you may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "\"I have a few volumes available. Reasonable prices,\" Miriam gestures. Reveal the top three spells in the deck. You may buy one of them for half price (rounded up). Place the rest on the bottom of the deck. If you buy a spell, Miriam nods and chats with you about recent events; gain one clue from your neighborhood."
        , shoppeOneHalf
        )
      ]
  , event
      25
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "You hear several yowling cats (observation). If you pass, you discover the source is actually several human boys meowing and hissing at the ground; gain one clue from your neighborhood and one common item. If you fail, you realize that the sound is a trap when you are struck from behind; suffer one damage."
        , Test Observation 0 (Seq [clue, commonItem]) (damage 1)
        )
      ,
        ( "St. Mary's Hospital"
        , "You or an ally may recover two health. Nurse Sharon finishes treating your wounds and walks out of the room (observation). If you pass, you see her through the cracked door, smearing soiled bandages across the wall; gain one clue from your neighborhood. If you fail, you hear some awful noises from the hallway but wisely elect to ignore them."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher's cat is frantically running through the shop (observation). If you pass, you dodge a toppling shelf just before it strikes you and take note of the cat's strange behavior; gain one clue from your neighborhood. If you fail, you are struck by a falling Amazonian burial urn; discard two focus tokens."
        , Test Observation 0 clue (Seq [DiscardAFocus, DiscardAFocus])
        )
      ]
  , event
      26
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "Three women laugh as they peel away layer after layer of bark from the old tree. One of them tosses something to the ground in her fervor. Gain one common item (observation). If you pass, you manage to get close enough to see their disturbing work; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "St. Mary's Hospital"
        , "Doctor Mortimore asks if you would like to try a new drug (observation). If you pass, you decline after you glimpse his notes, which are covered in doodles that are either eyes or mouths; gain one clue from your neighborhood and become DRIVEN. If you fail, you accept the offer and the new drug fills you with anxiety; suffer one horror."
        , Test Observation 0 (Seq [clue, driven]) (horror 1)
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "\"Come in, come in! It's much too hot!\" Miriam Beecher bustles about as you note the chill in the air. Gain one clue from your neighborhood. Miriam scatters several artifacts in front of you and barks, \"Choose!\" Reveal the top four spells in the deck. You may buy any number of them. Place the rest on the bottom of the deck."
        , Seq [clue, spells 4 Nothing FullPrice]
        )
      ]
  , event
      27
      "Merchant District"
      ["The Unnamable"]
      [
        ( "The Unnamable"
        , "The hallway before you twists and warps before your eyes. As the floor buckles and writhes, you realize that something seeks to shepherd you into a side room rather than letting you move down the hall. Gain one clue from your space. You may move down the hall or search the side room.\nMove Down the Hall: You reach out to place a hand against the left wall, trusting your touch to guide you where your eyes cannot (observation). If you pass, you reach the nearly empty room at the end of the hall; gain one curio. If you fail, the hallway stretches on forever; become FATIGUED when you finally open your eyes and find you haven't moved at all.\nSearch the Side Room: Rather than braving the impossible hallway, you turn to the side room, where the walls are maddeningly blank (will). If you pass, you resist the urge to cover them in unknown words; remove one doom from any space. If you fail, you begin to cover the walls with scrawled predictions of your own death; suffer one horror."
        , Seq
            [ clue
            , Choose
                [ ("Move Down the Hall", Test Observation 0 curioItem fatigued)
                , ("Search the Side Room", Test Will 0 (anywhere 1) (horror 1))
                ]
            ]
        )
      ]
  , event
      28
      "Merchant District"
      ["The Unnamable", "The Unnamable"]
      [
        ( "The Unnamable"
        , "You happen upon someone trying to grab at a fat gray cat perched on top of a china cabinet. \"Help, will you? Stupid thing has a key tied around its neck!\" Gain one ally when you agree to help them. You may coax the cat down or grab at it.\nCoax the Cat Down: You look around, searching for something that might interest the apathetic creature (observation). If you pass, you spot several mice scurrying around the legs of a table in a figure-eight pattern and catch one; gain one clue from your space and become DRIVEN. If you fail, you show the cat a bit of string and it deigns to yawn at you.\nGrab At It: You pull a chair up to the china cabinet and climb up, ready to grab at the lazy beast. The gleam in its eyes reveals that something about the creature isn't quite right. You may become CURSED to gain one clue from your space. If you do, the thing smiles at you, stands upright, and drops an object at your feet; gain one curio."
        , Seq
            [ ally
            , Choose
                [ ("Coax the Cat Down", pass Observation 0 (Seq [clue, driven]))
                , ("Grab At It", mayPay (CostCondition "CURSED") (Seq [clue, curioItem]))
                ]
            ]
        )
      ]
  ]
