-- | The Pale Lantern: the Lantern Club prospers while Kingsport's missing return hollow.
module AH3e.Content.UnderDarkWaves.ThePaleLantern (code, scenario, cards) where

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
code = "the-pale-lantern"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

-- | Declan Pearce waits in the box until card 78 turns him up.
heldBack :: [CardCode]
heldBack = ["declan-pearce", "archive-89"]

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "The Pale Lantern"
    , expansion = UnderDarkWaves
    , startingSpace = spaceIdFor "River Docks"
    , reckoningText =
        "Each investigator with doom in their space suffers one horror unless they discard one focus."
    , reckoning =
        ForInvestigators
          EveryInvestigator
          ( If
              (CountAtLeast DoomInYourSpace 1)
              ( Choose
                  [ ("Suffer one horror", SufferHorror (N 1))
                  , ("Discard one focus", Pay (SpendFocus 1) NoEffect)
                  ]
              )
              NoEffect
          )
    , setupMap =
        buildMapOf
          [ nb "Downtown"
          , nb "Merchant District"
          , nb "Uptown"
          , nb "Central Kingsport"
          , nb "Kingsport Harbor"
          ]
          -- Arkham is one cluster, Kingsport the other
          [ StreetDef (nb "Downtown") BottomLeft (nb "Merchant District") Bridge
          , StreetDef (nb "Merchant District") BottomRight (nb "Uptown") Residential
          , StreetDef (nb "Central Kingsport") SideRight (nb "Kingsport Harbor") Scenic
          ]
          noPieces
            { routes =
                [ RouteDef (nb "Downtown") SideLeft CountryRoad
                , RouteDef (nb "Merchant District") SideRight TrainPlatform
                , RouteDef (nb "Uptown") SideLeft CountryRoad
                , RouteDef (nb "Central Kingsport") TopRight TrainPlatform
                , RouteDef (nb "Kingsport Harbor") BottomLeft CountryRoad
                ]
            , mysteries = [MysteryTile (nb "Kingsport Harbor") TopRight "Strange High House"]
            , {- Kingsport runs off to the left of Uptown, joined by nothing. The link names
              the Harbor because the street runs from Central Kingsport to it: laying the
              Harbor against Uptown puts Central Kingsport the next step out. -}
              clusters = [ClusterLink (nb "Uptown") SideLeft (nb "Kingsport Harbor")]
            , {- and the town then stands a little further out again, so it reads apart from
              Arkham. Both its tiles move together, or the street between them would skew. -}
              nudges =
                [ nudge (nb "Central Kingsport") (-0.3) 0
                , nudge (nb "Kingsport Harbor") (-0.3) 0
                ]
            }
    , monsters =
        [ ("guardian-beast", 1)
        , ("prowling-abductor", 2)
        , ("robed-figure", 3)
        , ("terrified-wanderer", 2)
        , ("void-touched", 2)
        , -- every byakhee monster
          ("swift-byakhee", 1)
        , ("hovering-byakhee", 1)
        , ("swooping-scavenger", 1)
        , -- and every moon-beast
          ("cruel-slaver", 1)
        , ("feasting-master", 1)
        , ("pale-lord", 1)
        ]
    , startingMonsters =
        [ ("swooping-scavenger", spaceIdFor "Independence Square")
        , ("prowling-abductor", spaceIdFor "North Point Lighthouse")
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
          , "Hangman's Hill"
          , "Hall School"
          , "St. Erasmus's Home"
          ]
    , startingMarkers = []
    , startingBystanders = []
    , eventCards = [CardCode ("lantern-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , setAside = heldBack
    , codex = [2, 76, 87]
    , anomalySet = Just "Visions of the Moon"
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = fromBox UnderDarkWaves events

-- | Zero padded, so the card codes sort the way the cards are numbered.
pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("lantern-event-" <> pad n))
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
      "Central Kingsport"
      ["Congregational Hospital"]
      [
        ( "Congregational Hospital"
        , "An influx of amnesiac patients has baffled the doctors here, and they have no time to treat your injuries. Gain one clue from your neighborhood. You may spend $1 to interview one of the afflicted. If you do, she gives you a small pale stone that she doesn't remember picking up; gain a remnant."
        , Seq [clue, mayPay (SpendMoney 1) (remnants 1)]
        )
      ,
        ( "Hall School"
        , "Several influential families send their children here. You speak to a woman dropping off her daughter and ask about a secret society in Kingsport (influence). If you pass, her response is enigmatic, but it confirms your suspicions; gain one clue from your neighborhood. If you fail, she tells you there is no such thing and notes your name; become TAINTED."
        , Test Influence 0 clue tainted
        )
      ,
        ( "Neil's Curiosity Shop"
        , "You tell Neil he should leave town for a while. You may spend one remnant to prove to him that there is a vast conspiracy. If you do, Neil tells you what little he knows about the Lantern Club and warns that you should take something to protect yourself; gain one clue from your neighborhood and one curio."
        , mayPay (SpendRemnants 1) (Seq [clue, curioItem])
        )
      ]
  , event
      2
      "Central Kingsport"
      ["Hall School", "Hall School"]
      [
        ( "Congregational Hospital"
        , "Your physician is replaced by a second doctor who tends to your injuries. You or an ally may recover two health. Your new doctor and her nurse confer briefly about the first man (observation). If you pass, you sense they think something has fouled the man's judgment; gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "Hall School"
        , "A light is on in a classroom in the middle of the night. You approach the window and listen to the voices coming from that room (observation). If you pass, you hear an in-depth discussion of arcane arts; gain a spell and one clue from your neighborhood. If you fail, you are discovered and beaten by men wearing bone-colored masks; suffer two damage."
        , Test Observation 0 (Seq [spell, clue]) (damage 2)
        )
      ,
        ( "Neil's Curiosity Shop"
        , "The inside of the shop is covered in moths and the shadows of their wings flutter on the walls. You collect one of the large insects. Gain a remnant. You search for what has attracted them (observation). If you pass, you find a small lamp and destroy it; gain one clue from your neighborhood. If you fail, the moths follow you; become TAINTED."
        , Seq [remnants 1, Test Observation 0 clue tainted]
        )
      ]
  , event
      3
      "Central Kingsport"
      ["Hall School"]
      [
        ( "Congregational Hospital"
        , "You hear a voice speaking an unfamiliar language from one of the rooms (observation). If you pass, you write down what the rambling patient says; gain one clue from your neighborhood and a remnant. If you fail, you enter the wrong room and see a monstrous silhouette hidden behind a screen; become TAINTED."
        , Test Observation 0 (Seq [clue, remnants 1]) tainted
        )
      ,
        ( "Hall School"
        , "Something about the portrait of Eben Hall seems off, and when you examine the back of the canvas, you find strange markings. Gain a spell. The portrait depicts Hall next to a distinctively carved bookshelf. You search the school for that shelf (observation). If you pass, you find odd books about dreams there; gain one clue from your neighborhood."
        , Seq [spell, pass Observation 0 clue]
        )
      ,
        ( "Neil's Curiosity Shop"
        , "You turned your back for only a second, but when you look at the shop's collection again, something seems slightly different (observation). If you pass, you notice that a strange, silver puzzle box has been opened and whatever was once inside it is now gone; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      4
      "Central Kingsport"
      ["Hall School"]
      [
        ( "Congregational Hospital"
        , "A doctor says his clinic has lost a number of patients, and he's happy for any business. You may spend $1 for you or an ally to recover two health. If you do, he tells you that many of his clinic's regulars were behaving oddly before they stopped coming by; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "Hall School"
        , "Students have told Dean Bryant that they are seeing things that are not there. Gain one clue from your neighborhood. The school has hired an unorthodox specialist to help. You meet with this expert (influence). If you pass, you make a friend; gain an ally. If you fail, you are accused of causing these hallucinations and are kicked out; suffer one damage."
        , Seq [clue, Test Influence 0 ally (damage 1)]
        )
      ,
        ( "Neil's Curiosity Shop"
        , "A woman is posting pictures of people who have gone missing. She tells you everything the police know about these cases. Gain one clue from your neighborhood. Neil comforts the woman by saying that you are sure to figure out what happened and that he'll provide the tools you need to do so. You may spend $2 to gain one curio."
        , Seq [clue, mayPay (SpendMoney 2) curioItem]
        )
      ]
  , event
      5
      "Central Kingsport"
      ["Congregational Hospital"]
      [
        ( "Congregational Hospital"
        , "The evening staff does not allow visitors at this hour, but they can be paid to look the other way. You may spend $1 to get inside and explore the tunnels beneath the hospital. If you do, you find evidence that a large group of people came through here recently; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) clue
        )
      ,
        ( "Hall School"
        , "Walking through the halls you hear teachers lecturing and children reciting lessons. Somewhere in the building you hear different kinds of voices (observation). If you pass, you find one classroom where the students are reciting strange words in perfect unison; gain one clue from your neighborhood and one spell."
        , pass Observation 0 (Seq [clue, spell])
        )
      ,
        ( "Neil's Curiosity Shop"
        , "Neil tells you that your order has arrived, despite the fact that you have no recollection of ordering anything. Gain one curio. Neil reassures you that this has happened to other people. You may spend $1 to find out who else has forgotten placing an order. If you do, you gain one clue from your neighborhood."
        , Seq [curioItem, mayPay (SpendMoney 1) clue]
        )
      ]
  , event
      6
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "A specialist who deals in dream analysis tells you that many of her patients report nightmares about the moon. Gain one clue from your neighborhood. You may spend $1 to arrange for a session with this doctor. If you do, she guides you through your own anxieties; you or an ally may recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Independence Square"
        , "Several families have brought everything they own to the square to sell. You may buy one common item from the display for half price (rounded up). If you do, you speak to a woman who explains that they must rid themselves of what they own so that the Pale Lantern might shine eternal; gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") HalfPrice (Just 1) clue
        )
      ,
        ( "La Bella Luna"
        , "Two large employees escort a man out the door, while he insists that a dream told him he cannot lose. You try to arrange for him to be allowed back to the tables (influence). If you pass, he shares his winnings from the good fortune promised him by the Bloodless Man; gain one clue from your neighborhood and $3."
        , pass Influence 0 (Seq [clue, money 3])
        )
      ]
  , event
      7
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "The doctors attempt to treat you through hypnosis. You or an ally may recover two sanity. In a trance, you relive a memory of robed figures (observation). If you pass, you notice details you missed the first time; gain one clue from your neighborhood. If you fail, you notice only their claws; suffer one damage."
        , Seq [sanity 2, Test Observation 0 clue (damage 1)]
        )
      ,
        ( "Independence Square"
        , "The large, full moon is having a baleful effect on the large crowd gathered to stroll in its glow (will). If you pass, you realize that the mist that rolls across the ground is seeping out of the people walking the square; gain one clue from your neighborhood. If you fail, the moon continues to loom large in the sky for weeks; become TAINTED."
        , Test Will 0 clue tainted
        )
      ,
        ( "La Bella Luna"
        , "You cannot seem to lose at cards tonight. Gain $3. The dealer seems distracted by something and you try to start a conversation with him (influence). If you pass, you realize he is under the sway of the Lantern Club; gain one clue from your neighborhood. If you fail, he lashes out angrily and breaks a glass over your head; suffer one damage."
        , Seq [money 3, Test Influence 0 clue (damage 1)]
        )
      ]
  , event
      8
      "Downtown"
      ["La Bella Luna"]
      [
        ( "Arkham Asylum"
        , "You or an ally may recover two sanity. Some other patients seem beyond the asylum's ability to help. You may spend $1 to continue caring for a very troubled man. If you do, you learn that he was once a member of the Lantern Club; gain one clue from your neighborhood."
        , Seq [sanity 2, mayPay (SpendMoney 1) clue]
        )
      ,
        ( "Independence Square"
        , "You notice a tall man with a still, mask-like face on the other side of the square. You try not to lose him in the crowd and angle to catch a glimpse of his face (observation). If you pass, you look into his eyes as he tucks a package next to Founders Rock and see an endless vista of stars; gain one clue from your neighborhood and one common item."
        , pass Observation 0 (Seq [clue, commonItem])
        )
      ,
        ( "La Bella Luna"
        , "You sit across the table from an out-of-town visitor who casually mentions shipping a large quantity of merchandise from the Lantern Club. Gain one clue from your neighborhood. You scrutinize her tells and bet everything but your starting stake (observation). If you pass, you can tell that you have the better hand; gain $2."
        , Seq [clue, pass Observation 0 (money 2)]
        )
      ]
  , event
      9
      "Downtown"
      ["La Bella Luna"]
      [
        ( "Arkham Asylum"
        , "Staff and patients alike lie on the floor in a deep sleep, and a strange odor lingers in the air (observation). If you pass, you clear out the gas and wake everyone up to discover they all had the same dream; gain one clue from your neighborhood. If you fail, you fall unconscious and hit your head; suffer two damage."
        , Test Observation 0 clue (damage 2)
        )
      ,
        ( "Independence Square"
        , "An object waits for you inside the gazebo. Gain one curio. Once you accept this gift you hear your name whispered from somewhere nearby (observation). If you pass, you spot a stranger wearing the symbols of the Lantern Club; gain one clue from your neighborhood. If you fail, the whispers seem to come from inside your head; suffer one horror."
        , Seq [curioItem, Test Observation 0 clue (horror 1)]
        )
      ,
        ( "La Bella Luna"
        , "A sickly sweet mist fills the air and the walls begin to slither. You no longer trust your eyes, but you try to find the exit (observation). If you pass, the hallucinations clear and you see agents of the Club walking away quickly; gain one clue from your neighborhood. If you fail, you seem to be walking in a maze even after you leave; become TAINTED."
        , Test Observation 0 clue tainted
        )
      ]
  , event
      10
      "Downtown"
      ["Arkham Asylum", "La Bella Luna"]
      [
        ( "Arkham Asylum"
        , "Patients have written poems to articulate their fears, and you take comfort in seeing that others share your troubles. You may spend $1 for you or an ally to recover two sanity. If you do, one poem features a silhouetted abomination hidden behind a screen; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Independence Square"
        , "Tonight the lamp posts provide no light in the square and yet a woman has goods for sale. She tells you that she has traveled far by secret paths to bring you wonders from other worlds. Gain one clue from your neighborhood. As she tells you of her travels, you peruse what she's offering. You may buy any number of curios from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "La Bella Luna"
        , "Peter Clover seems to be in a haze tonight. \"It is a beautiful moon,\" he says, carelessly dropping some money to the floor. \"Have you ever been to the moon?\" Gain $2. All night long, you watch him (observation). If you pass, you see a woman wearing a lantern brooch whispering into his ear; gain one clue from your neighborhood."
        , Seq [money 2, pass Observation 0 clue]
        )
      ]
  , event
      11
      "Kingsport Harbor"
      ["St. Erasmus's Home"]
      [
        ( "North Point Lighthouse"
        , "Basil Elton tells you the legend of a creature who kept its true form hidden in silver light. Gain one clue from your neighborhood. The image stays in your mind as you add fuel to the lighthouse's red lamp (observation). If you pass, you feel you have impeded the creature; remove one doom from any space."
        , Seq [clue, pass Observation 0 (RemoveDoomFrom AnySpaceWithDoom (N 1))]
        )
      ,
        ( "The Rope and Anchor"
        , "Jonas Rigg hears every conversation that takes place here, but you have never known him to share other people's private affairs. You may spend $2 to buy a drink and convince him to make an exception. If you do, you or an ally may recover two sanity as he tells you how Club members identify themselves; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [sanity 2, clue])
        )
      ,
        ( "St. Erasmus's Home"
        , "You help the volunteer staff clean out the basement. You are welcome to keep anything you find. Gain one common item. You learn that the woman working with you has traveled far, and you try to get her to talk more (influence). If you pass, she tells you tales of a secret society she encountered once; gain one clue from your neighborhood."
        , Seq [commonItem, pass Influence 0 clue]
        )
      ]
  , event
      12
      "Kingsport Harbor"
      ["The Rope and Anchor"]
      [
        ( "North Point Lighthouse"
        , "Around the lighthouse, you spot small paper lanterns bobbing on the water. Each has a strange verse written on it. Remove one doom from any space. You try to see where they came from (observation). If you pass, you see a nightgaunt soaring across the sky; gain one clue from your neighborhood."
        , Seq [RemoveDoomFrom AnySpaceWithDoom (N 1), pass Observation 0 clue]
        )
      ,
        ( "The Rope and Anchor"
        , "The pub is crowded and dark but you feel as if someone is watching you (observation). If you pass, you find a bone-white half mask abandoned next to an address hastily written on a napkin; spawn one clue and gain one clue from your neighborhood. If you fail, you feel yourself being watched all the time; become TAINTED."
        , Test Observation 0 (Seq [SpawnOneClue, clue]) tainted
        )
      ,
        ( "St. Erasmus's Home"
        , "You ask about the donors who fund the home, but the staff wants to know if you are a member. You may spend a remnant to prove that you know the ways of the secret society and see a list of donors. If you do, you are sure that this is a list of members of the Lantern Club; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ]
  , event
      13
      "Kingsport Harbor"
      ["The Rope and Anchor"]
      [
        ( "North Point Lighthouse"
        , "At night, you see the White Ship surrounded by black sails. Gain one clue from your neighborhood. You try a charm to lure away the pirates (lore). If you pass, the White Ship sails freely; any investigator may move one space. If you fail, the pirate ships chase their prey over the horizon; suffer one horror."
        , Seq [clue, Test Lore 0 (Custom "any-investigator-moves") (horror 1)]
        )
      ,
        ( "The Rope and Anchor"
        , "After speaking to you for much of the evening, a boastful young woman offers you the assistance of her masked society. You may gain a DARK PACT to agree to her offer. If you do, she begins to tell you the secret history of the city and the Lantern Club; gain one clue from your neighborhood and spawn one clue."
        , mayPay (CostCondition "DARK PACT") (Seq [clue, SpawnOneClue])
        )
      ,
        ( "St. Erasmus's Home"
        , "One of the old sailors seems to be growing more delusional, and raves about \"men with still faces\" and \"great white toads.\" Gain one clue from your neighborhood. You may spend a remnant to prove to the man that he is not going mad. If you do, he gratefully gives you a gold coin; gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ]
  , event
      14
      "Kingsport Harbor"
      ["St. Erasmus's Home"]
      [
        ( "North Point Lighthouse"
        , "Basil Elton is in a trance, reciting nonsense rhymes about the moon (lore). If you pass, you interpret his dreams; remove one doom from any space and gain one clue from your neighborhood. If you fail, the voice speaking through the lighthouse keeper simply laughs at you; become TAINTED."
        , Test Lore 0 (Seq [RemoveDoomFrom AnySpaceWithDoom (N 1), clue]) tainted
        )
      ,
        ( "The Rope and Anchor"
        , "The exuberant crowd is singing sea chanteys. You or an ally may recover two sanity. It's hard to discern the lyrics (observation). If you pass, the song describes an encounter with a faceless pirate captain with no blood; gain one clue from your neighborhood. If you fail, the song gives you a headache but you don't know why; suffer one damage."
        , Seq [sanity 2, Test Observation 0 clue (damage 1)]
        )
      ,
        ( "St. Erasmus's Home"
        , "You speak with the oldest sailor in the home and ask him about the history of Kingsport (influence). If you pass, he shares a keepsake and tells you of a story from his childhood about the man who lives in the Strange High House on top of Kingsport Head; gain one clue from your neighborhood and one common item."
        , pass Influence 0 (Seq [clue, commonItem])
        )
      ]
  , event
      15
      "Kingsport Harbor"
      ["St. Erasmus's Home"]
      [
        ( "North Point Lighthouse"
        , "Someone has been inside the lighthouse, ransacking drawers and emptying cabinets. You look for an indication of what this intrudder was seeking (observation). If you pass, you find an old journal that mentions an ancient sect of the Bloodless One; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "The Rope and Anchor"
        , "Jonas Rigg confides in you that people he has known for years have been acting strangely. Gain one clue from your neighborhood. As you sit at the bar and watch his old friends behave as if they were in a trance, Jonas asks if you'd like anything to eat or drink. You may spend $1 for you or an ally to recover two sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 2)]
        )
      ,
        ( "St. Erasmus's Home"
        , "A new resident pays you to help him move in. Gain $2. As he unpacks, you take note of his possessions (observation). If you pass, you notice a painting of a wealthy man with a blank, bone-colored face awash in the light of the moon; gain one clue from your neighborhood. If you fail, your mind dwells on the moon; become TAINTED."
        , Seq [money 2, Test Observation 0 clue tainted]
        )
      ]
  , event
      16
      "Merchant District"
      ["Tick-Tock Club"]
      [
        ( "River Docks"
        , "Abandoned personal effects are scattered across the dock. Gain a common item. You may spend a remnant to reassure the frightened dock hands. If you do, they say they were left behind after strangers loaded glowing white spheres onto a ship with black sails; gain one clue from your neighborhood."
        , Seq [commonItem, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Tick-Tock Club"
        , "A cup of silvery tea is brought to your table and you are told it is a gift from management. Of course, you do not drink it. But you do watch another guest who had a similar gift brought to his table (observation). If you pass, you see him grow quiet and walk out as though in a trance; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Unvisited Isle"
        , "You find a crowd of people lying on the ground, asleep but speaking in a strange language (lore). If you pass, you understand the ancient dialect and intuit that they are sharing a dream about traveling to the moon on a ship; gain one clue from your neighborhood. If you fail, you grow nauseated and disoriented; suffer one damage."
        , Test Lore 0 clue (damage 1)
        )
      ]
  , event
      17
      "Merchant District"
      ["Tick-Tock Club"]
      [
        ( "River Docks"
        , "One of the dock workers claims that he loaded a number of very heavy crates onto a ship that refused to present a manifest, and says he thinks the people he spoke to were disguised somehow. Gain one clue from your neighborhood. You may spend a remnant to show him he was right and gain $3 in return."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Tick-Tock Club"
        , "An elegantly dressed woman sees you trying to bribe the bartender for information. She invites you to join her for a meal. You may spend $1 for you or an ally to recover two health. If you do, the woman tells you that people who drink too much often leave in the company of a stranger in a white mask; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "Unvisited Isle"
        , "Masked figures have gathered here to perform some sort of ritual that you hope to disrupt with a protective chant (lore). If you pass, you quietly alter their ritual and learn what their intent was; gain one clue from your neighborhood. If you fail, your useless chanting alerts them to your presence and they attack; suffer one damage."
        , Test Lore 0 clue (damage 1)
        )
      ]
  , event
      18
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "This warehouse has been ransacked by agents of the Lantern Club. You search the building for some indication of their motivation (observation). If you pass, you see they were after specific ingredients used in ancient rites and left some behind; gain one clue from your neighborhood and one remnant."
        , pass Observation 0 (Seq [clue, remnants 1])
        )
      ,
        ( "Tick-Tock Club"
        , "As you approach the entrance, one of the dishwashers stops you, warning, \"This is a bad time to be eating here. People have been disappearing.\" Gain one clue from your neighborhood. You may spend $2 to get the kitchen staff to prepare any dish you like to go. If you do, you or an ally may recover two health and two sanity."
        , Seq [clue, mayPay (SpendMoney 2) (RecoverBoth YouOrAlly (N 2) (N 2))]
        )
      ,
        ( "Unvisited Isle"
        , "You find footprints around the island's stone circle. Gain one clue from your neighborhood. Within the circle, it appears as if a large form has flattened the mud and grass. The whole area is covered in a repulsive black oil (will). If you pass, you push forward to the center stone where you find a bone token; gain a remnant."
        , Seq [clue, pass Will 0 (remnants 1)]
        )
      ]
  , event
      19
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "The harbor master says he knows nothing about reports of black-sailed ships. You may spend a remnant to prove to him they've been here and check his records. If you do, he tells you that someone falsely claimed they were bound for the moon; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ,
        ( "Tick-Tock Club"
        , "A manager offers you a free meal to move to a different table. You or an ally may recover one health and one sanity. They set up a screen by your old table to mask their special guest (observation). If you pass, you see members of the Lantern Club; gain one clue from your neighborhood. If you fail, all you see is an unsettling silhouette; become TAINTED."
        , Seq [RecoverBoth YouOrAlly (N 1) (N 1), Test Observation 0 clue tainted]
        )
      ,
        ( "Unvisited Isle"
        , "In the pitch black on this moonless night, you hear a faint cry for help (observation). If you pass, you reach an old woman who tells you that she has traveled the world of dreams for years; gain a remnant and one clue from your neighborhood. If you fail, the voice warns you of your impending doom, but you never find the source; become TAINTED."
        , Test Observation 0 (Seq [remnants 1, clue]) tainted
        )
      ]
  , event
      20
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "No one has been buried here for generations, but today you see fresh mounds of dirt. Gain one clue from your neighborhood. You start to dig up the shallow graves (strength). If you pass, you unearth a few enemies of the Lantern Club and document each of the victims; gain a remnant."
        , Seq [clue, pass Strength 0 (remnants 1)]
        )
      ,
        ( "St. Mary's Hospital"
        , "You know that the night watchman likes to play dice behind the hospital during his shift. You may spend $1 to get him to bring you hospital records and some painkillers. If you do, the files show that many patients have gone missing; gain one clue from your neighborhood and you or an ally may recover two health."
        , mayPay (SpendMoney 1) (Seq [clue, health 2])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The shop is closed for repairs. You may spend $2 to contribute to Miriam Beecher's expenses. If you do, she invites you in to chat about all the damage that was done to her store by a gang of puppet-like goons with bone-white masks over their eyes; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) clue
        )
      ]
  , event
      21
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "A dead body lies on the hill, amid their scattered possessions. Gain one common item. Upon closer inspection you see the body has been mutilated according to certain occult rituals (will). If you pass, you tamp down your fear and find script carved into the body; gain one clue from your neighborhood."
        , Seq [commonItem, pass Will 0 clue]
        )
      ,
        ( "St. Mary's Hospital"
        , "Before this patient slipped into a coma, they had been drawing surreal pictures of their vivid dreams. Gain a remnant. You look closely at the illustrations of a pirate ship sailing through the clouds (observation). If you pass, you see an image of a man in a bone mask at the prow of the ship; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Several masked figures stand around the shop. A figure you can't see all that clearly directs the group. Gain one clue from your neighborhood. When the man sees you, he offers to sell you dark secrets. Reveal the top two spells in the deck. You may buy one of them for half price (rounded up). Put the rest on the bottom of the deck."
        , Seq [clue, spells 2 (Just 1) HalfPrice]
        )
      ]
  , event
      22
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "Several masked socialites wander the hill as if in a trance. They stare up at the moon and whisper, leaving paper lanterns behind them (observation). If you pass, you steal an offering to Nyarlathotep in the name of the Bloodless Man; gain one clue from your neighborhood and one remnant."
        , pass Observation 0 (Seq [clue, remnants 1])
        )
      ,
        ( "St. Mary's Hospital"
        , "A visiting specialist offers a demonstration of acupuncture. You or an ally may recover three health. You feel much better, but something about the needles seems strange (observation). If you pass, the placement on your arm creates a symbol of the Pale Lantern; gain one clue from your neighborhood. If you fail, your flesh burns; suffer one damage."
        , Seq [health 3, Test Observation 0 clue (damage 1)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher possesses a book that describes the Pale Lantern in detail, but she refuses to let you see it. \"Dark magic,\" she says. \"Strictly forbidden.\" You try to sneak a look (observation). If you pass, you learn the cult's methods; gain a spell and one clue from your neighborhood. If you fail, Miriam stops you with a terrifying vision; suffer one horror."
        , Test Observation 0 (Seq [spell, clue]) (horror 1)
        )
      ]
  , event
      23
      "Uptown"
      ["St. Mary's Hospital", "Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "In the gloom, you hide and watch something digging through the earth with its claws (observation). If you pass, you see that the creature is a ghoul fishing an object out of a grave; gain one clue from your neighborhood and one common item. If you fail, the ghoul vomits on you; become TAINTED."
        , Test Observation 0 (Seq [clue, commonItem]) tainted
        )
      ,
        ( "St. Mary's Hospital"
        , "A nurse quietly tells you that she thinks the hospital's administrators have been acting strangely. When you meet them it's clear that they are under some sort of hypnosis. Gain one clue from your neighborhood. You may spend $1 to receive care for your injuries without keeping any records. If you do, you or an ally may recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "When you offer to help a customer work out a charm he just purchased, he shows you the formula. Gain a spell. You watch his efforts to perform the rite (observation). If you pass, you see a pin that identifies him as a new member of the Lantern Club; gain one clue from your neighborhood. If you fail, his ritual turns on you; become TAINTED."
        , Seq [spell, Test Observation 0 clue tainted]
        )
      ]
  , event
      24
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "Beneath your feet, you feel the ground heave up and down as if the whole hill were breathing (will). If you pass, the clouds part to reveal the moon, and the ground is solid once again; gain one clue from your neighborhood. If you fail, you fear your enemy is bigger than you thought; suffer one horror."
        , Test Will 0 clue (horror 1)
        )
      ,
        ( "St. Mary's Hospital"
        , "You recognize this doctor as a habitual gambler who is deep in debt. If you were to help cover her debt, she would happily help you in return. You may spend $1 for you or an ally to recover two health. If you do, she reveals that someone has blackmailed her into \"losing\" files for some kidnapped patients; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The proprietor silently shows you the final book of the Seven Cryptical Books of Hsan. It describes Nyarlathotep in all his forms. Gain one clue from your neighborhood. It also describes powerful ancient rituals. Reveal the top four spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
        , Seq [clue, spells 4 Nothing FullPrice]
        )
      ]
  ]
