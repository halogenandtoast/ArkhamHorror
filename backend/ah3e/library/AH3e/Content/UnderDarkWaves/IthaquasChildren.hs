-- | Ithaqua's Children: the Wind-Walker reaches down from the frozen north.
module AH3e.Content.UnderDarkWaves.IthaquasChildren (code, scenario, cards) where

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
code = "ithaquas-children"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

-- | How far off Arkham the two towns stand, and how far one is lifted and the other dropped.
aside, lift :: Double
aside = -0.15
lift = 0.15

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Ithaqua's Children"
    , expansion = UnderDarkWaves
    , startingSpace = spaceIdFor "Velma's Diner"
    , reckoningText = "Place one doom in any space in Innsmouth and one doom in any space in Kingsport."
    , reckoning = Custom "ithaqua-reckoning"
    , setupMap =
        buildMapOf
          [ nb "Downtown"
          , nb "Northside"
          , nb "Rivertown"
          , nb "Easttown"
          , nb "Southside"
          , nb "Innsmouth Shore"
          , nb "Central Kingsport"
          ]
          [ StreetDef (nb "Downtown") BottomLeft (nb "Northside") Scenic
          , StreetDef (nb "Downtown") BottomRight (nb "Rivertown") Bridge
          , StreetDef (nb "Northside") SideRight (nb "Rivertown") Bridge
          , StreetDef (nb "Rivertown") SideRight (nb "Easttown") Residential
          , StreetDef (nb "Rivertown") BottomRight (nb "Southside") Residential
          , StreetDef (nb "Easttown") BottomRight (nb "Southside") Scenic
          ]
          noPieces
            { routes =
                [ RouteDef (nb "Innsmouth Shore") SideLeft CountryRoad
                , RouteDef (nb "Innsmouth Shore") SideRight FerryTerminal
                , RouteDef (nb "Downtown") TopRight CountryRoad
                , RouteDef (nb "Northside") BottomRight TrainPlatform
                , RouteDef (nb "Easttown") SideRight FerryTerminal
                , RouteDef (nb "Central Kingsport") SideLeft TrainPlatform
                , RouteDef (nb "Central Kingsport") SideRight CountryRoad
                , RouteDef (nb "Southside") BottomRight CountryRoad
                ]
            , {- Innsmouth Shore and Central Kingsport reach the map by travel route alone,
              so they are only set down beside it: Innsmouth Shore off Downtown's left, and
              Central Kingsport off Southside's. -}
              clusters =
                [ ClusterLink (nb "Downtown") SideLeft (nb "Innsmouth Shore")
                , ClusterLink (nb "Southside") SideLeft (nb "Central Kingsport")
                ]
            , {- The two towns stand clear of Arkham in one column off its left, Innsmouth
              lifted above the map and Kingsport dropped below it by as much. Kingsport's
              own place is a lattice step to the right of Innsmouth's, so its shift carries
              that step as well. -}
              nudges =
                [ nudge (nb "Innsmouth Shore") aside lift
                , nudge (nb "Central Kingsport") (aside - tileStep) (-lift)
                ]
            }
    , monsters =
        [ ("accursed-somnambulist", 2)
        , ("altered-servant", 2)
        , ("avian-thrall", 1)
        , ("high-priest", 1)
        , ("hulking-thrall", 2)
        , ("icebound-captive", 2)
        , ("lupine-thrall", 1)
        , ("ravenous-predator", 1)
        , ("terrified-wanderer", 2)
        , -- every shantak monster
          ("dread-shadow", 1)
        , ("guardian-beast", 1)
        ]
    , startingMonsters = [("high-priest", spaceIdFor "Gilman House")]
    , mythosCup =
        [ (SpreadDoomToken, 3)
        , (SpawnMonsterToken, 2)
        , (SpawnClueToken, 2)
        , (ReadHeadlineToken, 2)
        , (GateBurstToken, 1)
        , (ReckoningToken, 1)
        , (SpreadTerrorToken, 1)
        , (BlankToken, 2)
        ]
    , startingDoom =
        map
          spaceIdFor
          [ "Marsh Refinery"
          , "Arkham Asylum"
          , "Train Station"
          , "Hibb's Roadhouse"
          , "Congregational Hospital"
          , "Hall School"
          , "Black Cave"
          , "South Church"
          ]
    , startingMarkers = []
    , startingBystanders = []
    , eventCards = [CardCode ("ithaqua-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , -- the wendigo card 91 calls up, and the Ithaqua of cards 99 and 102
      setAside = ["archive-104", "archive-105"]
    , codex = [61, 91]
    , anomalySet = Nothing
    , terrorSet = Just "Frozen City"
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
    (CardCode ("ithaqua-event-" <> pad n))
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
      ["Hall School", "Hall School"]
      [
        ( "Congregational Hospital"
        , "A string of smoldering footsteps leads up to the hospital door and then disappears. Gain one clue from your neighborhood. The terrified doctors want to leave, worried about their families. You may spend $1 to convince them to stay. If you do, you or an ally may recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "Hall School"
        , "All the doors of the school have frozen shut. You walk around, searching for a different way in (observation). If you pass, you breach a side entrance and discover a strange symbol painted on the inside of each door; gain one clue from your neighborhood and one spell. If you fail, the cold takes a toll as you walk through the wintery wind; suffer one damage."
        , Test Observation 0 (Seq [clue, spell]) (damage 1)
        )
      ,
        ( "Neil's Curiosity Shop"
        , "Neil is thrilled to see that you have pushed through the snow drifts and offers you a deal on anything that catches your eye. You may buy one curio from the display for half price (rounded up). If you do, Neil thanks you and warns you never to answer if the Wind-Walker calls your name; gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Curio") HalfPrice (Just 1) clue
        )
      ]
  , event
      2
      "Central Kingsport"
      ["Hall School"]
      [
        ( "Congregational Hospital"
        , "A patient has been restrained until the police arrive after trying to bite her doctor (observation). If you pass, you write down the chant she whispers to the wind spirits; gain one remnant and one clue from your neighborhood. If you fail, you notice nothing, but the room grows cold as the grave; become TAINTED."
        , Test Observation 0 (Seq [remnants 1, clue]) tainted
        )
      ,
        ( "Hall School"
        , "A stranger has come to help search for a few missing students. Gain an ally. You join this new friend in scouring the area for lost kids (influence). If you pass, you find a small group of frost-bitten young women who tell you of the strange whispers they heard echoing through the air; gain one clue from your neighborhood."
        , Seq [ally, pass Influence 0 clue]
        )
      ,
        ( "Neil's Curiosity Shop"
        , "Someone dropped their purchase outside in the snow. Gain one curio. You ask Neil what happened to the person who bought this. When he hesitates to answer, you may spend one remnant to reassure him. If you do, he warns you that cannibals are roaming the streets; gain one clue from your neighborhood."
        , Seq [curioItem, mayPay (SpendRemnants 1) clue]
        )
      ]
  , event
      3
      "Central Kingsport"
      ["Neil's Curiosity Shop"]
      [
        ( "Congregational Hospital"
        , "A young man without a coat staggers, shivering, into the waiting room. You may spend $1 to provide him with the means to buy warm clothes. If you do, he shows you a drawing of a creature he saw outside and tells you where it was headed; gain one clue from your neighborhood and one remnant."
        , mayPay (SpendMoney 1) (Seq [clue, remnants 1])
        )
      ,
        ( "Hall School"
        , "Something large has broken into the school. It must have been wounded, leaving behind a trail of black ichor. Gain one clue from your neighborhood. You track the beast through the halls (observation). If you pass, the trail ends next to strange runes; gain one spell. If you fail, you never find it, but feel it is always with you; become TAINTED."
        , Seq [clue, Test Observation 0 spell tainted]
        )
      ,
        ( "Neil's Curiosity Shop"
        , "You hide next to Neil as the two of you try to covertly watch a strange, foul caricature of Neil amble through the shop, searching for something (observation). If you pass, you see this eerie duplicate choose a particular carving and leave with it; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      4
      "Central Kingsport"
      ["Neil's Curiosity Shop"]
      [
        ( "Congregational Hospital"
        , "Given the terrible weather, the hospital is offering medical help for free. You or an ally may recover two health. You may spend $1 to help the hospital recover its costs. If you do, the doctors confide in you some of the strange things they have seen in the icy winds; gain one clue from your neighborhood."
        , Seq [health 2, mayPay (SpendMoney 1) clue]
        )
      ,
        ( "Hall School"
        , "Victoria Bryant tells you that the school is rapidly running out of food and fuel. You make some phone calls to fortify the building's reserves (influence). If you pass, truck drivers arrive with supplies and strange stories of creatures lurking in the night; gain one clue from your neighborhood. If you fail, you can't find anyone willing to brave the roads."
        , pass Influence 0 clue
        )
      ,
        ( "Neil's Curiosity Shop"
        , "An old hunter brings his bric-a-brac to sell to Neil. He tells you that he's seen this evil cold before, up north. Gain one clue from your neighborhood. Neil wants to buy the man's wares but he is low on cash. Both of them look at you expectantly, hoping you will make some purchases. You may buy any number of curios from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ]
  , event
      5
      "Downtown"
      ["Independence Square", "Independence Square"]
      [
        ( "Arkham Asylum"
        , "You or an ally may recover two sanity. Your nurse confides that a couple of patients have gone missing without a trace, so you help the staff search (observation). If you pass, you find bones hidden in the locker of an orderly who has himself disappeared; gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Independence Square"
        , "A vagabond huddles under the gazebo for warmth. You may spend $2 to make sure he gets food and a warm place to stay. If you do, he tells you of all the creatures he saw and the cryptic voices he heard during the long night; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) clue
        )
      ,
        ( "La Bella Luna"
        , "You find a wad of cash hidden under a table. Gain $3. Someone else is sneaking through the closed casino, and you hide to see who it is (observation). If you pass, you see a hideous duplicate of yourself shuffling around; gain one clue from your neighborhood. If you fail, your double points at you and howls; become TAINTED."
        , Seq [money 3, Test Observation 0 clue tainted]
        )
      ]
  , event
      6
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "Doctors and patients alike huddle around the warm radiators. You or an ally may recover two sanity. You hear a voice in the howling wind outside (observation). If you pass, you hear it speak to the patients; gain one clue from your neighborhood. If you fail, you only hear it say your name; become TAINTED."
        , Seq [sanity 2, Test Observation 0 clue tainted]
        )
      ,
        ( "Independence Square"
        , "A hunter prowls through the park amid the winter storm. He tells you that he is tracking an antlered beast that walks on two feet. Gain one clue from your neighborhood. You join the hunt despite the cold (will). If you pass, the tracks lead to an abandoned backpack; gain one curio. If you fail, the beast finds you and howls loudly; become TAINTED."
        , Seq [clue, Test Will 0 curioItem tainted]
        )
      ,
        ( "La Bella Luna"
        , "One of the patrons hits big at the roulette wheel and the whole room cheers loudly for the winner, but you think you heard something else rumbling under the din of the crowd (observation). If you pass, you heard a monstrous howl coming from outside; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      7
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "You'll need to pay the short-handed staff a bit extra to make sure you are taken care of. You may spend $1 for you or an ally to recover two sanity. If you do, the doctor chats with you about the strange things people claimed to see in the wintery night; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Independence Square"
        , "Most of the usual street vendors have moved on to warmer places, but one cart seems abandoned with its wares available for the taking. Gain one common item. You search the area for a sign of what happened to the cart's owner (observation). If you pass, you find a large gnawed bone nearby; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "La Bella Luna"
        , "One of the guests lashes out violently when he loses every hand, and Peter Clover wants him gone. You try to calm him down (influence). If you pass, he relaxes enough to be escorted out and the staff thanks you, saying there is a rumor that the man has engaged in cannibalism; gain one clue from your neighborhood and $3."
        , pass Influence 0 (Seq [clue, money 3])
        )
      ]
  , event
      8
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "Despite the arctic wind, the roadhouse is still a hub of activity. All it takes is buying one person a drink to get the whole crowd sharing stories of the things they have seen walking through the storm. You may spend $1 to gain one clue from your neighborhood and for you or an ally to recover two sanity."
        , mayPay (SpendMoney 1) (Seq [clue, sanity 2])
        )
      ,
        ( "Police Station"
        , "All the windows have been shattered and the station is filled with snow. You search the building for any police or prisoners who may still be here (observation). If you pass, you find the half-eaten body of one of the prisoners, clutching the only item he had to defend himself with; gain a common item and one clue from your neighborhood."
        , pass Observation 0 (Seq [commonItem, clue])
        )
      ,
        ( "Velma's Diner"
        , "Velma warily locks the door behind you. She serves coffee and food and then \"forgets\" to ask for any money. You or an ally may recover two health. You look around for a reason she locked the door (observation). If you pass, you see people across the street with a dangerous, deadly hunger in their eyes; gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ]
  , event
      9
      "Easttown"
      ["Hibb's Roadhouse", "Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "You or an ally may recover two sanity. You examine your meal served with your drink very closely (observation). If you pass, you are positive this meat came from a person and someone here served it to you; gain one clue from your neighborhood. If you fail, it seems fine to you; become TAINTED."
        , Seq [sanity 2, Test Observation 0 clue tainted]
        )
      ,
        ( "Police Station"
        , "The station is dark, but it appears someone who has been camping out in the lobby left their things behind. Gain one common item. You wait to catch this person by surprise (observation). If you pass, you speak to this terrified young woman who tells you she is hiding from a hunting, humanoid beast; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Velma's Diner"
        , "Velma has barricaded her diner to prevent looting. She tells you several names of decent people who have begun to wander the streets. Gain one clue from your neighborhood. You believe the best way to help is to make sure the diner's supplies are paid for and eaten. You may spend $1 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ]
  , event
      10
      "Easttown"
      ["Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "Two customers get into a brutal, ferocious fight, biting each other like ravenous animals. Gain one clue from your neighborhood. After they are pulled apart, you know you could restore a festive mood by buying drinks for the other patrons. You may pay $1 for you or an ally to recover two sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 2)]
        )
      ,
        ( "Police Station"
        , "The police are logging a large number of looted items. \"Somebody even hit the butcher shop,\" says a patrolman, indicating a parcel of raw meat. Gain one clue from your neighborhood. You ask what else they've found (influence). If you pass, they look the other way; gain one common item. If you fail, the red meat is too tantalizing; become TAINTED."
        , Seq [clue, Test Influence 0 commonItem tainted]
        )
      ,
        ( "Velma's Diner"
        , "One of the other patrons suddenly bolts out into the snow. You step outside and take a look (observation). If you pass, you follow a trail of discarded clothing and find him howling at the wind; gain one clue from your neighborhood. If you fail, you hear only the wind and begin to grow more and more famished; become TAINTED."
        , Test Observation 0 clue tainted
        )
      ]
  , event
      11
      "Innsmouth Shore"
      ["Marsh Refinery", "Marsh Refinery"]
      [
        ( "Falcon Point"
        , "The fire on the beach is still smoldering, but when you search the camp, you find only abandoned supplies. Gain one curio. You may suffer two damage to stay out in the freezing wind, searching for the camp's owner. If you do, you find her frozen solid; gain one clue from your neighborhood."
        , Seq [curioItem, mayPay (CostDamage 2) clue]
        )
      ,
        ( "Gilman House"
        , "The man in the room next to yours keeps calling out to the wind, pleading for its help (influence). If you pass, you calm him down and he tells you about the voices on the wind that promise to make him whole again; gain one clue from your neighborhood. If you fail, the look in his eye warns you to flee, but the chill wind follows you; become TAINTED."
        , Test Influence 0 clue tainted
        )
      ,
        ( "Marsh Refinery"
        , "There are more guards at the refinery than usual. When you see one of the hulking Marsh cousins move away from the building to check on something, you follow him discreetly (observation). If you pass, you watch something huge lurking in the fog snuff out his lantern and consume him; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      12
      "Innsmouth Shore"
      ["Falcon Point", "Falcon Point"]
      [
        ( "Falcon Point"
        , "The ocean fog clears and you find the frozen body of a fish-like man. Gain one clue from your neighborhood. You pick up his shovel and continue digging (strength). If you pass, you find buried gold; gain $3. If you fail, you stop digging and stare into the dead bulbous eyes of the corpse; become TAINTED."
        , Seq [clue, Test Strength 0 (money 3) tainted]
        )
      ,
        ( "Gilman House"
        , "The police are here investigating an attack on one of the guests. Constable Ropes tells you what he knows about the would-be cannibal. Gain one clue from your neighborhood. You may spend one remnant to provide real evidence to arrest this lunatic. If you do, you or an ally recover two sanity, knowing that Ropes is likely to catch the correct person."
        , Seq [clue, mayPay (SpendRemnants 1) (sanity 2)]
        )
      ,
        ( "Marsh Refinery"
        , "The refinery is closed due to the snow, but a mechanic is working on the furnace. You ask her about Innsmouth (influence). If you pass, she warns you about the change that happens to the people of this village; gain one clue from your neighborhood and one remnant. If you fail, she tells you that you belong here; become TAINTED."
        , Test Influence 0 (Seq [clue, remnants 1]) tainted
        )
      ]
  , event
      13
      "Innsmouth Shore"
      ["Falcon Point"]
      [
        ( "Falcon Point"
        , "The night is cold but crystal clear. You see flashes of light in the sky. You try to see the source of these sparks (observation). If you pass, you see a creature walking through and across the winds with flames erupting with each step; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Gilman House"
        , "As the snow falls outside, you rest next to a fire and spend the night in comfort. You or an ally may recover two sanity. In the morning, you find large, misshapen footprints in the snow and follow them (observation). If you pass, you discover an ancient symbol scratched on the door to the hotel's cellar; gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Marsh Refinery"
        , "In the middle of the night, hulking fish-like creatures march through the snow to the refinery. You watch carefully as they deliver large crates. Gain one clue from your neighborhood. Once they leave, you search the area for anything they may have left behind (observation). If you pass, you find a map to a point beyond the reef; gain one remnant."
        , Seq [clue, pass Observation 0 (remnants 1)]
        )
      ]
  , event
      14
      "Innsmouth Shore"
      ["Marsh Refinery"]
      [
        ( "Falcon Point"
        , "The deeper you venture into the fog, the hungrier you get. You may continue to let your hunger gnaw at you and suffer one damage. If you do, voices calling from the mist lead you to a frozen corpse clutching a fistful of coins; gain one clue from your neighborhood and $3."
        , mayPay (CostDamage 1) (Seq [clue, money 3])
        )
      ,
        ( "Gilman House"
        , "Othera Gilman labors to clear piles of snow from the front entrance. As you help her, she wonders aloud what has made the weather so deadly. You may spend one remnant to explain the history of the wendigo to her. If you do, she is fascinated and tells the history of Innsmouth in return; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ,
        ( "Marsh Refinery"
        , "A terrified Innsmouth child saw something tear the refinery door off its hinges in the night. They describe the creature and point out where it went. Spawn one clue. You may become delayed to search the building for additional damage. If you do, you see several crates have been pried open; gain one clue from your neighborhood."
        , Seq [SpawnOneClue, mayPay CostDelayed clue]
        )
      ]
  , event
      15
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "You search the archives for anything useful (observation). If you pass, you find an account of a hunting party that encountered some sort of monster; gain one clue from your neighborhood. If you fail, each story you read mentions meat and you get hungrier and hungrier; become TAINTED."
        , Test Observation 0 clue tainted
        )
      ,
        ( "Curiositie Shoppe"
        , "For some reason, the storm outside does not feel quite so oppressive while you are inside this shop. You search the shop to see if something specific makes this place feel safer (observation). If you pass, you spot the five pointed star of an elder sign carved over the door; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Train Station"
        , "The train arrived despite the storm and your friend is here! Gain an ally. You notice the crew unloading a large number of crates. You try to see what is in them (observation). If you pass, you confirm that someone is importing an incredible amount of food; gain one clue from your neighborhood."
        , Seq [ally, pass Observation 0 clue]
        )
      ]
  , event
      16
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "The paper is offering a cash reward for anyone who can explain the unseasonable cold snap. You hear many stories to corroborate your own, but no one has any solid evidence. Gain one clue from your neighborhood. You may spend one remnant to claim the reward yourself and gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "Many stores cannot get deliveries due to the storm, but a driver is here to drop off a crate. You talk to him about things he has seen on the road. Gain one clue from your neighborhood. The shop has a full inventory now and the time is ripe to make some purchases. You may buy any number of curios from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "A stranger, waiting for a late train, asks for your help getting the porter to say what is going on (influence). If you pass, she tells you and your new friend that the train is delayed but still on its way; gain one clue from your neighborhood and an ally. If you fail, she refuses to say, which leads you to believe that the train crashed; suffer one horror."
        , Test Influence 0 (Seq [clue, ally]) (horror 1)
        )
      ]
  , event
      17
      "Northside"
      ["Arkham Advertiser", "Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "Between the people who are missing and those trapped by the storm, the paper is desperate to find someone willing to write an article. Gain $2. Minnie Klein is skeptical, but happy to help with your research. You may spend one remnant to gain one clue from your neighborhood."
        , Seq [money 2, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Curiositie Shoppe"
        , "You pick up a book about Hyperborea but the shopkeeper immediately warns you against reading such things. You may become TAINTED to take the book regardless and gain a curio tome. If you do, you learn about the mythical prison that once held Ithaqua; gain one clue from your neighborhood."
        , May "Become TAINTED to take the book" (Seq [tainted, tomeItem, clue])
        )
      ,
        ( "Train Station"
        , "A porter unloading your cargo on the icy platform claims something tried to push the train off of the track and shows you massive claw marks on the side of a boxcar (will). If you pass, gain one clue from your neighborhood and one common item. If you fail, fear paralyzes you even after hypothermia sets in; suffer one damage."
        , Test Will 0 (Seq [clue, commonItem]) (damage 1)
        )
      ]
  , event
      18
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "In the dark, you can hear a monstrous voice reciting an incantation. Gain one spell. The voice fills your heart with terror (will). If you pass, you know the voice is carried on the wind; gain one clue from your neighborhood. If you fail, you find yourself sounding more like that voice; become TAINTED."
        , Seq [spell, Test Will 0 clue tainted]
        )
      ,
        ( "General Store"
        , "Ravenous looters run rampant through the store, gorging themselves on whatever food they can find. You hide in the storeroom with Davy Schoffner and listen to their raid (observation). If you pass, you hear that with each bite they eat, their appetites grow larger and larger; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Graveyard"
        , "With the icy fog as thick as it is in the graveyard, it is almost impossible to navigate from one side to the other (observation). If you pass, you not only find your way, you see deep, unshod footprints left by something large and almost human; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      19
      "Rivertown"
      ["Graveyard", "Graveyard"]
      [
        ( "Black Cave"
        , "Whoever has been living here has painted crude symbols and images on the walls. Gain one clue from your neighborhood as you study the pictographs (lore). If you pass, you know where to search; gain one curio. If you fail, you do not understand but grow famished; become TAINTED."
        , Seq [clue, Test Lore 0 curioItem tainted]
        )
      ,
        ( "General Store"
        , "There is a hunter's moon out tonight. Outside, you see misshapen footprints in the snow. Gain one clue from your neighborhood. The shopkeeper intends to close the shop now, so he asks you to make your final purchases. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "A large, fur-covered humanoid pursues a mourning widow (strength). If you pass, the monstrosity flees from you and the widow thanks you with both cash and the story of a similar beast her husband once hunted; gain one clue from your neighborhood and $3. If you fail, the beast looks you in the eyes; become TAINTED."
        , Test Strength 0 (Seq [clue, money 3]) tainted
        )
      ]
  , event
      20
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The wind and snow here make it nearly impossible to see anything (observation). If you pass, you feel certain the storm is worse here than anywhere else, as though the blizzard were blowing from the arcane runes carved in the cave walls; gain one clue from your neighborhood and one spell."
        , pass Observation 0 (Seq [clue, spell])
        )
      ,
        ( "General Store"
        , "\"Help me!\" shouts the shopkeeper. You drag his injured body into his shop and he tells you to equip yourself. Gain one common item. You look for a sign of the attack (observation). If you pass, you see the bloodied snow where the thing bit its victim; gain one clue from your neighborhood. If you fail, it catches you with a sneak attack; suffer one damage."
        , Seq [commonItem, Test Observation 0 clue (damage 1)]
        )
      ,
        ( "Graveyard"
        , "You find the contents of someone's handbag thrown about the graveyard. Gain $3. Walking a little farther through the cold foggy night, you find the mangled corpse of a woman, badly mauled (will). If you pass, you examine the corpse more clearly and see that the bite marks aren't from an animal; gain one clue from your neighborhood."
        , Seq [money 3, pass Will 0 clue]
        )
      ]
  , event
      21
      "Rivertown"
      ["Black Cave", "General Store"]
      [
        ( "Black Cave"
        , "You are shocked by the number of human bones you have found in the cave (will). If you pass, you sort through the remains and find a number of personal items to identify the victims; gain one clue from your neighborhood. If you fail, you are paralyzed with terror; suffer one horror."
        , Test Will 0 clue (horror 1)
        )
      ,
        ( "General Store"
        , "Customers must have thought the snow would close the store because you are the only one here and you see some bargains. You may buy one common item from the display for half price (rounded up). If you do, Davy Schoffner tells you that he went hunting in Canada once, and this winter feels the same; gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") HalfPrice (Just 1) clue
        )
      ,
        ( "Graveyard"
        , "You notice that the grass and flowers in this part of the graveyard have frozen completely solid. Gain one clue from your neighborhood. Before you step away, an enormous fur-covered creature steps into view (will). If you pass, you steal a sample of its fur and escape; gain one remnant. If you fail, you collapse and whimper; suffer two horror."
        , Seq [clue, Test Will 0 (remnants 1) (horror 2)]
        )
      ]
  , event
      22
      "Southside"
      ["Historical Society", "Historical Society"]
      [
        ( "Historical Society"
        , "You stand casually near the door and eavesdrop on the proceedings behind it (observation). If you pass, you spot and befriend a stranger, also listening in on the grave debate over the historic significance of this record-breaking winter storm; gain one clue from your neighborhood and one ally."
        , pass Observation 0 (Seq [clue, ally])
        )
      ,
        ( "Ma's Boarding House"
        , "Ma tells you that all her rooms were full, but all her guests disappeared into the night. You search each room (observation). If you pass, it appears that each ate a large meal before going outside without a coat; gain one clue from your neighborhood. If you fail, you hear a loud howl outside before you feel something beckon to you; become TAINTED."
        , Test Observation 0 clue tainted
        )
      ,
        ( "South Church"
        , "Arkham's destitute residents thank you for helping them find shelter in the church. You or an ally may recover two sanity. Later, when they think no one can hear, they have whispered conversations (observation). If you pass, you hear about the people that took to looting and the terrible fate they met; gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ]
  , event
      23
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "Given the low turnout for tonight's seminar, Mr. Peabody is happy to talk to you at length and answer all your questions (influence). If you pass, he eagerly tells you about Inuit folklore; gain one clue from your neighborhood. If you fail, his answers just make you hungry; become TAINTED."
        , Test Influence 0 clue tainted
        )
      ,
        ( "Ma's Boarding House"
        , "Ma Mathison tells you that an unusually large number of guests have gone missing or skipped out on their bills. Gain one clue from your neighborhood. She is eager for any paying business she can get. \"This blasted storm has darn near ruined me!\" You may spend $1 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "South Church"
        , "Father Michael pensively confides that the confessions he's heard lately have been unusually upsetting. Gain one clue from your neighborhood. He is happy to help you quiet your own thoughts, regardless of your faith, and asks only for a donation to help the poor. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ]
  , event
      24
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "One of the lecturers shares information about the legendary wendigo, but laughingly says it is just a story. Gain one clue from your neighborhood. She is skeptical, but you think she might be genuinely helpful if you prove some of what you've seen. You may spend one remnant to gain one curio."
        , Seq [clue, mayPay (SpendRemnants 1) curioItem]
        )
      ,
        ( "Ma's Boarding House"
        , "Ma is sick and you thought it wise to call a doctor to pay her a visit. As long as he is here, he offers to take a look at you, as well. You or an ally may recover two health. It occurs to you that a little rest might give you the opportunity to track the monstrosity that hunts near here. You may become delayed to gain one clue from your neighborhood."
        , Seq [health 2, mayPay CostDelayed clue]
        )
      ,
        ( "South Church"
        , "Dozens of the city's poor have come inside the church to seek shelter from the storm. It would take you a long time to speak to everyone about what they have seen hunting in the storm, but you think it might be worthwhile. You may become delayed to gain one clue from your neighborhood."
        , mayPay CostDelayed clue
        )
      ]
  ]
