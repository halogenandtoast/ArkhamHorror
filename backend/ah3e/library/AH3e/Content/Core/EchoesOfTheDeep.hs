{- | Echoes of the Deep.

R'lyeh is surfacing through Arkham. Clues on the sheet bring the old captain's
journal (card 29) and the breaches it locates (cards 31 to 35); doom on the sheet
brings the Servitor (card 30) and then Cthulhu itself (card 37), and the markers
the severed breaches leave are what weakens it (card 38).
-}
module AH3e.Content.Core.EchoesOfTheDeep (code, scenario, cards) where

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
code = "echoes-of-the-deep"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

{- | The two epic monsters wait in the archive: card 39 until card 30 spawns it,
card 40 until card 37 does.
-}
heldBack :: [CardCode]
heldBack = ["echoes-39", "echoes-40"]

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Echoes of the Deep"
    , expansion = CoreSet
    , startingSpace = spaceIdFor "Observatory"
    , reckoningText = "Each investigator suffers horror equal to the amount of doom in their space."
    , reckoning = ForInvestigators EveryInvestigator (SufferHorror (Counted DoomInYourSpace))
    , setupMap =
        buildMap
          [nb "Northside", nb "Downtown", nb "Merchant District", nb "Rivertown", nb "Miskatonic University"]
          [ StreetDef (nb "Northside") SideRight (nb "Downtown") Residential
          , StreetDef (nb "Northside") BottomRight (nb "Merchant District") Residential
          , StreetDef (nb "Downtown") BottomRight (nb "Rivertown") Bridge
          , StreetDef (nb "Merchant District") SideRight (nb "Rivertown") Residential
          , StreetDef (nb "Merchant District") BottomRight (nb "Miskatonic University") Scenic
          , StreetDef (nb "Rivertown") BottomLeft (nb "Miskatonic University") Scenic
          ]
    , monsters =
        [ ("hooded-stalker", 2)
        , ("occult-ritualist", 2)
        , ("rlyeh-guardian", 1)
        , -- every Deep One monster; setup keeps back the boxes that are not in play
          ("hybrid-thug", 2)
        , ("ocean-scion", 2)
        , ("river-skulk", 2)
        , ("sea-singer", 1)
        , ("shallows-predator", 2)
        , ("shoreline-brute", 1)
        , ("wake-titan", 1)
        , ("entranced-hybrid", 1)
        , ("frenzied-hunter", 1)
        ]
    , startingMonsters =
        [("river-skulk", spaceIdFor "River Docks"), ("hybrid-thug", spaceIdFor "Black Cave")]
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
          ["Train Station", "La Bella Luna", "River Docks", "Unvisited Isle", "Black Cave"]
    , startingMarkers = []
    , eventCards = [CardCode ("echoes-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , setAside = heldBack
    , codex = [2, 29, 30]
    , anomalySet = Just "Nightmare Breach"
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = archive <> events <> anomalies <> [servitor, cthulhu]

-- | Zero padded, so the card codes sort the way the cards are numbered.
pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

-- doom removal, as the anomaly cards word it
nearby, here, anywhere :: Int -> Effect
nearby n = RemoveDoomFrom SpaceInYourNeighborhood (N n)
here n = RemoveDoomFrom YourSpace (N n)
anywhere n = RemoveDoomFrom AnySpace (N n)

-- | "You may buy one X from the display. If you do, ..."
buyOneThen, buyOneHalfThen :: Trait -> Effect -> Effect
buyOneThen t = BuyFromDisplay (Just t) FullPrice (Just 1)
buyOneHalfThen t = BuyFromDisplay (Just t) HalfPrice (Just 1)

remnant :: Effect
remnant = remnants 1

archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n title front back =
  CardDef
    (CardCode ("echoes-" <> tshow n))
    title
    CoreSet
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

archive :: [CardDef]
archive =
  [ archiveCard
      29
      "The Stars Are Right"
      "[Objective] When there are three or more clues on the scenario sheet, flip this card."
      ( Just
          "Add card 31 to the codex. Set aside cards 32-35. Return this card to the archive."
      )
  , archiveCard
      30
      "Echoes of the Deep"
      "[Doom] When there is four or more doom on the scenario sheet, flip this card."
      ( Just
          "Take card 39 (Servitor of R'lyeh epic monster) and spawn it at the Unvisited Isle. Add card 37 to the codex and return this card to the archive."
      )
  , archiveCard
      31
      "Lost City of R'lyeh"
      "Action: You may spend two clues from the scenario sheet to find one of the locations where Arkham and R'lyeh are connected. If you do, take one card at random from the set-aside cards and attach it to its corresponding neighborhood deck. Perform this action only at the Observatory.\nWhen there are four markers on the scenario sheet, if card 38 is in the codex, flip this card and read \"Better Late than Never.\" Otherwise, flip this card and read \"To the River Docks!\""
      ( Just
          "To the River Docks!\nAdd card 36 to the codex and return this card to the archive.\n\nBetter Late than Never\nReturn this card to the archive."
      )
  , breach 32 "Rivertown" "strength"
  , breach 33 "Downtown" "observation"
  , breach 34 "Northside" "influence"
  , breach 35 "Miskatonic University" "will"
  , archiveCard
      36
      "Into the Breach"
      "Action: You may attempt to close the gate to R'lyeh (lore -2); you may spend any number of remnants to roll that many additional dice. If you pass, flip this card. Perform this action only at the River Docks."
      (Just "Investigators win the game!")
  , archiveCard
      37
      "R'lyeh Rising"
      "[Doom] When there is nine or more doom on the scenario sheet, flip this card."
      ( Just
          "Return card 36 to the archive (if it is in the codex). Take card 40 (Cthulhu epic monster) and spawn it at the River Docks. Add card 38 to the codex and return this card to the archive."
      )
  , archiveCard
      38
      "R'lyeh Rises"
      "The Cthulhu epic monster's health is reduced by three for each marker on the scenario sheet.\n[Objective] After the Cthulhu epic monster has been defeated, flip this card and read the \"That is Not Dead Which Can Eternal Lie\" effect.\n[Doom] When there is thirteen or more doom on the scenario sheet, flip this card and read the \"Strange Aeons Come\" effect."
      ( Just
          "That is Not Dead Which Can Eternal Lie\nInvestigators win the game!\n\nStrange Aeons Come\nInvestigators lose the game!"
      )
  ]

{- | Cards 32-35: one breach per neighborhood, severed by a test taken after an
encounter there. Each one flipped is a marker on the sheet, and four of them is
the whole city.
-}
breach :: Int -> Text -> Text -> CardDef
breach n hood skill =
  archiveCard
    n
    ("Nightmare Breach - " <> hood)
    ( "After you resolve a "
        <> hood
        <> " encounter, you may attempt to sever Arkham's connection with R'lyeh ("
        <> skill
        <> " -2); you may spend any number of remnants to roll that many additional dice. If you pass, flip this card."
    )
    (Just "Place one marker on the scenario sheet and return this card to the archive.")

{- | Card 39. Spawned by card 30 at the Unvisited Isle. Epic, so it goes back to the
archive rather than the monster deck when it leaves play.
-}
servitor :: CardDef
servitor =
  CardDef
    "echoes-39"
    "Servitor of R'lyeh"
    CoreSet
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn = CustomSpaceRule "Spawned by card 30 at the Unvisited Isle"
          , activation = Lurker (ForInvestigators EveryInvestigator (SufferHorror (N 1)))
          , speed = 0
          , traits = ["Star Spawn"]
          , health = 4
          , elite = 2
          , attackSkill = Strength
          , attackModifier = -2
          , evadeModifier = -1
          , damage = 2
          , horror = 2
          , remnant = True
          , keywords = [Massive]
          , epic = True
          , text =
              "Elite 2 (Has 2 additional health per investigator.) Massive (Servitor of R'lyeh engages and attacks each investigator in its space. It cannot be exhausted.) Lurker - Each investigator suffers one horror."
          }
    )

{- | Card 40. Spawned by card 37 at the River Docks, three health lighter for every
breach the investigators managed to sever first (card 38).
-}
cthulhu :: CardDef
cthulhu =
  CardDef
    "echoes-40"
    "Cthulhu"
    CoreSet
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn = CustomSpaceRule "Spawned by card 37 at the River Docks"
          , activation = Lurker (ForInvestigators EveryInvestigator (SufferHorror (N 1)))
          , speed = 0
          , traits = ["Ancient One"]
          , health = 8
          , elite = 4
          , attackSkill = Strength
          , attackModifier = -3
          , evadeModifier = -1
          , damage = 3
          , horror = 3
          , remnant = True
          , keywords = [Massive]
          , epic = True
          , text =
              "Elite 4 (Has 4 additional health per investigator.) Massive (Cthulhu engages and attacks each investigator in its space. It cannot be exhausted.) Lurker - Each investigator suffers one horror."
          }
    )

{- | One of the twenty-four event cards: the neighborhood it belongs to, the spaces
its doom symbols name, and an encounter for each of that neighborhood's spaces.
-}
event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("echoes-event-" <> pad n))
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
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "You rest for a moment in the peace and quiet of the lobby. You or an ally recovers two sanity. An orderly takes you to where a madman has scratched arcane symbols onto the walls of his cell (observation). If you pass, you recognize crude towers and strangely angled streets; you gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Independence Square"
        , "An entire family has disappeared, leaving all of their things behind. Footprints lead straight from their home into the river. You gain one clue from your neighborhood. Their cousins are selling their things in the square. You may buy one curio item from the display."
        , Seq [clue, buyOne "Curio"]
        )
      ,
        ( "La Bella Luna"
        , "Clubs flush, aces high. You can't lose. You gain $2. The other players seem distracted, and you ask them about it (influence). If you pass, they tell you about the strange dreams they have been having in which they walk deliberately into the ocean to sink beneath the waves; you gain one clue from your neighborhood."
        , Seq [money 2, pass Influence 0 clue]
        )
      ]
  , event
      2
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "\"I can let you in to see him for five minutes,\" says the orderly. You may spend $1 to bribe him. If you do, you are able to interrogate the bug-eyed man from Innsmouth and learn what you can about his origin; you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ,
        ( "Independence Square"
        , "At Founder's Rock, a cloaked figure gestures you over and hands you a gift. You gain one curio item. Beneath his hood, you catch a glimpse of his fishlike visage (will). If you pass, you smile politely; you gain one clue from your neighborhood. If you fail, you recoil from the sight; you suffer two horror."
        , Seq [curioItem, Test Will 0 clue (horror 2)]
        )
      ,
        ( "La Bella Luna"
        , "You're about to roll the dice when seawater comes flooding down the stairs. You gain one clue from your neighborhood. You try to grab some of the stuff being carried away by the water (observation). If you pass, Peter Clover pays you for your help in saving his property; you gain $3."
        , Seq [clue, pass Observation 0 (money 3)]
        )
      ]
  , event
      3
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "Nurse Heather gives you a stern look. \"A lot of strange folk coming in from Innsmouth these days.\" You gain one clue from your neighborhood. \"I suppose you'll be wanting to see the doctor?\" You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Independence Square"
        , "Something about the geometry in the square seems odd to you (will). If you pass, you follow the strange angles to where they converge and find something useful; you gain one common item and one clue from your neighborhood. If you fail, you experience a severe bout of vertigo; you suffer one horror."
        , Test Will 0 (Seq [commonItem, clue]) (horror 1)
        )
      ,
        ( "La Bella Luna"
        , "Something about the mysterious woman has you on edge and you rifle through her things when she is occupied (observation). If you pass, you find a strange wooden idol in her purse; you gain $3 and one clue from your neighborhood. If you fail, you are caught and kicked out of the club; you suffer two damage."
        , Test Observation 0 (Seq [money 3, clue]) (damage 2)
        )
      ]
  , event
      4
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "\"Sit down,\" says Doctor Badoe. \"We're all friends here.\" You may spend $1 to participate in the group therapy session. If you do, many of the other participants speak of dreams in which they are drowning; you or an ally recovers two sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Independence Square"
        , "A kid with a three card monte setup will talk about what he saw if you buy something from his uncle. You may buy one common item from the display. If you do, he tells you that he saw dark figures emerging from the river the night before; you gain one clue from your neighborhood."
        , buyOneThen "Common" clue
        )
      ,
        ( "La Bella Luna"
        , "You find a clump of seaweed on the floor after a party from out of town gets up to leave. You gain one clue from your neighborhood. You try to convince the chef that seaweed is a delicacy (influence). If you pass, he is overwhelmingly grateful and happily pays you for it; you gain $3."
        , Seq [clue, pass Influence 0 (money 3)]
        )
      ]
  , event
      5
      "Merchant District"
      ["River Docks", "River Docks"]
      [
        ( "River Docks"
        , "You find the strongbox in the crew quarters of the Innsmouth merchantman, and you think you can pick the lock (observation). If you pass, the lock clicks open and you find tarnished gold ingots and a wooden idol of a squid-like monstrosity; you gain $3 and one clue from your neighborhood."
        , pass Observation 0 (Seq [money 3, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "The piano player tickles the ivories like nothing you've ever heard. You may spend $1 to tip him. If you do, you chat with him through the evening and he tells you that a dream of the deep sea inspired the song; you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ,
        ( "Unvisited Isle"
        , "Fish-like men and women dance and cavort through the night around a raging bonfire. You gain one clue from your neighborhood. You investigate the area after they leave (lore). If you pass, you recognize the components used in their ritual; you gain one remnant."
        , Seq [clue, pass Lore 0 remnant]
        )
      ]
  , event
      6
      "Merchant District"
      ["River Docks"]
      [
        ( "River Docks"
        , "The midshipman might know about the strange creatures you've encountered recently. You may spend one remnant to show him what you found. If you do, he has seen something similar in Innsmouth and offers to buy it; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "You may spend $1 to buy a round of drinks for a group of stevedores in hopes that they will open up about the strange events at the docks. If you do, you spend several pleasant hours in their company; you or an ally recovers two sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Unvisited Isle"
        , "You find the bodies of a dozen or more of the fish-men. You gain one clue from your neighborhood. You dig through the rotting corpses (will). If you pass, you find something; you gain one curio item. If you fail, their restless spirits seem to settle oppressively on your mind; you become CURSED."
        , Seq [clue, Test Will 0 curioItem cursed]
        )
      ]
  , event
      7
      "Merchant District"
      ["River Docks", "Unvisited Isle"]
      [
        ( "River Docks"
        , "You haul a disfigured sailor's corpse from the water and begin searching his pockets. You gain $2. You may spend one remnant to compare the wounds on his body to wounds you have seen before. If you do, you think you know what killed him; you gain one clue from your neighborhood."
        , Seq [money 2, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Tick-Tock Club"
        , "An Innsmouth sailor sits alone in the club. You gain one clue from your neighborhood. He tells you he has a surefire way to cure a hangover. You may spend $1 to learn the remedy. If you do, he drops a copper penny and a small iron nail into your drink; you or an ally recovers two health and two sanity."
        , Seq [clue, mayPay (SpendMoney 1) (RecoverBoth YouOrAlly (N 2) (N 2))]
        )
      ,
        ( "Unvisited Isle"
        , "Slimy, wriggly polyps drop from the treetops and plop onto your head and shoulders. You gain one remnant. Some atavistic part of your mind is drooling at the thought of eating them (will). If you pass, you resist the insane urge; you gain one clue from your neighborhood."
        , Seq [remnant, pass Will 0 clue]
        )
      ]
  , event
      8
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "You eavesdrop by a lighted window and hear voices conversing in an unknown tongue. You gain one clue from your neighborhood. When they emerge, you pretend to be looking to get rid of some \"goods.\" You may spend one remnant. If you do, they engage you in trade; you gain one common item."
        , Seq [clue, mayPay (SpendRemnants 1) commonItem]
        )
      ,
        ( "Tick-Tock Club"
        , "\"Army surplus painkillers,\" says Joey \"the Rat.\" You may spend $1 to buy some off him. If you do, you or an ally recovers three health. He seems on edge (observation). If you pass, you notice his wounds and he tells you that some Innsmouth sailors beat him up; you gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (health 3), pass Observation 0 clue]
        )
      ,
        ( "Unvisited Isle"
        , "You overhear an argument in an alien dialect (lore). If you pass, you understand enough to work out the location of a buried treasure; you gain one curio item and one clue from your neighborhood. If you fail, the words ring in your mind, shattering your composure; you suffer two horror."
        , Test Lore 0 (Seq [curioItem, clue]) (horror 2)
        )
      ]
  , event
      9
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "The drunken sailors will trade cargo for anything they can get their hands on. You may spend one remnant to gain one common item. If you do, they tell you of strange emerald lights and dark figures moving beneath the waves; you gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [commonItem, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "The doctor at the bar lost his license for treating one of the disfigured fish-men, and he tells you all about it. You gain one clue from your neighborhood. You may spend $1 to buy refreshments. If you do, he drinks the night away; you or an ally recovers three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Unvisited Isle"
        , "The only way to cross is over the slippery rocks (will). If you pass, you maintain your composure and cross in time to recover the fish-man's corpse before the tide takes it away; you gain one remnant and one clue from your neighborhood. If you fail, you panic and slip; you suffer one damage."
        , Test Will 0 (Seq [remnant, clue]) (damage 1)
        )
      ]
  , event
      10
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "You sit on a bench in the fog, listening to the whispered conversation of the unseen Innsmouth sailors while you wait for your buyer to arrive. You gain one clue from your neighborhood. Eventually, your buyer shows. You may spend one remnant to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Tick-Tock Club"
        , "The woman claims that she can heal the wounds in your mind just by pressing different spots on your palms. You or an ally recovers two sanity. You observe her technique (observation). If you pass, you notice her lips moving in a silent chant as she works; you gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Unvisited Isle"
        , "The inlet doesn't appear on any map, but you think you can find it from what you remember of your dream (will). If you pass, you find a hidden cache at your destination; you gain one remnant and one clue from your neighborhood. If you fail, you become lost for hours; you become delayed."
        , Test Will 0 (Seq [remnant, clue]) delayed
        )
      ]
  , event
      11
      "Miskatonic University"
      ["Observatory"]
      [
        ( "Observatory"
        , "The desk is strewn with all manner of disorganized notes (lore). If you pass, you order them well enough to make a list of the reagents you need; you gain one remnant and one clue from your neighborhood. If you fail, hours later you are no further than you were at the start; you become delayed."
        , Test Lore 0 (Seq [remnant, clue]) delayed
        )
      ,
        ( "Orne Library"
        , "You come across a waterlogged tome that triggers something in your memory (lore). If you pass, you find a companion volume to the tome deeper in the stacks; you gain one spell and one clue from your neighborhood. If you fail, the overpowering stench of fish is too distracting; you suffer one horror."
        , Test Lore 0 (Seq [spell, clue]) (horror 1)
        )
      ,
        ( "Science Building"
        , "The students swarm around you like minnows, telling you everything they know about your find. You gain one clue from your neighborhood. They offer to buy your specimen for research purposes. You may spend one remnant to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ]
  , event
      12
      "Miskatonic University"
      ["Observatory", "Science Building"]
      [
        ( "Observatory"
        , "You move the dial a touch more clockwise and put your eye to a standing telescope (observation). If you pass, you see a monstrous dark shape moving rapidly beneath the surface of the river and note its location; you gain one clue from your neighborhood and remove one doom from any space."
        , pass Observation 0 (Seq [clue, anywhere 1])
        )
      ,
        ( "Orne Library"
        , "Henry Armitage's new colleague, an archaeologist, introduces himself and shakes your hand. You gain one clue from your neighborhood. He grills you on your knowledge of lost cities (lore). If you pass, he entrusts you with something he learned; you gain one spell. If you fail, he is dismissive of you."
        , Seq [clue, pass Lore 0 spell]
        )
      ,
        ( "Science Building"
        , "You are conscripted rather unexpectedly to assist with an experiment. You gain $2. The web-footed creature you are dissecting looks very familiar (observation). If you pass, you think it shares some facial features with a man you saw walking the streets; you gain one clue from your neighborhood."
        , Seq [money 2, pass Observation 0 clue]
        )
      ]
  , event
      13
      "Miskatonic University"
      ["Science Building"]
      [
        ( "Observatory"
        , "You watch in amazement as stars blink in and out of existence. You remove one doom from any space. You observe the events this sets into motion on the streets below (observation). If you pass, you see cloaked figures roaming the streets; you gain one clue from your neighborhood and spawn one clue."
        , Seq [anywhere 1, pass Observation 0 (Seq [clue, SpawnOneClue])]
        )
      ,
        ( "Orne Library"
        , "A group of students is speaking in hushed whispers about the visions they've shared. You gain one clue from your neighborhood. You think you have read something similar in a book about lucid dreams (lore). If you pass, you learn to harness the arcane energy of dreams; you gain one spell."
        , Seq [clue, pass Lore 0 spell]
        )
      ,
        ( "Science Building"
        , "The researchers at Miskatonic are seeking any information on the strange creatures. You may spend one remnant to sell them some evidence. If you do, they discover traces of plant life native to the South Pacific within the creature's stomach; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ]
  , event
      14
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "A learned astronomer lectures about the effects of the moon on the tide. You gain one clue from your neighborhood. You search through the astronomer's notes (observation). If you pass, you find a strange wooden idol of a squid-like dragon creature in his bag; you gain one remnant."
        , Seq [clue, pass Observation 0 remnant]
        )
      ,
        ( "Orne Library"
        , "There's something irregular about von Junzt's book, besides the fact that it's chained to the lectern. You gain one spell. You flip through the ancient tome's dusty pages (observation). If you pass, you find two pages stuck together that reveal the names of ancient beings; you gain one clue from your neighborhood."
        , Seq [spell, pass Observation 0 clue]
        )
      ,
        ( "Science Building"
        , "The technician tells you to be quiet while he adjusts the photographic oscillograph. You may spend one remnant to sell him an object to experiment on. If you do, he shows you the strange waveforms, almost like spires and towers, that the thing gives off; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ]
  , event
      15
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "Professor Tremaine lectures, and invites her audience to take turns at the telescope. You gain one clue from your neighborhood. You look for anything out of the ordinary (lore). If you pass, you notice a star where one shouldn't be; you remove one doom from any space."
        , Seq [clue, pass Lore 0 (anywhere 1)]
        )
      ,
        ( "Orne Library"
        , "The tag on the wax cylinder says \"1890.\" You struggle to make out anything comprehensible in the hissing, scratchy sound (lore). If you pass, you make out the sound of waves crashing and voices chanting and record what you hear; you gain one spell and one clue from your neighborhood."
        , pass Lore 0 (Seq [spell, clue])
        )
      ,
        ( "Science Building"
        , "The students at Miskatonic University claim to have discovered the secrets of alchemy. You may spend one remnant to donate materials for their test. If you do, the raw materials are transformed into glittering rubies before your eyes; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ]
  , event
      16
      "Northside"
      ["Arkham Advertiser"]
      [
        ( "Arkham Advertiser"
        , "The Advertiser is looking for photographic evidence of the creatures prowling the streets. You may spend one remnant to show them physical proof. If you do, they pay you handsomely and share information with you regarding sightings in the area; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Curiositie Shoppe"
        , "Oliver Thomas shoves the strange onyx statuette into your hand as you enter, claiming to just want it out of the shop. You gain one clue from your neighborhood. Relieved, he asks you if there is anything else you would like. You may buy any number of curio items from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "A stranger begins talking with you as if they know you. You gain one ally. After a moment, they breathe a sigh of relief and explain that someone was following them (observation). If you pass, you spot a woman with strange bulging eyes slip away into the crowd; you gain one clue from your neighborhood."
        , Seq [ally, pass Observation 0 clue]
        )
      ]
  , event
      17
      "Northside"
      ["Curiositie Shoppe", "Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "Doyle Jefferies tells two young men that having identical dreams isn't a newsworthy occurrence. You gain one clue from your neighborhood. You interject, saying that there might be something to it. You may spend one remnant to provide proof. If you do, the editor is intrigued; you gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "\"Washed up from the river along with some other... things,\" Oliver tells you. \"You can have it.\" You gain one curio item. Something about the object is familiar (observation). If you pass, you think you've seen one of the dockyard workers carrying it around; you gain one clue from your neighborhood."
        , Seq [curioItem, pass Observation 0 clue]
        )
      ,
        ( "Train Station"
        , "The platform is slick with water and seaweed. You shout a warning to someone walking by (influence). If you pass, they are gracious; you gain one ally and one clue from your neighborhood. If you fail, the hapless passerby falls off of the platform into the path of an oncoming train; you suffer two horror."
        , Test Influence 0 (Seq [ally, clue]) (horror 2)
        )
      ]
  , event
      18
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "The workmen need an extra hand to help get the press running again. You gain $2. Nothing seems to be working, and you look for the problem (observation). If you pass, you find a gummed-up gear compartment filled with soft, tubular things that smell of the sea; you gain one clue from your neighborhood."
        , Seq [money 2, pass Observation 0 clue]
        )
      ,
        ( "Curiositie Shoppe"
        , "\"Lots of those washing up on shore lately,\" says Oliver Thomas. \"Maybe a ship sank somewhere.\" You may buy one curio item from the display for half price (rounded up). If you do, you can feel the call of the ocean by simply holding your recent find; you gain one clue from your neighborhood."
        , buyOneHalfThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "The station is abuzz with gossip, and you make out the word \"island\" several times. You gain one clue from your neighborhood. You spot something valuable lying on the tracks (will). If you pass, you have the wherewithal to leap down and grab it in time; you gain one common item."
        , Seq [clue, pass Will 0 commonItem]
        )
      ]
  , event
      19
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "Editor Doyle Jefferies is contemplating a photo of a bug-eyed, lipless man. You may spend one remnant to show that the photo might not be fake. If you do, he thanks you with his wallet and lets you have a closer look at the photograph; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Curiositie Shoppe"
        , "\"That old fellow Marsh on Central Hill died intestate,\" says Oliver Thomas. \"So I bought all his weird stuff for a song.\" You may buy one curio item from the display for half price (rounded up). If you do, Oliver shows you Marsh's obituary; you gain one clue from your neighborhood."
        , buyOneHalfThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "\"Hot peanuts?\" offers Andre Hopkins, holding out the steaming bag. You share a bite and some gossip. You gain one clue from your neighborhood. He is reluctant to discuss some topics (influence). If you pass, he gives you something that washed up from the river; you gain one common item."
        , Seq [clue, pass Influence 0 commonItem]
        )
      ]
  , event
      20
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "Strange voices echo from somewhere up ahead. You struggle to understand the words (lore). If you pass, you copy them down: \"Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn;\" you gain one spell and one clue from your neighborhood. If you fail, the words are disturbing; you suffer two horror."
        , Test Lore 0 (Seq [spell, clue]) (horror 2)
        )
      ,
        ( "General Store"
        , "Several young men in front of Schoffner's are on edge. Apparently, some sort of fish creature attempted to break into the store last night. You gain one clue from your neighborhood. Seeing that you are no threat, they let you pass. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "You attempt to exhume old man Marsh's body, but it is tiring work (strength). If you pass, the skeleton you find is more fish than human; you gain one remnant and one clue from your neighborhood. If you fail, your pockets are filled with maggots after sitting on the ground to rest; you suffer one horror."
        , Test Strength 0 (Seq [remnant, clue]) (horror 1)
        )
      ]
  , event
      21
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "Bats hang from the ceiling and the ground is slick with their droppings. It smells horrid (will). If you pass, you excavate a stinking, seaweed-covered tome from the guano; you gain one spell and one clue from your neighborhood. If you fail, you stumble as you retch from the smell; you suffer one damage."
        , Test Will 0 (Seq [spell, clue]) (damage 1)
        )
      ,
        ( "General Store"
        , "The leaky roof has flooded the display, so the shopkeeper gives you one for free. You gain one common item. You get on a step stool to help him plug the leak (observation). If you pass, you are surprised to discover that the roof is leaking saltwater; you gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Graveyard"
        , "Plants you know to be native to the South Pacific are growing over some of the graves. You gain one clue from your neighborhood. You work to dig some of them up (strength). If you pass, you uncover strange, inhuman skeletons, more fish than human; you gain one remnant."
        , Seq [clue, pass Strength 0 remnant]
        )
      ]
  , event
      22
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "Ancient pictographs cover the walls of the cave. You gain one clue from your neighborhood. Something about the symbols stays with you (will). If you pass, you master the invading presence in your mind; you gain one spell. If you fail, you have a vision of your death by drowning; you suffer one horror."
        , Seq [clue, Test Will 0 spell (horror 1)]
        )
      ,
        ( "General Store"
        , "It seems like the whole town is here, talking in worried voices. You may buy one common item from the display for half price (rounded up). If you do, while you are checking out, you hear concerns about dark shapes moving about the Unvisited Isle; you gain one clue from your neighborhood."
        , buyOneHalfThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "You join the gravediggers for a friendly game of cards and do quite nicely for yourself. You gain $2. You study your opponents for tics and tells (observation). If you pass, you notice one of them is wearing a driftwood amulet, probably lifted from one of the graves; you gain one clue from your neighborhood."
        , Seq [money 2, pass Observation 0 clue]
        )
      ]
  , event
      23
      "Rivertown"
      ["General Store", "General Store"]
      [
        ( "Black Cave"
        , "The rain has washed away the dirt and revealed a once-buried object. You gain one curio item. You search the area for anything else that might have surfaced (observation). If you pass, you discover a cluster of massive piscine eggs when the tide is at its lowest; you gain one clue from your neighborhood."
        , Seq [curioItem, pass Observation 0 clue]
        )
      ,
        ( "General Store"
        , "Your coupon is a little waterlogged, but the shopkeeper will still accept it. \"Lotta folk coming in here drenched to the bone,\" he says. You gain one clue from your neighborhood. \"What can I do for you today?\" he asks. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "A dog bounds happily up to you, something covered in drool hanging from its mouth. You wrinkle your nose and gingerly try to remove it (will). If you pass, you extract a grime-covered wallet filled with cash and a map of the river docks; you gain $3 and one clue from your neighborhood."
        , pass Will 0 (Seq [money 3, clue])
        )
      ]
  , event
      24
      "Rivertown"
      ["General Store"]
      [
        ( "Black Cave"
        , "You wander about in a maze of twisty little passages, all alike, and attempt to carefully map out the cave network before it floods again (lore). If you pass, a dead-end hides a long-forgotten cache; you gain one curio item and one clue from your neighborhood. If you fail, you are lost; you suffer one horror."
        , Test Lore 0 (Seq [curioItem, clue]) (horror 1)
        )
      ,
        ( "General Store"
        , "Nathan says they are overstocked. You may buy one common item from the display for half price (rounded up). If you do, he mentions that a large portion of their customer base are dock workers, many of whom have not been by for some time; you gain one clue from your neighborhood."
        , buyOneHalfThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "You crouch down to retrieve a glinting Spanish doubloon from the roots of an old tree. What on Earth is that doing there? You gain one clue from your neighborhood. The coin fills you with dread and you consider tossing it (will). If you pass, you hold onto it long enough to sell it; you gain $3."
        , Seq [clue, pass Will 0 (money 3)]
        )
      ]
  ]

-- Nightmare Breach anomalies
anomaly :: Int -> [((Int, Maybe Int), Text, Effect)] -> CardDef
anomaly n sections =
  CardDef
    (CardCode ("nightmare-breach-" <> pad n))
    "Nightmare Breach"
    CoreSet
    1
    ( AnomalyCard
        (AnomalyDef "Nightmare Breach" [(range, Encounter txt eff) | (range, txt, eff) <- sections])
    )

anomalies :: [CardDef]
anomalies =
  [ anomaly
      1
      [
        ( (0, Just 1)
        , "\"They picked the wrong town to mess with, I'll tell you that much,\" says Joey the Rat. \"Slip me a few bucks, and I'll take care of it. I know a few guys -- that fish thing won't know what hit it.\" You may spend $2 to remove one doom from any space in your neighborhood."
        , mayPay (SpendMoney 2) (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "The street slopes at a steep angle (will). If you pass, you keep your balance as you traverse the elongated street toward the promise of relative safety; you remove one doom from your space. If you fail, you realize that the angles you are seeing are impossible; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "The scale-covered man babbles at you in an unfamiliar language (lore -2). If you pass, you understand his warning and accept the carved stone he presses into your hand; you remove three doom from your space and gain one remnant. If you fail, he grows angry; you become CURSED."
        , Test Lore (-2) (Seq [here 3, remnant]) cursed
        )
      ]
  , anomaly
      2
      [
        ( (0, Just 1)
        , "A basalt obelisk rises from the street, covered with odd pictographs (lore). If you pass, the images convey the steps of a ritual; you remove one doom from any space in your neighborhood. If you fail, you come away from the monument bleeding from your eyes and ears; you suffer one damage."
        , Test Lore 0 (nearby 1) (damage 1)
        )
      ,
        ( (2, Just 2)
        , "A scaly hand reaches from the window of a parked car and claws at your arm. You suffer two damage. Stumbling away in pain, you pluck the barbed nails from your bloodied flesh. You remove two doom from your space and gain one remnant."
        , Seq [damage 2, here 2, remnant]
        )
      ,
        ( (3, Nothing)
        , "The city has flooded, and the streets are awash with jelly-like creatures (will -1). If you pass, you wade boldly through the fleshy creatures; you remove two doom from your space and gain one remnant. If you fail, some of them attach to your arm and slip under your skin; you become CURSED."
        , Test Will (-1) (Seq [here 2, remnant]) cursed
        )
      ]
  , anomaly
      3
      [
        ( (0, Just 1)
        , "You page frantically through the star atlas, comparing the charts to the night sky (lore). If you pass, you use the charts to navigate the flooded streets; you remove one doom from any space in your neighborhood. If you fail, the stars that should be overhead are missing; you suffer one horror."
        , Test Lore 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "The automobile's engine is covered in slime (will -1). If you pass, you remove a mass of sticky seaweed wrapped around the fan belt; you remove two doom from your space and gain one remnant. If you fail, a brown-green fibrous thing detaches itself from the gearbox and scuttles away; you suffer two horror."
        , Test Will (-1) (Seq [here 2, remnant]) (horror 2)
        )
      ,
        ( (3, Nothing)
        , "You have a vision of drifting peacefully amongst the beautiful coral spires of Y'ha-nthlei. When you come to, you are soaked with seawater, and you long to return to the dream. You may gain a DARK PACT condition to remove three doom from your space and gain one remnant."
        , mayPay (CostCondition "DARK PACT") (Seq [here 3, remnant])
        )
      ]
  , anomaly
      4
      [
        ( (0, Just 1)
        , "A child's voice echoes amongst the green spires (will). If you pass, you realize that your mind is playing tricks on you; you remove one doom from any space in your neighborhood. If you fail, you push yourself to the point of exhaustion searching for the source of the voice; you suffer one damage."
        , Test Will 0 (nearby 1) (damage 1)
        )
      ,
        ( (2, Just 2)
        , "The scaly street vendor has a cart heaped with seafood that you have never seen before (lore). If you pass, you recognize the strange creatures as monkfish; you remove one doom from your space. If you fail, sticky tentacles reach weakly from the cart toward your face; you suffer one horror."
        , Test Lore 0 (here 1) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "Arkham's streets are deserted, save for the footprints of several unknown creatures (lore -2). If you pass, you notice a webbed pattern in each set of tracks; you remove three doom from your space and gain one remnant. If you fail, the creatures ambush you while you study their tracks; you suffer two damage."
        , Test Lore (-2) (Seq [here 3, remnant]) (damage 2)
        )
      ]
  , anomaly
      5
      [
        ( (0, Just 1)
        , "There is something strange about the water that has flooded the street (will). If you pass, you grimace and reach into the water, pulling away a small, deformed fish; you remove one doom from any space in your neighborhood. If you fail, the sickening smell of the water makes you nauseated; you suffer one horror."
        , Test Will 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "You attempt to understand the fish-woman's guttural language (lore -1). If you pass, she hands you a small round stone; you remove two doom from your space and gain one remnant. If you fail, she sinks her gleaming teeth into your shoulder; you suffer one damage and one horror."
        , Test Lore (-1) (Seq [here 2, remnant]) (harm 1 1)
        )
      ,
        ( (3, Nothing)
        , "Your voice catches and you struggle to scream out a warning to a man who strays too near the deep water (will -1). If you pass, you warn him in time; you remove two doom from your space and gain one remnant. If you fail, dark figures leap out and pull him under; you suffer three horror."
        , Test Will (-1) (Seq [here 2, remnant]) (horror 3)
        )
      ]
  , anomaly
      6
      [
        ( (0, Just 0)
        , "The dead man's notebook contains scrawled passages copied from the Cthaat Aquadingen (lore). If you pass, you learn the secret of the emerald spires; you remove one doom from any space in your neighborhood. If you fail, the book flashes violet and singes your hands; you suffer two damage."
        , Test Lore 0 (nearby 1) (damage 2)
        )
      ,
        ( (1, Just 2)
        , "Slimy weeds hang thickly from the hotel windows, glistening with rivulets of dark water. You suffer two horror. You collect a sample of the alien plant matter from a stinking pile at the base of the building. You remove two doom from your space and gain one remnant."
        , Seq [horror 2, here 2, remnant]
        )
      ,
        ( (3, Nothing)
        , "Ooze drips down the walls and putrid, black heaps fill the corners (will -2). If you pass, you carefully pick through the mess and find a fragment of greenish stone; you remove three doom from your space and gain one remnant. If you fail, the black ooze begins crawling toward you; you suffer two horror."
        , Test Will (-2) (Seq [here 3, remnant]) (horror 2)
        )
      ]
  , anomaly
      7
      [
        ( (0, Just 0)
        , "You bristle as you feel dark eyes upon you (will). If you pass, you keep calm and stay sharp; you remove one doom from any space in your neighborhood. If you fail, you break into a run, stumbling and slipping over seaweed that lashes out at your legs as you pass; you suffer one damage and one horror."
        , Test Will 0 (nearby 1) (harm 1 1)
        )
      ,
        ( (1, Just 2)
        , "The notes scribbled on the napkin are smeared and difficult to decipher (lore). If you pass, you realize it is an account of the last human moments of one of the scaly creatures; you remove one doom from your space. If you fail, you give up and throw the napkin into the trash."
        , pass Lore 0 (here 1)
        )
      ,
        ( (3, Nothing)
        , "You try to speak the fish-men's language (lore -1). If you pass, you pass by them unharmed; you remove two doom from your space and gain one remnant. If you fail, the monstrous creatures rush you, eyes bulging and hook-like teeth smiling in wide, lipless mouths; you suffer three damage."
        , Test Lore (-1) (Seq [here 2, remnant]) (damage 3)
        )
      ]
  , anomaly
      8
      [
        ( (0, Just 0)
        , "The brass bell is incised with a prayer of blessing around the rim (lore). If you pass, you read it aloud, enunciating carefully; you remove one doom from any space in your neighborhood. If you fail, the words warp and bend as they leave your mouth; you become CURSED."
        , Test Lore 0 (nearby 1) cursed
        )
      ,
        ( (1, Just 1)
        , "The sun shines peacefully upon the sunken, waterlogged streets of Arkham, but something seems off (will). If you pass, you shake yourself from your stupor, remembering that none of this is normal; you remove one doom from your space. If you fail, you smile happily to yourself."
        , pass Will 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "\"Come, quickly!\" The man gestures from the doorway. \"We need your help! We've got one of them tied up in the basement. Damn thing came up through the drain.\" You may become delayed to remove two doom from your space and gain one remnant."
        , mayPay CostDelayed (Seq [here 2, remnant])
        )
      ]
  , anomaly
      9
      [
        ( (0, Just 0)
        , "The tattooed man spins a hair-raising tale (will). If you pass, he expresses his approval and offers his assistance; you remove one doom from any space in your neighborhood. If you fail, he gestures with his hand and you reel back, your skin blackened and burning; you suffer one damage and one horror."
        , Test Will 0 (nearby 1) (harm 1 1)
        )
      ,
        ( (1, Just 1)
        , "Something is following you. You cannot see it, but you know it is there. Does it want something of yours? Would it stop if it got it? Perhaps you could draw it in another direction with the scent of spoiled flesh. You may spend one remnant to remove one doom from your space."
        , mayPay (SpendRemnants 1) (here 1)
        )
      ,
        ( (2, Nothing)
        , "A towering green spire thrusts upward from the earth (lore -1). If you pass, you snap off a piece of the spire for later analysis; you remove two doom from your space and gain one remnant. If you fail, an emerald shard burrows its way into your flesh; you suffer one damage and one horror."
        , Test Lore (-1) (Seq [here 2, remnant]) (harm 1 1)
        )
      ]
  , anomaly
      10
      [
        ( (0, Just 0)
        , "A coral reef grows where the parking garage once stood (lore). If you pass, you realize that the building is still there beneath the reef; you remove one doom from any space in your neighborhood. If you fail, you wonder whether anything is left of Arkham at all; you suffer two horror."
        , Test Lore 0 (nearby 1) (horror 2)
        )
      ,
        ( (1, Just 1)
        , "The doorway is clogged with a congealed mass. You take a deep breath and head through (will). If you pass, you emerge, gasping, from the other side; you remove one doom from your space. If you fail, you become stuck and begin hyperventilating, collapsing into a panicked heap; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (2, Nothing)
        , "A small worm-like creature swims up from the drain into the sink water (lore -2). If you pass, you smash it into a fine pulp with a nearby encyclopedia; you remove three doom from your space and gain one remnant. If you fail, it burrows into your wrist; you suffer two damage."
        , Test Lore (-2) (Seq [here 3, remnant]) (damage 2)
        )
      ]
  , anomaly
      11
      [
        ( (0, Just 0)
        , "You try to soothe a spooked horse (will). If you pass, you calm it long enough to detach awful things that look like overgrown sea lice that are sucking at its neck; you remove one doom from any space in your neighborhood. If you fail, the horse kicks you to the ground; you suffer two damage."
        , Test Will 0 (nearby 1) (damage 2)
        )
      ,
        ( (1, Just 1)
        , "The stars seem bigger than usual. You struggle to recall a passage from the Necronomicon (lore). If you pass, you think it to be a trick and keep your head down, avoiding the unblinking sky; you remove one doom from your space. If you fail, you stare at the constellations. All of them wrong."
        , pass Lore 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "A shadow falls over the street, cast by something gargantuan (will -1). If you pass, you keep calm and don't draw any attention to yourself; you remove two doom from your space and gain one remnant. If you fail, the creature's gaze falls upon you; you suffer one damage and one horror."
        , Test Will (-1) (Seq [here 2, remnant]) (harm 1 1)
        )
      ]
  , anomaly
      12
      [
        ( (0, Just 0)
        , "The sound of chanting comes from a tunnel beneath the street (lore). If you pass, some untapped part of your brain begins to make sense of the words; you remove one doom from any space in your neighborhood. If you fail, you find yourself chanting the words as well; you suffer two horror."
        , Test Lore 0 (nearby 1) (horror 2)
        )
      ,
        ( (1, Just 1)
        , "You wake from a dream and desperately try to make sense of what you saw (will). If you pass, scenes of sunken cyclopean towers gradually fade from your memory; you remove one doom from your space. If you fail, you believe you have just witnessed a glimpse of Arkham's future; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (2, Nothing)
        , "The statues are coated with milfoil and draped with weeds. You try to clean them (will -2). If you pass, you keep some of the plant for yourself; you remove three doom from your space and gain one remnant. If you fail, your cleaning uncovers drowned, bloated corpses, not statues; you suffer two horror."
        , Test Will (-2) (Seq [here 3, remnant]) (horror 2)
        )
      ]
  ]
