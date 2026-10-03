-- | Tyrants of Ruin: the tyrants of Y'ha-nthlei stir beneath Devil Reef.
module AH3e.Content.UnderDarkWaves.TyrantsOfRuin (code, scenario, cards) where

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
code = "tyrants-of-ruin"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

-- | How far Innsmouth stands off Arkham: out to the right, and lifted.
out, lift :: Double
out = 0.37
lift = 0.15

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Tyrants of Ruin"
    , expansion = UnderDarkWaves
    , startingSpace = spaceIdFor "General Store"
    , reckoningText = "Spread terror in each neighborhood with a Deep One monster."
    , reckoning = Custom "tyrants-reckoning"
    , setupMap =
        buildMapOf
          [ nb "Innsmouth Village"
          , nb "Innsmouth Shore"
          , nb "Northside"
          , nb "Easttown"
          , nb "Miskatonic University"
          , nb "Southside"
          ]
          -- Innsmouth is its own cluster; the Arkham tiles are the other
          [ StreetDef (nb "Innsmouth Village") SideRight (nb "Innsmouth Shore") Scenic
          , StreetDef (nb "Northside") SideRight (nb "Easttown") Residential
          , StreetDef (nb "Northside") BottomRight (nb "Miskatonic University") Bridge
          , StreetDef (nb "Easttown") BottomLeft (nb "Miskatonic University") Bridge
          , StreetDef (nb "Easttown") BottomRight (nb "Southside") Scenic
          , StreetDef (nb "Miskatonic University") SideRight (nb "Southside") Residential
          ]
          noPieces
            { routes =
                [ RouteDef (nb "Innsmouth Village") BottomLeft CountryRoad
                , RouteDef (nb "Innsmouth Shore") BottomRight FerryTerminal
                , RouteDef (nb "Northside") TopLeft CountryRoad
                , RouteDef (nb "Easttown") TopRight FerryTerminal
                , RouteDef (nb "Southside") BottomRight CountryRoad
                ]
            , mysteries = [MysteryTile (nb "Innsmouth Shore") TopRight "Devil Reef"]
            , -- Innsmouth sits above and right of Arkham, a row of the same honeycomb
              clusters = [ClusterLink (nb "Northside") TopRight (nb "Innsmouth Village")]
            , {- and stands a little clear of it, further along the way it already hangs.
              Both Innsmouth tiles move together, or the street between them would skew. -}
              nudges =
                [ nudge (nb "Innsmouth Village") out lift
                , nudge (nb "Innsmouth Shore") out lift
                ]
            }
    , monsters =
        [ ("altered-beast", 2)
        , ("hulking-thrall", 2)
        , ("prowling-abductor", 2)
        , -- every Deep One monster; setup leaves out the boxes that are not in play
          ("shoreline-brute", 1)
        , ("sea-singer", 1)
        , ("wake-titan", 1)
        , ("hybrid-thug", 2)
        , ("ocean-scion", 2)
        , ("shallows-predator", 2)
        , ("river-skulk", 2)
        , ("entranced-hybrid", 1)
        , ("frenzied-hunter", 1)
        ]
    , startingMonsters =
        [ ("entranced-hybrid", spaceIdFor "Devil Reef")
        , ("prowling-abductor", spaceIdFor "Hibb's Roadhouse")
        ]
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
          [ "Esoteric Order of Dagon"
          , "Falcon Point"
          , "Curiositie Shoppe"
          , "Hibb's Roadhouse"
          , "Orne Library"
          , "South Church"
          ]
    , startingMarkers = []
    , startingBystanders = []
    , eventCards = [CardCode ("tyrants-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , -- the tyrants cards 65, 66, 67 and 73 call up
      setAside = ["archive-74", "archive-75"]
    , codex = [61, 62, 63]
    , anomalySet = Nothing
    , terrorSet = Just "Feeding Frenzy"
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
    (CardCode ("tyrants-event-" <> pad n))
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
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "You are stopped at the front door and scrutinized closely. At the bar, you learn that Innsmouth folk are not allowed in anymore, but you don't have \"the look.\" Gain one clue from your neighborhood. You may spend $1 to get a drink and socialize. If you do, you or an ally may recover two sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 2)]
        )
      ,
        ( "Police Station"
        , "A prisoner lashes out, overpowering two bruised and bloody officers. The only way to save them is to calm down their attacker (influence). If you pass, you see the humanity return to the prisoner's bulbous eyes and he confesses his crimes; gain one clue from your neighborhood. If you fail, the inhuman thing kills the men in front of you; become TAINTED."
        , Test Influence 0 clue tainted
        )
      ,
        ( "Velma's Diner"
        , "Sitting at a table adjacent to you are several people in suits looking through files. You try to sneak a look at what these files contain without drawing attention to yourself (observation). If you pass, you see maps, photographs, and diagrams outlining federal observation of Innsmouth; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      2
      "Easttown"
      ["Hibb's Roadhouse", "Velma's Diner"]
      [
        ( "Hibb's Roadhouse"
        , "Outside, a strange-looking man from Innsmouth is shouting something, but his voice is so hoarse it is difficult to understand (observation). If you pass, you hear him invoke Hydra and Dagon and cover your ears; gain one clue from your neighborhood. If you fail, it just sounds like gurgling; become TAINTED."
        , Test Observation 0 clue tainted
        )
      ,
        ( "Police Station"
        , "A young girl hands you an object and says, \"I don't think my gran should have this anymore.\" Gain one common item. You try to overhear what the child tells the duty officer (observation). If you pass, you hear her say that her gran turned into a monster and wants to eat her; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Velma's Diner"
        , "\"Last slice,\" says Velma indicating a nearly empty pie tray. You may spend $1 for you or an ally to recover two health. If you do, she tells you about the out-of-town customers she's seen, including some from Innsmouth and some from Washington D.C.; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ]
  , event
      3
      "Easttown"
      ["Velma's Diner"]
      [
        ( "Hibb's Roadhouse"
        , "You buy into a friendly game and pass the time talking to the other players, but you don't win your money back. You may spend $1 for you or an ally to recover two sanity. If you do, the others tell you about outsiders from Innsmouth starting fights; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Police Station"
        , "After a blood-chilling howl erupts from the cells, the police scramble to investigate. You take this opportunity to nab something useful from an officer's desk (observation). If you pass, you grab photos and physical evidence from the scene of a slaughter aboard a fishing trawler; gain one clue from your neighborhood and one common item."
        , pass Observation 0 (Seq [clue, commonItem])
        )
      ,
        ( "Velma's Diner"
        , "A member of the Bureau of Investigation offers you a meal. You or an ally may recover two health. While she asks you questions, you try to read her behavior and reactions (observation). If you pass, it becomes clear that she takes the threat of the Deep Ones very seriously; gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ]
  , event
      4
      "Innsmouth Shore"
      ["Falcon Point"]
      [
        ( "Falcon Point"
        , "A group of men in well-tailored suits are walking along the beach. Either test observation to hide behind a rock or take one damage to jump down into a tide pool. If you pass or suffer the damage, you listen to the men discuss plans for a government raid on Innsmouth; gain one clue from your neighborhood."
        , orPay Observation "Take one damage" (CostDamage 1) clue
        )
      ,
        ( "Gilman House"
        , "A trifling work of fiction left in your room proves to be a pleasant diversion. You or an ally may recover two sanity. After you finish it, you go down to the lobby to find more (observation). If you pass, you find a book about Devil Reef; gain one clue from your neighborhood. If you fail, you find a handwritten journal in unreadable script; become TAINTED."
        , Seq [sanity 2, Test Observation 0 clue tainted]
        )
      ,
        ( "Marsh Refinery"
        , "An anxious worker comes out to speak to you. She fears her employer badly and will tell you what you want. Gain one clue from your neighborhood. You offer to keep her safe (influence). If you pass, she repeats a few rumors; spawn one clue. If you fail, she hands you a piece of jewelry and says it is a gift from the sea; become TAINTED."
        , Seq [clue, Test Influence 0 SpawnOneClue tainted]
        )
      ]
  , event
      5
      "Innsmouth Shore"
      ["Falcon Point", "Gilman House"]
      [
        ( "Falcon Point"
        , "A horrific fish-like humanoid drags you toward the water (strength). If you pass, you pull yourself free and the thing leaves its few possessions behind; gain a curio and one clue from your neighborhood. If you fail, you are pulled briefly under the waves and hear the voice of Mother Hydra; become TAINTED."
        , Test Strength 0 (Seq [curioItem, clue]) tainted
        )
      ,
        ( "Gilman House"
        , "Othera Gilman greets another guest and you eavesdrop as she warns the new arrival about the dangers of objects that come from under the waves. Gain one clue from your neighborhood. Later, you think showing this newcomer an object found in the sea would spark a conversation. You may spend a remnant to gain an ally."
        , Seq [clue, mayPay (SpendRemnants 1) ally]
        )
      ,
        ( "Marsh Refinery"
        , "The refinery is overrun by lurching, toad-like figures. You find a place to hide and hope that these monstrosities do not stay long (observation). If you pass, you hear the creatures talk about a kingdom called Y'ha-nthlei; gain a remnant and one clue from your neighborhood. If you fail, you are forced to flee in a mad panic; suffer two horror."
        , Test Observation 0 (Seq [remnants 1, clue]) (horror 2)
        )
      ]
  , event
      6
      "Innsmouth Shore"
      ["Marsh Refinery"]
      [
        ( "Falcon Point"
        , "You approach a dead sailor near a badly wounded Deep One and find gold clutched in his hands. Gain $3. You may suffer one damage to get close enough to the reeking Deep One to examine it. If you do, you get a close look at the creature's physiology; gain one clue from your neighborhood."
        , Seq [money 3, mayPay (CostDamage 1) clue]
        )
      ,
        ( "Gilman House"
        , "Constable Ropes is here looking to make an arrest, but you believe that you can dissuade him (influence). If you pass, the Constable relents and tells you that a boat full of fishermen was found dead at Devil Reef, while his intended prisoner thanks you; gain one clue from your neighborhood and an ally."
        , pass Influence 0 (Seq [clue, ally])
        )
      ,
        ( "Marsh Refinery"
        , "You suspect at least one employee at the refinery can be bribed to sneak you inside (influence). If you pass, you discover a box of fossils and shells from deep beneath the sea; gain one clue from your neighborhood and a remnant. If you fail, the worker you tried to bribe throws you to the ground; suffer one damage."
        , Test Influence 0 (Seq [clue, remnants 1]) (damage 1)
        )
      ]
  , event
      7
      "Innsmouth Shore"
      ["Marsh Refinery"]
      [
        ( "Falcon Point"
        , "An army of creatures gathers in the thunderstorm raging on Devil Reef. You may suffer two damage to weather the storm and get closer to them. If you do, the High Priest of Dagon who commands these horrors leaves his possessions unguarded; gain a curio and one clue from your neighborhood."
        , mayPay (CostDamage 2) (Seq [curioItem, clue])
        )
      ,
        ( "Gilman House"
        , "You pass the evening talking to two people in dark suits, and the conversation somehow reassures you. You or an ally may recover two sanity. They take a keen interest in the oddities you have acquired. You may spend a remnant to convince these federal agents to describe the evidence they've gathered on Innsmouth; gain one clue from your neighborhood."
        , Seq [sanity 2, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Marsh Refinery"
        , "You see workers leaving and the refinery closing early. You sneak inside and find tools bent out of shape. Gain a remnant. You try to spy on the workers still inside (observation). If you pass, you see these workers' bodies have transformed to the point that they can no longer work; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ]
  , event
      8
      "Innsmouth Shore"
      ["Falcon Point", "Marsh Refinery"]
      [
        ( "Falcon Point"
        , "The wreckage of a ship drifts up to the shore, piece by piece. Gain one clue from your neighborhood. A large trunk floats close enough that you can swim out to it (strength). If you pass, you pull the trunk to shore and pry it open; gain one curio. If you fail, you return to shore exhausted; suffer one damage."
        , Seq [clue, Test Strength 0 curioItem (damage 1)]
        )
      ,
        ( "Gilman House"
        , "A young Innsmouth girl has taken an interest in the grisly souvenirs you have collected. You may spend one remnant to give it to the youth. If you do, she gleefully tells you the story of the father and mother in the sea and her joy brings you strange comfort; you or an ally may recover two sanity and you gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Marsh Refinery"
        , "The only light on at night is coming from the manager's office. You sneak up to the window and try to peek inside (observation). If you pass, you eye a large, ugly man kneeling before small idols that represent Father Dagon and Mother Hydra; spawn a clue and gain one clue from your neighborhood."
        , pass Observation 0 (Seq [SpawnOneClue, clue])
        )
      ]
  , event
      9
      "Innsmouth Village"
      ["Innsmouth Jail"]
      [
        ( "Esoteric Order of Dagon"
        , "The High Priest welcomes you to the temple and embraces you (observation). If you pass, you manage to steal his wallet; gain one clue from your neighborhood. If you fail, he managed to sneak a small carving of Hydra in your pocket without your knowledge; become TAINTED."
        , Test Observation 0 clue tainted
        )
      ,
        ( "First National Grocery"
        , "Brian Burnham brings you some groceries before he closes the store. You or an ally may recover two health. You hear him talking to someone inside (observation). If you pass, you hear the other voice croak about Dagon; gain one clue from your neighborhood. If you fail, you hear your own name; become TAINTED."
        , Seq [health 2, Test Observation 0 clue tainted]
        )
      ,
        ( "Innsmouth Jail"
        , "Constable Ropes has left a copy of his latest interrogation on his unattended desk (will). If you pass, you decode a litany to the rulers of the Deep Ones; gain one clue from your neighborhood. If you fail, the Constable's notes about his brutal methods are too much to bear; suffer two horror."
        , Test Will 0 clue (horror 2)
        )
      ]
  , event
      10
      "Innsmouth Village"
      ["Innsmouth Jail"]
      [
        ( "Esoteric Order of Dagon"
        , "A woman rushes out and hands you a package. Gain one curio. The terrified woman demands you help her hide (observation). If you pass, you stay hidden as a creature emerges and lurches down the street; gain one clue from your neighborhood. If you fail, the creature overpowers you both; suffer two damage."
        , Seq [curioItem, Test Observation 0 clue (damage 2)]
        )
      ,
        ( "First National Grocery"
        , "You notice that customers sometimes pay for their groceries with gold coins or strange golden jewelry. Gain one clue from your neighborhood. Brian Burnham reassures you that cash also works. You may spend $1 for groceries. If you do, you or an ally may recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "Innsmouth Jail"
        , "You know that a past prisoner left a message in their cell. The only way you can get a look is to get yourself incarcerated for the night. You may become delayed to find a carving of a secret prayer to Dagon and Hydra among the things they hid behind a loose brick in the cell; gain one clue from your neighborhood and one common item."
        , mayPay CostDelayed (Seq [clue, commonItem])
        )
      ]
  , event
      11
      "Innsmouth Village"
      ["Esoteric Order of Dagon", "First National Grocery"]
      [
        ( "Esoteric Order of Dagon"
        , "You are called to join the High Priest at the altar. He invites you to be initiated in the Order of Dagon. You may suffer two horror to undergo this monstrous ritual. If you do, your mind is flooded with forbidden knowledge; gain one clue from your neighborhood and one spell."
        , mayPay (CostHorror 2) (Seq [clue, spell])
        )
      ,
        ( "First National Grocery"
        , "You are the only customer and Brian Burnham is doing his best to convince you to buy something. You may spend $1 to buy some groceries and find out why so many people are leaving town suddenly. If you do, you or an ally may recover two health and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "Innsmouth Jail"
        , "The prisoner handcuffed next to you tells you about a city beneath the sea. Gain one clue from your neighborhood. He tells you to search his coat pocket. You find a golden brooch and as you touch it, the abyss fills your thoughts (will). If you pass, you keep the brooch; gain a remnant. If you fail, you keep the abyss; become TAINTED."
        , Seq [clue, Test Will 0 (remnants 1) tainted]
        )
      ]
  , event
      12
      "Innsmouth Village"
      ["Esoteric Order of Dagon"]
      [
        ( "Esoteric Order of Dagon"
        , "No one is in the temple, but a sickening voice fills the dark, wet room, calling you to give yourself over to the great mother and father. You may gain a DARK PACT to open your mind to monstrous secrets. If you do, gain one clue from your neighborhood and one spell."
        , mayPay (CostCondition "DARK PACT") (Seq [clue, spell])
        )
      ,
        ( "First National Grocery"
        , "Many of the customers at the grocery have scratches on their faces and arms. You listen to their conversations to determine what has happened to them (observation). If you pass, you hear them discussing relatives who are changing and becoming more dangerous; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Innsmouth Jail"
        , "Constable Ropes is tormenting one of his prisoners, taking one of the inmate's possessions and throwing it to you. Gain one common item. The prisoner doesn't respond, but whispers something to himself you struggle to hear (observation). If you pass, you can discern that he is praying to Mother Hydra; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ]
  , -- two doom icons on the one location, so two doom land there
    event
      13
      "Innsmouth Village"
      ["Esoteric Order of Dagon", "Esoteric Order of Dagon"]
      [
        ( "Esoteric Order of Dagon"
        , "Murals depict a meeting between humans and sea creatures. Gain one clue from your neighborhood. Looking closer, you see strange symbols hidden within the artwork (lore). If you pass, you see their arcane significance; gain a spell. If you fail, you start to see these symbols everywhere; become TAINTED."
        , Seq [clue, Test Lore 0 spell tainted]
        )
      ,
        ( "First National Grocery"
        , "No trucks have been able to reach Innsmouth and there is little for sale right now. You may spend $1 to buy meager supplies. If you do, Brian Burnham tells you about the monsters that roam the streets at night; gain one clue from your neighborhood and you or an ally may recover one health."
        , mayPay (SpendMoney 1) (Seq [clue, health 1])
        )
      ,
        ( "Innsmouth Jail"
        , "One prisoner is quite unlike the usual village rubes and you try to gain her trust (influence). If you pass, she confides in you that she is a federal agent, wrongly imprisoned, and she gives you her notes about what is really happening in Innsmouth and something to help you; gain one clue from your neighborhood and one common item."
        , pass Influence 0 (Seq [clue, commonItem])
        )
      ]
  , event
      14
      "Miskatonic University"
      ["Science Building"]
      [
        ( "Observatory"
        , "The observatory is closed, but you find the lock and chain that secure the doors at night have been torn apart. Gain a remnant. You search the dark room for any sign of the intruder (observation). If you pass, you smell the ocean and notice a large wet footprint on the floor; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "Orne Library"
        , "The library is about to close but you are completely absorbed in your research. You might not see what is happening until it is too late (observation). If you pass, you catch up to the librarian as she is locking the door and ask her about Innsmouth; gain one clue from your neighborhood. If you fail, you are locked in overnight; become delayed."
        , Test Observation 0 clue delayed
        )
      ,
        ( "Science Building"
        , "One of the scientists quietly pulls you aside and covertly hands you some cash. Gain $3. She wants your help searching the steam tunnels beneath the building to recover an escaped test subject (observation). If you pass, you find the creature, a bizarre hybrid of fish and human; gain one clue from your neighborhood."
        , Seq [money 3, pass Observation 0 clue]
        )
      ]
  , event
      15
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "A strange woman with pale, exaggerated features asks the astronomers a lot of strange questions. You struggle to see the significance of her inquiries (lore). If you pass, you make an illustration of the celestial bodies she wants to know about; gain one clue from your neighborhood and one remnant."
        , pass Lore 0 (Seq [clue, remnants 1])
        )
      ,
        ( "Orne Library"
        , "Someone has written an arcane chant in the margins of this collection of oceanic maps. Gain a spell. You also see several series of numbers that have no apparent meaning (lore). If you pass, you see that these numbers refer to the latitudes and longitudes of various places beneath the seas; gain one clue from your neighborhood."
        , Seq [spell, pass Lore 0 clue]
        )
      ,
        ( "Science Building"
        , "Several scientists are loudly reviewing research and discussing hypotheses. As you listen, you try to discern some useful information (observation). If you pass, you hear that some outside source accelerates the change into a Deep One; gain one clue from your neighborhood. If you fail, you hear, \"Praise to Dagon and Hydra;\" suffer one horror."
        , Test Observation 0 clue (horror 1)
        )
      ]
  , event
      16
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "A visitor to the observatory left behind a notebook that lists addresses where worshipers of Dagon meet in secret. Remove one doom from any space. You may become delayed to take time to decipher more of the notebook. If you do, you learn prayers to Dagon; gain one clue from your neighborhood."
        , Seq [RemoveDoomFrom AnySpaceWithDoom (N 1), mayPay CostDelayed clue]
        )
      ,
        ( "Orne Library"
        , "The library's restricted section includes a handwritten collection of legends and anecdotes from a historic Innsmouth family. Some of the text is written in a strange code (lore). If you pass, gain a spell and one clue from your neighborhood. If you fail, the words convey no meaning but make you feel afraid and nauseated; suffer two horror."
        , Test Lore 0 (Seq [spell, clue]) (horror 2)
        )
      ,
        ( "Science Building"
        , "The researchers believe they have identified what has been transforming the citizens of Innsmouth into monstrous sea creatures, and they will pay a volunteer for further experiments. You may become TAINTED to let them run some tests on you. If you do, the results are highly informative; gain one clue from your neighborhood and $3."
        , mayPay (CostCondition "TAINTED") (Seq [clue, money 3])
        )
      ]
  , event
      17
      "Miskatonic University"
      ["Science Building", "Science Building"]
      [
        ( "Observatory"
        , "Looking through the telescope, you are absolutely entranced by the constellations. You barely notice what is happening around you (observation). If you pass, you see two strangers staring at you; gain one clue from your neighborhood. If you fail, something wet brushes you; become TAINTED."
        , Test Observation 0 clue tainted
        )
      ,
        ( "Orne Library"
        , "According to the card catalog, the library has a definitive volume of myths and legends from the sea, but it is not on the shelf where it belongs. You may become delayed to search carefully for this misplaced book. If you do, you find it wedged between a desk and the wall; gain one clue from your neighborhood and one spell."
        , mayPay CostDelayed (Seq [clue, spell])
        )
      ,
        ( "Science Building"
        , "The professors seek any photographs or drawings of corrupted people from Innsmouth. You may spend one remnant to provide these experts with examples of their affliction to determine how it progresses. If you do, they share their funding and the information they have collected so far; gain one clue from your neighborhood and $3."
        , mayPay (SpendRemnants 1) (Seq [clue, money 3])
        )
      ]
  , event
      18
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "Doyle Jefferies received an anonymous threat not to publish an article about the Order of Dagon (observation). If you pass, you recognize the postmark and he rewards your sleuthing; gain one clue from your neighborhood and $3. If you fail, slick oil transfers from the paper to your hand; become TAINTED."
        , Test Observation 0 (Seq [clue, money 3]) tainted
        )
      ,
        ( "Curiositie Shoppe"
        , "Antique nautical maps feature strange creatures lurking at various points, including just off the Massachusetts coast. Gain one clue from your neighborhood. As you study the maps, Oliver Thomas asks if you intend to buy anything; you may buy any number of curios from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "Just as a train is departing, one of its passengers turns to look out the window and you see the face of a hideous, inhuman creature (will). If you pass, you describe the thing to the ticketing agent who tells you its name and its destination; gain one clue from your neighborhood. If you fail, you see that face everywhere; become TAINTED."
        , Test Will 0 clue tainted
        )
      ]
  , event
      19
      "Northside"
      ["Curiositie Shoppe", "Train Station"]
      [
        ( "Arkham Advertiser"
        , "The paper is offering cash for genuine proof of the horrors that stalk the streets of Arkham. People have come out in droves to share what they have seen. Gain one clue from your neighborhood. You may spend one remnant to provide a scientific study of these creatures. If you do, gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "Oliver Thomas is loudly complaining about \"those degenerate Innsmouth scum!\" To help you overcome the threat of Deep Ones, he offers you a discount. You may buy one curio from the display for half price (rounded up). If you do, he regales you with everything he hates about Innsmouth; gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Curio") HalfPrice (Just 1) clue
        )
      ,
        ( "Train Station"
        , "You spot a grotesque-looking man sneaking into a baggage car. You try to convince the conductor to hold the train and remove the intruder (influence). If you pass, the thief is caught and his stolen goods are left behind, forgotten and unclaimed; gain one clue from your neighborhood and one curio."
        , pass Influence 0 (Seq [clue, curioItem])
        )
      ]
  , event
      20
      "Northside"
      ["Arkham Advertiser"]
      [
        ( "Arkham Advertiser"
        , "Minnie Klein is putting together a story about the violence in Arkham, but refuses to let slip any details. You may spend one remnant to share what you know of Deep Ones. If you do, she shows you all the notes she has taken about the attacks; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ,
        ( "Curiositie Shoppe"
        , "An old sailor's trunk sits empty in one corner. Oliver Thomas informs you that everything that was in there has been sold already. Still, you examine the trunk carefully (observation). If you pass, you find a false bottom in the chest, containing baubles and a map to someplace called Y'ha-nthlei; gain one clue from your neighborhood and one curio."
        , pass Observation 0 (Seq [clue, curioItem])
        )
      ,
        ( "Train Station"
        , "A stranger steps off the train and looks around, but no one is here waiting. You introduce yourself. Gain an ally. As you walk, you think someone is following your new friend (observation). If you pass, you catch a glimpse of the toad-like figure; gain one clue from your neighborhood. If you fail, you are struck from behind; suffer one damage."
        , Seq [ally, Test Observation 0 clue (damage 1)]
        )
      ]
  , event
      21
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "Many of the Society's records of Innsmouth have been damaged. They are eager to rebuild their collection and anything you offer would be greatly appreciated. You may spend one remnant to help. If you do, they discuss the small town's darker history with you; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ,
        ( "Ma's Boarding House"
        , "Ma Mathison wants you to meet her lodger over dinner. You or an ally may recover two health. You wait, but he doesn't emerge from his room (observation). If you pass, you hear a struggle and rush to save the man from a Deep One; gain one clue from your neighborhood. If you fail, you find his corpse after dinner; suffer one horror."
        , Seq [health 2, Test Observation 0 clue (horror 1)]
        )
      ,
        ( "South Church"
        , "Church records can be used to trace family lines back to the earliest days of Arkham. You search for the ancestors of people you know to have acquired the distinctive Innsmouth look (observation). If you pass, you identify a number of family lines that seem to have these hereditary traits; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      22
      "Southside"
      ["Historical Society", "Historical Society"]
      [
        ( "Historical Society"
        , "A historian is giving a lecture on the importance of Obed Marsh to Innsmouth's history. Gain one clue from your neighborhood. Afterwards, you speak to another attendee who seems guarded, but might open up if you share proof of what you have learned. You may spend one remnant to gain an ally."
        , Seq [clue, mayPay (SpendRemnants 1) ally]
        )
      ,
        ( "Ma's Boarding House"
        , "One of Ma's boarders is a well-dressed man with a briefcase. You may become delayed to wait for him to let it out of his sight so you can see what it contains. If you do, an opportunity presents itself and you discover naval plans for an assault off the coast of Innsmouth; gain one clue from your neighborhood."
        , mayPay CostDelayed clue
        )
      ,
        ( "South Church"
        , "Someone has defaced the altar, painting symbols of Dagon on the walls. You or an ally may recover two sanity as you help restore the sanctity of the church. You may become TAINTED to record the profane icons. If you do, you recall seeing the symbol on jewelry and lapel pins; gain one clue from your neighborhood."
        , Seq [sanity 2, mayPay (CostCondition "TAINTED") clue]
        )
      ]
  , event
      23
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "A large man with pale skin and bulging eyes is in the Society's archives, threatening to destroy the books (influence). If you pass, you and a stranger stop him from damaging any records; gain an ally and one clue from your neighborhood. If you fail, the fire he starts rages out of control; suffer one damage."
        , Test Influence 0 (Seq [ally, clue]) (damage 1)
        )
      ,
        ( "Ma's Boarding House"
        , "Ma was expecting all her rooms to be full, but guests have just been vanishing without a trace. She tells you what she knows about these absent visitors. Gain one clue from your neighborhood. Ma asks you if you want to buy any of the extra food she got for the weekend. You may spend $2 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 2) (health 3)]
        )
      ,
        ( "South Church"
        , "The Deep Ones have been wreaking havoc on innocent families, and you may donate money to the victims of this violence. You may spend $1 for you or an ally to recover two sanity. If you do, each of these families has their own tale about the monsters that attacked them; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ]
  , event
      24
      "Southside"
      ["South Church"]
      [
        ( "Historical Society"
        , "A box of artifacts is sitting out and no one is looking, so you borrow one of the historic treasures. Gain one curio. You also swipe some of the papers (observation). If you pass, you read a letter describing Obed Marsh; gain one clue from your neighborhood. If you fail, become TAINTED."
        , Seq [curioItem, Test Observation 0 clue tainted]
        )
      ,
        ( "Ma's Boarding House"
        , "One of the guests has certain facial features that could be seen as the \"Innsmouth look,\" but then again, she could be perfectly normal (observation). If you pass, you determine the woman is an ordinary doctor treating older clients in the area; gain one clue from your neighborhood and you or an ally may recover two health."
        , pass Observation 0 (Seq [clue, health 2])
        )
      ,
        ( "South Church"
        , "You may spend $2 to provide funding for Father Michael's outreach mission to Innsmouth. If you do, he tells you everything he has learned about the seaside village's unnatural lineages and the pagan rituals that regularly occur there; gain one clue from your neighborhood and you or an ally may recover two sanity."
        , mayPay (SpendMoney 2) (Seq [clue, sanity 2])
        )
      ]
  ]
