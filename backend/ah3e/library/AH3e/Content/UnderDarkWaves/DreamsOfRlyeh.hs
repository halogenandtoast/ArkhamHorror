-- | Dreams of R'lyeh: a song from a sunken city, heard only in sleep.
module AH3e.Content.UnderDarkWaves.DreamsOfRlyeh (code, scenario, cards) where

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
code = "dreams-of-rlyeh"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Dreams of R'lyeh"
    , expansion = UnderDarkWaves
    , startingSpace = spaceIdFor "St. Mary's Hospital"
    , reckoningText = "Each investigator places one doom in their space unless they suffer one horror."
    , reckoning =
        ForInvestigators
          EveryInvestigator
          ( Choose
              [ ("Place one doom in your space", PlaceDoomAt YourSpace (N 1))
              , ("Suffer one horror", SufferHorror (N 1))
              ]
          )
    , setupMap =
        {- Only Arkham is set out. Whichever of cards 117-120 the investigation
        turns up brings its own town's two tiles, laid above Arkham for Innsmouth
        and below it for Kingsport. -}
        buildMapWith
          [nb "Miskatonic University", nb "Rivertown", nb "Uptown", nb "Southside"]
          [ StreetDef (nb "Miskatonic University") SideRight (nb "Rivertown") Residential
          , StreetDef (nb "Miskatonic University") BottomRight (nb "Uptown") Residential
          , StreetDef (nb "Rivertown") BottomRight (nb "Southside") Residential
          , StreetDef (nb "Uptown") SideRight (nb "Southside") Scenic
          ]
          [ RouteDef (nb "Miskatonic University") SideLeft CountryRoad
          , RouteDef (nb "Uptown") BottomLeft TrainPlatform
          , RouteDef (nb "Southside") BottomRight FerryTerminal
          ]
          []
    , monsters =
        [ ("high-priest", 1)
        , ("occult-ritualist", 2)
        , ("sea-singer", 1)
        , ("shallows-predator", 2)
        , ("shoreline-brute", 1)
        , -- every dreaming monster
          ("accursed-somnambulist", 2)
        , ("enraged-dreamer", 2)
        , ("entranced-hybrid", 1)
        , ("terrified-wanderer", 2)
        , -- and every star spawn
          ("rlyeh-guardian", 1)
        , ("cantor-of-rlyeh", 1)
        ]
    , startingMonsters =
        [ ("accursed-somnambulist", spaceIdFor "Graveyard")
        , ("enraged-dreamer", spaceIdFor "Ye Olde Magick Shoppe")
        ]
    , mythosCup =
        [ (SpreadDoomToken, 3)
        , (SpawnMonsterToken, 2)
        , (SpawnClueToken, 2)
        , (ReadHeadlineToken, 2)
        , (GateBurstToken, 1)
        , (ReckoningToken, 1)
        , (BlankToken, 1)
        ]
    , startingDoom =
        map spaceIdFor ["Orne Library", "Black Cave", "Hangman's Hill", "Historical Society"]
    , startingMarkers = []
    , startingBystanders = []
    , {- Only Arkham's sixteen events start in the deck; cards 117-120 shuffle in
      the eight belonging to whichever town they add. -}
      eventCards = [CardCode ("rlyeh-event-" <> pad n) | n <- [17 .. 32 :: Int]]
    , setAside =
        [CardCode ("rlyeh-event-" <> pad n) | n <- [1 .. 16 :: Int]]
          -- the epic monsters cards 113 and 116 call up, and 109's Cthulhu
          <> ["echoes-39", "echoes-40", "archive-75"]
    , codex = [1, 106, 107]
    , anomalySet = Nothing
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = fromBox UnderDarkWaves events

-- | Zero padded, so the card codes sort the way the cards are numbered.
pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

-- | "Gain a DARK PACT unless you ...", the choice the headlines print too.
darkPact :: Effect
darkPact = Pay (CostCondition "DARK PACT") NoEffect

event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("rlyeh-event-" <> pad n))
    ("Event " <> tshow n <> "/32")
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
      ["Hall School"]
      [
        ( "Congregational Hospital"
        , "Several people in the waiting room suffer the same symptoms: pale skin, rasping breath, large eyes, and a foul odor. \"Nothing to be done for them,\" says the doctor, \"but I may be able to help you.\" Gain one clue from your neighborhood. You may spend $1 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "Hall School"
        , "In one classroom you see a collection of student artwork. Each drawing depicts nightmarish underwater landscapes. Something in the pictures seems out of place (observation). If you pass, you recognize familiar Kingsport landmarks in the illustrations; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Neil's Curiosity Shop"
        , "You find a sketch book for sale, left behind by someone from the Artists Colony on the South Shore. You may spend $2 to buy it and gain one remnant. If you do, the illustrations inside depict strange, humanoid fish-creatures negotiating a contract with desperate sailors; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [remnants 1, clue])
        )
      ]
  , event
      2
      "Central Kingsport"
      ["Hall School", "Hall School"]
      [
        ( "Congregational Hospital"
        , "A patient left her journal behind. Gain one remnant. As you read, you fall asleep and dream of a vast undersea kingdom, and search desperately to find an exit (observation). If you pass, you wake with vivid memories of what you saw; gain one clue from your neighborhood. If you fail, become TAINTED."
        , Seq [remnants 1, Test Observation 0 clue tainted]
        )
      ,
        ( "Hall School"
        , "Parents and other concerned parties have gathered at the school. You listen to all of them share their experiences. Gain one clue from your neighborhood. You think someone here may be of help to you, but it may take some convincing (influence). If you pass, your argument proves persuasive; gain one ally."
        , Seq [clue, pass Influence 0 ally]
        )
      ,
        ( "Neil's Curiosity Shop"
        , "Neil's inventory seems unusually sparse, but you do have a few carved shells that the pawn broker might want. You may spend one remnant to help restock the shelves. If you do, Neil trades his knowledge and an old oddity for the new merchandise; gain one clue from your neighborhood and one curio."
        , mayPay (SpendRemnants 1) (Seq [clue, curioItem])
        )
      ]
  , event
      3
      "Central Kingsport"
      ["Congregational Hospital", "Hall School"]
      [
        ( "Congregational Hospital"
        , "The hospital is filled with exhausted citizens, afraid to sleep for fear of the dreams that await. You may spend $2 for you or an ally to recover two health. If you do, the doctor confides in you about what has been happening in the community; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [health 2, clue])
        )
      ,
        ( "Hall School"
        , "An intruder with strange, wide, gaping eyes tries to steal a paper written by a former student. You drive them off and read Asenath Waite's work. Gain one spell. You ask around to determine the thief's identity (observation). If you pass, you learn that they are a criminal from Innsmouth; gain one clue from your neighborhood. If you fail, become TAINTED."
        , Seq [spell, Test Observation 0 clue tainted]
        )
      ,
        ( "Neil's Curiosity Shop"
        , "Neil is excited to show you an antique ship's log that mentions a race of underwater monsters. Gain one clue from your neighborhood. Neil says that you are free to keep the log, but you have to trade for anything else you want. You may spend one remnant to gain one curio."
        , Seq [clue, mayPay (SpendRemnants 1) curioItem]
        )
      ]
  , event
      4
      "Central Kingsport"
      ["Congregational Hospital"]
      [
        ( "Congregational Hospital"
        , "You try to sneak down into the hospital's sub-basement, but find the access door locked. You may spend $1 to bribe a janitor to open it. If you do, you find the tunnels half-flooded with seawater and inhabited by strange, alien-looking fish; gain one clue from your neighborhood and one remnant."
        , mayPay (SpendMoney 1) (Seq [clue, remnants 1])
        )
      ,
        ( "Hall School"
        , "Victoria Bryant pulls you aside for a quiet conversation. \"Someone has come to the school asking about Asenath Waite, a former student. I think this person might be quite troubled.\" You try to assuage the dean's fears (influence). If you pass, she introduces you to someone with vital information; gain an ally and one clue from your neighborhood."
        , pass Influence 0 (Seq [ally, clue])
        )
      ,
        ( "Neil's Curiosity Shop"
        , "A sailor offers you photos of his family in Innsmouth after Neil refuses to buy them. Gain one remnant. He also offers a scrapbook outlining his family tree. He says, \"I think your surname may be in here.\" You may become TAINTED to gain one clue from your neighborhood."
        , Seq [remnants 1, May "Become TAINTED to read the scrapbook" (Seq [tainted, clue])]
        )
      ]
  , event
      5
      "Innsmouth Shore"
      ["Falcon Point"]
      [
        ( "Falcon Point"
        , "An Innsmouth man staggers across the beach, struggling to breathe. His features are so malformed that he looks like a monstrous fish. Gain one clue from your neighborhood. He is holding an object, but as you approach, his claws rake at your face. You may suffer two damage to gain one curio."
        , Seq [clue, mayPay (CostDamage 2) curioItem]
        )
      ,
        ( "Gilman House"
        , "At breakfast you can see that one of the other guests did not sleep well and is upset about something. She dismisses her mood as \"a bad dream,\" but you think she might open up about a nightmare if you can prove that something unnatural is happening in Innsmouth. You may spend one remnant to gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ,
        ( "Marsh Refinery"
        , "The refinery has been running day and night. You try to sneak close enough to see who is working there for these extra hours (observation). If you pass, you make sketches of the hulking creatures that work during the night; gain one clue from your neighborhood and one remnant."
        , pass Observation 0 (Seq [clue, remnants 1])
        )
      ]
  , event
      6
      "Innsmouth Shore"
      ["Falcon Point", "Marsh Refinery"]
      [
        ( "Falcon Point"
        , "Walking across the shore you feel a sense of déjà vu; you have dreamt this moment before. You stop abruptly and unearth a small piece of gold. Gain $3. You carefully examine the ornately carved treasure (observation). If you pass, gain one clue from your neighborhood."
        , Seq [money 3, pass Observation 0 clue]
        )
      ,
        ( "Gilman House"
        , "Sitting outside, you listen to the soothing sound of the ocean. You or an ally may recover two sanity. Joe Sargent rests nearby, lulled into a stupor by the waves. In this state, he may answer your questions (influence). If you pass, he tells you about Devil Reef; gain one clue from your neighborhood. If you fail, you fall into a trance; become TAINTED."
        , Seq [sanity 2, Test Influence 0 clue tainted]
        )
      ,
        ( "Marsh Refinery"
        , "Two workers have stepped outside for a lunch break. You hide nearby and try to eavesdrop on their conversation (observation). If you pass, you hear them discuss dreams they've had of swimming with their elders in the great city of Y'ha-nthlei; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      7
      "Innsmouth Shore"
      ["Marsh Refinery"]
      [
        ( "Falcon Point"
        , "You wake after sleepwalking far out into the water, and scramble to return to shore (strength). If you pass, you find several coins with a strange symbol; gain $3 and one clue from your neighborhood. If you fail, you feel something calling you from the depths before you make it back to land; become TAINTED."
        , Test Strength 0 (Seq [money 3, clue]) tainted
        )
      ,
        ( "Gilman House"
        , "The lobby is littered with sketches of a vast shadowy form in an underwater landscape. Gain one clue from your neighborhood. The source of these illustrations has been troubled by terrifying nightmares. You may spend one remnant to show them proof that you have also experienced these horrors and gain one ally."
        , Seq [clue, mayPay (SpendRemnants 1) ally]
        )
      ,
        ( "Marsh Refinery"
        , "You stop a truck leaving the refinery and demand to see the driver's papers. The invoice includes cryptic notes about the tides. Gain one remnant. You try to bribe her for details about the refinery (influence). If you pass, gain one clue from your neighborhood. If you fail, she is likely to tell her boss about you; become TAINTED."
        , Seq [remnants 1, Test Influence 0 clue tainted]
        )
      ]
  , event
      8
      "Innsmouth Shore"
      ["Falcon Point", "Falcon Point"]
      [
        ( "Falcon Point"
        , "The thought of floating through the city beneath the waves fills you with joy, but you know that the path would be painful. Gain a DARK PACT unless you suffer two damage. Either way, your mind is flooded with knowledge of the Deep Ones' terrible legacy; gain one clue from your neighborhood."
        , Seq [Choose [("Suffer two damage", damage 2), ("Gain a DARK PACT", darkPact)], clue]
        )
      ,
        ( "Gilman House"
        , "Something seems peculiar about your room and you examine its details closely (observation). If you pass, you insist on a different room when you discover that your door can only be locked from the outside; gain one clue from your neighborhood, then you or an ally may recover two sanity when you rest in your new room."
        , pass Observation 0 (Seq [clue, sanity 2])
        )
      ,
        ( "Marsh Refinery"
        , "In the basement, you find a photograph of toad-like creatures. Gain one clue from your neighborhood. While you study the image, a fire breaks out upstairs (observation). If you pass, you escape the flames and find writing on the back of the picture; spawn one clue. If you fail, you and the photo are both badly burned; suffer two damage."
        , Seq [clue, Test Observation 0 SpawnOneClue (damage 2)]
        )
      ]
  , event
      9
      "Innsmouth Village"
      ["Esoteric Order of Dagon"]
      [
        ( "Esoteric Order of Dagon"
        , "The high priest is reading aloud from a large gilded book. As you listen, you learn the history of the sunken city of R'lyeh, and the great dreamer at its center. The more of the story you hear, the more terrified you become. Suffer two horror and gain one clue from your neighborhood."
        , Seq [horror 2, clue]
        )
      ,
        ( "First National Grocery"
        , "Othera Gilman has come from Innsmouth to sell a few of her paintings, and tells you that she has been inspired lately by dreams of flooded castles. You may spend $1 to share a meal with her and ask about the dreams. If you do, gain one clue from your neighborhood and you or an ally may recover two health."
        , mayPay (SpendMoney 1) (Seq [clue, health 2])
        )
      ,
        ( "Innsmouth Jail"
        , "All the cell doors are open and the inmates are free, shouting threats and boasts about secrets they know. Gain one clue from your neighborhood. You ask one of the men to help you (influence). If you pass, he laughs and throws you something you might find useful; gain one common item. If you fail, he locks you in a cell; become delayed."
        , Seq [clue, Test Influence 0 commonItem delayed]
        )
      ]
  , event
      10
      "Innsmouth Village"
      ["Esoteric Order of Dagon", "Esoteric Order of Dagon"]
      [
        ( "Esoteric Order of Dagon"
        , "Inside the High Priest's office you find a large collection of obscure books (lore). If you pass, you know exactly which texts to examine; gain one clue from your neighborhood and one spell. If you fail, you choose a book at random and read the abominable words within; become TAINTED."
        , Test Lore 0 (Seq [clue, spell]) tainted
        )
      ,
        ( "First National Grocery"
        , "The apple Brian Burnham shares tastes great. You or an ally may recover two health. You hesitate before eating the fish sandwich he offers (observation). If you pass, you decline when he says the fish was caught off Devil Reef; gain one clue from your neighborhood. If you fail, you eat the sandwich; become TAINTED."
        , Seq [health 2, Test Observation 0 clue tainted]
        )
      ,
        ( "Innsmouth Jail"
        , "Constable Ropes is in the back with a prisoner. The \"evidence\" he is using to frame this man is on the reception desk. Gain a common item. The cries of agony and despair are unbearable (will). If you pass, you listen long enough to hear Ropes describe several murders at Devil Reef; gain one clue from your neighborhood."
        , Seq [commonItem, pass Will 0 clue]
        )
      ]
  , event
      11
      "Innsmouth Village"
      ["Esoteric Order of Dagon", "Innsmouth Jail"]
      [
        ( "Esoteric Order of Dagon"
        , "Inside the dark temple, you find abandoned possessions and ripped clothing strewn across the floor. Gain one curio. You consult the large book on the altar (lore). If you pass, you shudder at the words \"transformation\" and \"ascendancy;\" gain one clue from your neighborhood."
        , Seq [curioItem, pass Lore 0 clue]
        )
      ,
        ( "First National Grocery"
        , "There's a lot of gossip in the grocery today about older relatives growing restless and \"hearing the call.\" Gain one clue from your neighborhood. One elderly woman did not come in to pick up her usual order. You may spend $1 to buy her order for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "Innsmouth Jail"
        , "Late at night, the prisoners begin screaming. You try to discern words among the shouts to learn what they are seeing in their nightmares (observation). If you pass, you believe they are dreaming of being torn apart by frenzied sea creatures and record the names that they cry out; gain one clue from your neighborhood and one remnant."
        , pass Observation 0 (Seq [clue, remnants 1])
        )
      ]
  , event
      12
      "Innsmouth Village"
      ["Innsmouth Jail"]
      [
        ( "Esoteric Order of Dagon"
        , "From the temple, you hear rhythmic chanting that embeds itself in your mind. Gain one spell. The lights inside grow unbearably bright and you see visions of monstrous sea creatures swimming through the streets of Innsmouth. Suffer one horror and gain one clue from your neighborhood."
        , Seq [spell, horror 1, clue]
        )
      ,
        ( "First National Grocery"
        , "Brian Burnham is writing down a large order for the Order of Dagon. Curious as to what they order and how often, either test observation to steal the notebook, or spend $1 to bribe Burnham. If you pass or spend the money, you determine how large the Order really is; gain one clue from your neighborhood."
        , orPay Observation "Spend $1" (SpendMoney 1) clue
        )
      ,
        ( "Innsmouth Jail"
        , "One of the prisoners is having an episode, thrashing violently while his body twists and changes (will). If you pass, you suspect everyone in Innsmouth might undergo this loss of humanity eventually; gain one clue from your neighborhood. If you fail, whatever is happening begins to affect you; become TAINTED."
        , Test Will 0 clue tainted
        )
      ]
  , event
      13
      "Kingsport Harbor"
      ["North Point Lighthouse", "North Point Lighthouse"]
      [
        ( "North Point Lighthouse"
        , "Among the detritus and old wrecks littering the shoreline, you find a carving of an alien creature returning to the waves. Gain one clue from your neighborhood. You examine the icon carefully (observation). If you pass, you find an elder sign hidden in its eye; remove one doom from any space."
        , Seq [clue, pass Observation 0 (RemoveDoomFrom AnySpace (N 1))]
        )
      ,
        ( "The Rope and Anchor"
        , "The painting above one of the booths depicts an ancient stone building next to other structures you recognize. You've seen some of the symbols on the building before and ask Jonas if you can buy the painting from him to find the other buildings in the image. You may spend $2 to gain one clue from your neighborhood and spawn one clue."
        , mayPay (SpendMoney 2) (Seq [clue, SpawnOneClue])
        )
      ,
        ( "St. Erasmus's Home"
        , "You'd like to look through the records, but you will need to make nice with the caretaker (influence). If you pass, you learn that a number of sailors from Innsmouth have disappeared during the night, leaving their personal effects behind; gain one clue from your neighborhood and one common item."
        , pass Influence 0 (Seq [clue, commonItem])
        )
      ]
  , event
      14
      "Kingsport Harbor"
      ["North Point Lighthouse", "St. Erasmus's Home"]
      [
        ( "North Point Lighthouse"
        , "You sleep on the beach and dream of the White Ship. Remove one doom from any space. The crew teaches you the ways of the Deep Ones (lore). If you pass, you acquire valuable insight; gain one clue from your neighborhood. If you fail, you feel befouled by the knowledge; become TAINTED."
        , Seq [RemoveDoomFrom AnySpace (N 1), Test Lore 0 clue tainted]
        )
      ,
        ( "The Rope and Anchor"
        , "Tonight's patrons are terrified. They claim a sea witch has come to Kingsport. Gain one clue from your neighborhood. You suspect you could improve everybody's spirits, including your own, by buying a round of drinks. You may spend $1 for you or an ally to gain two sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 2)]
        )
      ,
        ( "St. Erasmus's Home"
        , "Granny Orne seems troubled but is hesitant to confide in you (influence). If you pass, you convince her to share her troubles and she describes the watery nightmares that have plagued her and the sailors who reside here; gain one clue from your neighborhood."
        , pass Influence 0 clue
        )
      ]
  , event
      15
      "Kingsport Harbor"
      ["St. Erasmus's Home"]
      [
        ( "North Point Lighthouse"
        , "The lighthouse is filled with photos of ships and sailors from throughout the years. You examine each of these in turn (observation). If you pass, you notice that many inhabitants of Innsmouth share facial features that resemble fish; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "The Rope and Anchor"
        , "The pub hosts a wake for a sailor who died under strange circumstances. Spawn one clue. As the assembled guests sing an unfamiliar chantey, you try to discern the lyrics (observation). If you pass, it describes a town plagued by evil exactly like that which faces Kingsport; gain one clue from your neighborhood. If you fail, the song haunts you; become TAINTED."
        , Seq [SpawnOneClue, Test Observation 0 clue tainted]
        )
      ,
        ( "St. Erasmus's Home"
        , "One sailor tells you of threats to humanity coming from the deep. Gain one clue from your neighborhood. You ask him about his background (influence). If you pass, he gives you a souvenir from his home in Innsmouth; gain one common item. If you fail, he asks if you hear the call; become TAINTED."
        , Seq [clue, Test Influence 0 commonItem tainted]
        )
      ]
  , event
      16
      "Kingsport Harbor"
      ["North Point Lighthouse"]
      [
        ( "North Point Lighthouse"
        , "Basil Elton describes a dream he had the night before. The images he describes are bizarre and difficult to interpret (lore). If you pass, the visions he describes coalesce into a clear arcane concept; gain one clue from your neighborhood and one spell."
        , pass Lore 0 (Seq [clue, spell])
        )
      ,
        ( "The Rope and Anchor"
        , "You eavesdrop on a sailor at the next table, gurgling about a city beneath the sea. You try to catch every word while you relax with an illicit drink (observation). If you pass, you hear a description of a monstrous ritual in this region; gain one clue from your neighborhood and you or an ally may recover two sanity."
        , pass Observation 0 (Seq [clue, sanity 2])
        )
      ,
        ( "St. Erasmus's Home"
        , "A particularly old sailor waves you over to him and hands you a strangely shaped piece of gold. Gain $3. He doesn't respond to questions, but gestures toward his trunk, which you search thoroughly (observation). If you pass, you find drawings of monstrous sea life; gain one clue from your neighborhood."
        , Seq [money 3, pass Observation 0 clue]
        )
      ]
  , event
      17
      "Miskatonic University"
      ["Orne Library", "Science Building"]
      [
        ( "Observatory"
        , "Each of the students waiting to use the telescope has a hand-drawn version of the night sky, depicting a specific alignment of stars. Gain one remnant. You listen to their conversations (observation). If you pass, you hear they all saw this view of the stars in a dream; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "Orne Library"
        , "You search the shelves for books about nineteenth century shipping routes across the Atlantic Ocean (observation). If you pass, you discover a history of goods coming and going through Innsmouth and you note the list of people who recently checked out this book; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Science Building"
        , "When you enter the laboratory, the researcher asks what you have that is reacting to her equipment. You may spend one remnant to give her the resonant object. If you do, she explains that they have been measuring a signal that is affecting the subconscious and certain specific objects; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ]
  , event
      18
      "Miskatonic University"
      ["Science Building"]
      [
        ( "Observatory"
        , "One of the university's astronomers is singing an eerie song as he stares at the stars. The lyrics to the song are in a strange, ancient language (lore). If you pass, you translate the words and listen to the song about a golden city calling all of its lost children home; gain one clue from your neighborhood."
        , pass Lore 0 clue
        )
      ,
        ( "Orne Library"
        , "You fall asleep reading and dream of a flood that sweeps over Arkham, washing away all of its secrets. Gain one clue from your neighborhood. In your dream, horrific monsters live in the city after the flood (will). If you pass, you witness these monsters performing strange rituals; gain one spell. If you fail, you wake up in a panic; suffer two horror."
        , Seq [clue, Test Will 0 spell (horror 2)]
        )
      ,
        ( "Science Building"
        , "A large radio crackles in an empty room. The static is interrupted by occasional bursts of sounds coming from the radio. Gain one clue from your neighborhood. You search the papers nearby for some instructions on how to use the receiver (observation). If you fail, you touch the radio and receive an electric shock; suffer one damage."
        , Seq [clue, Test Observation 0 NoEffect (damage 1)]
        )
      ]
  , event
      19
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "During a lecture about ancient astronomers, the speaker displays an Oceanic statue (observation). If you pass, you retain the lecture and make an accurate sketch; gain one clue from your neighborhood and one remnant. If you fail, the statue in your drawing looks strangely malignant; become TAINTED."
        , Test Observation 0 (Seq [clue, remnants 1]) tainted
        )
      ,
        ( "Orne Library"
        , "You find a children's book about a lonely princess in a beautiful undersea kingdom that called to her friends on dry land to join her. You think some ritual is threaded through the narrative (lore). If you pass, you recognize key hypnotic phrases in the text; gain one clue from your neighborhood. If you fail, you love this story; become TAINTED."
        , Test Lore 0 clue tainted
        )
      ,
        ( "Science Building"
        , "The researchers compensate you well and share details from their sleep study. Gain one clue from your neighborhood and $3. They put you through a rapid battery of tests, and it is hard to keep track of what they are doing (observation). If you fail, you notice too late that they have injected you with the wrong compound; suffer one damage."
        , Seq [clue, money 3, Test Observation 0 NoEffect (damage 1)]
        )
      ]
  , event
      20
      "Rivertown"
      ["Black Cave", "Black Cave"]
      [
        ( "Black Cave"
        , "You pass through the cave and walk through rooms from your memory. Gain one clue from your neighborhood. You try to navigate through this dream labyrinth, from a classroom to a hospital to a library (lore). If you pass, you wake up with an object you found in the dream; gain one curio."
        , Seq [clue, pass Lore 0 curioItem]
        )
      ,
        ( "General Store"
        , "As you consider what you might want to buy, a young woman enters and behaves strangely, bowing and gesturing to the empty air. You watch her keenly (observation). If you pass, you see that she is sleepwalking and probably dreaming of some courtly dance; gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Graveyard"
        , "You do not recall falling asleep but you wake up inside a crypt. You need to force the door open to escape (strength). If you pass, you step outside and examine the crypt, noting symbols of the Order of Dagon; gain one remnant and one clue from your neighborhood. If you fail, you bang on the door until it opens; suffer one damage."
        , Test Strength 0 (Seq [remnants 1, clue]) (damage 1)
        )
      ]
  , event
      21
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "The walls of the cave echo with exquisite music, sung in a language that only you can understand. Gain one spell. The music eventually fades and you struggle to remember everything else the song said (will). If you pass, you recall a passage about a golden city; gain one clue from your neighborhood."
        , Seq [spell, pass Will 0 clue]
        )
      ,
        ( "General Store"
        , "Davy Schoffner is late opening the store. He apologizes for oversleeping and says, \"I was in the middle of the most glorious dream. It was a home I never knew, but I miss it even now.\" Gain one clue from your neighborhood. Once the shop is open, he takes his place at the cash register. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "One of the gravestones too worn to read has had the phrase \"Not dead but dreaming\" painted on it. You may become delayed to dig up the grave to determine who was buried there. If you do, you discover a skeleton that appears to be a strange hybrid of human and fish; gain one clue from your neighborhood."
        , mayPay CostDelayed clue
        )
      ]
  , event
      22
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "In the darkness you hear the ocean's roar. The farther you descend, the colder the cave becomes and the harder it is for you to breathe. The walls are covered in luminescent algae that makes your mind buzz. You may become TAINTED to gain one remnant and one clue from your neighborhood."
        , May "Become TAINTED to press on" (Seq [tainted, remnants 1, clue])
        )
      ,
        ( "General Store"
        , "An angry man accuses the shopkeeper of selling goods hexed by the devil. After you intervene, the shopkeeper rewards you. Gain one common item. You admire your new acquisition (observation). If you pass, you spot a strange rune and remove it; gain one clue from your neighborhood. If you fail, it seems fine; become TAINTED."
        , Seq [commonItem, Test Observation 0 clue tainted]
        )
      ,
        ( "Graveyard"
        , "The church asks for your help in removing a blasphemous statue from the graveyard. It depicts a figure with malformed features walking into the ocean. Gain one clue from your neighborhood. It is not clear where the statue came from, but it is extremely difficult to move (strength). If you pass, the church rewards you; gain $3."
        , Seq [clue, pass Strength 0 (money 3)]
        )
      ]
  , event
      23
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The cave floor is littered with ocean fish, some still alive. One of these creatures has been torn open with a knife in some apparent attempt at divination. You try to read the future in the entrails (lore). If you pass, you see something very old returning; gain one clue from your neighborhood and one spell."
        , pass Lore 0 (Seq [clue, spell])
        )
      ,
        ( "General Store"
        , "The shopkeeper seems distracted and half asleep. He is not paying attention to your purchases and undercharges for everything. You may buy one common item from the display for half price (rounded up). If you buy anything, he tells you that you are nothing more than a dream; gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") HalfPrice (Just 1) clue
        )
      ,
        ( "Graveyard"
        , "Someone left a collection of gravestone rubbings and drawings. Gain one remnant. The illustrations include unsettling images of sea monsters devouring sailors (will). If you pass, you recognize one of the sailors; gain one clue from your neighborhood. If you fail, the drawings seem to move as you look at them; become TAINTED."
        , Seq [remnants 1, Test Will 0 clue tainted]
        )
      ]
  , event
      24
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "A collection of older citizens share ghost stories from their youth. You may spend one remnant to ask if they recognize the threat you now face. If you do; gain one clue from your neighborhood and one curio as each of them tells a variation of the same tale and offers what they can to help."
        , mayPay (SpendRemnants 1) (Seq [clue, curioItem])
        )
      ,
        ( "Ma's Boarding House"
        , "Ma shows you various things people have left behind in the rooms, including poems and drawings. Many of them feature aquatic themes and strange verses about transformation. Gain one clue from your neighborhood. You may spend $1 to stay for one of Ma's meals. If you do, you or an ally may recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "South Church"
        , "The church is hosting a wedding. You are welcome to witness the ceremony, but a contribution to the couple is customary. You may spend $2 to watch the happy event and for you or an ally to recover two sanity. If you do, you overhear several conversations about dreams of ancient cities; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [sanity 2, clue])
        )
      ]
  , event
      25
      "Southside"
      ["Historical Society", "Historical Society"]
      [
        ( "Historical Society"
        , "The original plans for several buildings are on display. You ask an expert to explain the bizarre features (influence). If you pass, she says local architects used elements from dreams in their work; gain one clue from your neighborhood. If you fail, the non-Euclidean methods disturb you; suffer one horror."
        , Test Influence 0 clue (horror 1)
        )
      ,
        ( "Ma's Boarding House"
        , "Ma offers you a hot meal in exchange for helping her move furniture around. You or an ally may recover two health. When you move the wardrobe you can hear something loose inside (observation). If you pass, your search reveals a painting of a ship sailing into a sunset; gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "South Church"
        , "You watch members of the church light candles and pray, and you are inspired to join them. You or an ally may recover two sanity. You may spend $1 to contribute to the poor box. If you do, Father Michael confides in you that several members of the church have succumbed to a sleeping sickness; gain one clue from your neighborhood."
        , Seq [sanity 2, mayPay (SpendMoney 1) clue]
        )
      ]
  , event
      26
      "Southside"
      ["Historical Society", "South Church"]
      [
        ( "Historical Society"
        , "You were hoping to meet a friend at tonight's lecture, but the speaker is late. You may become delayed to wait for him to arrive. If you do, you meet your friend and enjoy a lecture about historic artists from Arkham; gain one clue from your neighborhood and one ally."
        , mayPay CostDelayed (Seq [clue, ally])
        )
      ,
        ( "Ma's Boarding House"
        , "One of Ma's guests is a painter from Kingsport's Artist Colony. Ma warns you that her work is unpleasant and you may not like it. You may talk to the artist over dinner and examine her paintings of ruined, flooded cities. If you do, become TAINTED to gain one clue from your neighborhood and for you or an ally to recover two health."
        , May "Talk to the artist over dinner" (Seq [tainted, clue, health 2])
        )
      ,
        ( "South Church"
        , "A homeless man has fallen asleep on one of the pews. In his sleep, he whispers of the Great Dreamer of R'lyeh. Gain one clue from your neighborhood. You may spend $1 to help the man recover from his woes. If you do, he thanks you profusely and you know you've made a difference; you or an ally may recover two sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 2)]
        )
      ]
  , event
      27
      "Southside"
      ["South Church"]
      [
        ( "Historical Society"
        , "You chat with a stranger in a gallery from Kingsport's Artist Colony. Gain an ally. You examine a bizarre painting of a royal court populated by fish, lobsters, and other sea life (observation). If you pass, you see a hidden form in the background; gain one clue from your neighborhood. If you fail, you love it."
        , Seq [ally, pass Observation 0 clue]
        )
      ,
        ( "Ma's Boarding House"
        , "Ma seems tired and upset, and you hear she has not been eating. You may spend $1 to bring some food to her and share a meal. If you do, she tells you about awful dreams she has been having in which she drowns over and over; gain one clue from your neighborhood and you or an ally may recover one health."
        , mayPay (SpendMoney 1) (Seq [clue, health 1])
        )
      ,
        ( "South Church"
        , "Resting in one of the pews, you admire the architecture, statuary, and stained glass. You take comfort in all the beauty. You or an ally may recover two sanity. You study the images in detail (observation). If you pass, the imagery of fish and fishermen seems strangely omnipresent; gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ]
  , event
      28
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "A professor is trying to dispel Arkham's reputation for the supernatural. He describes several occult events and provides other explanations. Gain one clue from your neighborhood. He challenges you to show him proof of a genuine supernatural event. You may spend one remnant to gain one curio."
        , Seq [clue, mayPay (SpendRemnants 1) curioItem]
        )
      ,
        ( "Ma's Boarding House"
        , "One of Ma's guests fell sick during the night and you summon a doctor to tend to the man. You may spend $1 to have the doctor treat you as well; you or an ally recover two health. If you do, he tells you about the sleeping sickness that afflicted the guest; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "South Church"
        , "Father Michael invites you to stay and rest if you like. He assures you that you can sleep safely here. You may become delayed to fall asleep on a pew and for you or an ally to recover two sanity. If you do, you dream of strange stars looking down as an island rises up out of the sea; gain one clue from your neighborhood."
        , mayPay CostDelayed (Seq [sanity 2, clue])
        )
      ]
  , event
      29
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "You suddenly wake up from a vivid dream to find yourself digging up a real grave. Gain one clue from your neighborhood. You dig for quite some time (strength). If you pass, you uncover a long-buried object; gain one common item. If you fail, you exhaust yourself; suffer two damage."
        , Seq [clue, Test Strength 0 commonItem (damage 2)]
        )
      ,
        ( "St. Mary's Hospital"
        , "The doctors have fallen into a deep sleep, but the staff is willing to sell you some basic painkillers and antiseptics. You may spend $1 for you or an ally to recover two health. If you do, you see that the hospital's pharmacy is completely out of laudanum; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher is pleased to see you admiring her painting. It depicts an enormous pyramid on an island. You look at it closely (observation). If you pass, you see eyes lurking in the dark shadows of the pyramid and a swirling miasma of runes dancing in the painted waves; gain one clue from your neighborhood and one spell."
        , pass Observation 0 (Seq [clue, spell])
        )
      ]
  , event
      30
      "Uptown"
      ["Hangman's Hill", "Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "Someone dug up an old grave here, leaving behind a hole in the ground, a pile of dirt, and a few links of an old chain. Gain one remnant. You search for any sign of whoever dug the hole (observation). If you pass, you find an abandoned paint brush; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "St. Mary's Hospital"
        , "The doctor invites you to an examination room to treat your wounds. You or an ally may recover three health. Inside the room you see a painting of a ship in a storm. You stare at the image (observation). If you pass, you feel the room tilt back and forth; gain one clue from your neighborhood. If you fail, you fear what lies beneath the sea; become TAINTED."
        , Seq [health 3, Test Observation 0 clue tainted]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "A large crowd is here and Miriam Beecher is happy to meet the demand. Reveal the top three spells from the deck; you may buy one of them for half price (rounded up). If you buy something, the other customers talk to you about the bad dreams plaguing all the psychics in Arkham; gain one clue from your neighborhood."
        , Seq [spells 3 (Just 1) HalfPrice, clue]
        )
      ]
  , event
      31
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "You are not sure if you are dreaming or not, but you feel yourself sinking into the ground (will). If you pass, the spirits of the dead whisper secrets in your ear; gain one clue from your neighborhood. If you fail, you wake up after experiencing death by smothering in your dream; suffer two horror."
        , Test Will 0 clue (horror 2)
        )
      ,
        ( "St. Mary's Hospital"
        , "When you arrive, every person in the lobby is asleep, murmuring about a beautiful city. Gain one clue from your neighborhood. When you wake the staff, they seem happy and eager to treat your injuries. You may spend $1 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher tells you she has dreamed of this meeting. She hands you a copy of a ritual. Gain one spell. She describes her dream in detail and you try to retain the details (observation). If you pass, she predicts events in your near future; gain one clue from your neighborhood. If you fail, the dream is incomprehensible; suffer one horror."
        , Seq [spell, Test Observation 0 clue (horror 1)]
        )
      ]
  , event
      32
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "All around you, the sound of singing rises up out of the ground (will). If you pass, you follow the song to the open and empty grave of someone named Charity Marsh; gain one clue from your neighborhood and one remnant. If you fail, the voices grow louder and louder; become TAINTED."
        , Test Will 0 (Seq [clue, remnants 1]) tainted
        )
      ,
        ( "St. Mary's Hospital"
        , "A comatose patient has been singing in the middle of the night. If you want to record what the patient says, the staff will allow you to stay in the hospital overnight. You may become delayed to write down the lyrics while you rest; gain one clue from your neighborhood and you or an ally may recover two health."
        , mayPay CostDelayed (Seq [clue, health 2])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Occultists are flocking to Miriam Beecher's store, eager to share their prescient dreams. Gain one clue from your neighborhood. Miriam appreciates a paying customer, given all the loiterers. Reveal the top three spells of the spell deck. You may buy any number of them; put the rest on the bottom of the deck."
        , Seq [clue, spells 3 Nothing FullPrice]
        )
      ]
  ]
