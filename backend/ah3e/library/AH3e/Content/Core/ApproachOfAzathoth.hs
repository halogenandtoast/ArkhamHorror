module AH3e.Content.Core.ApproachOfAzathoth (scenario, cards) where

import AH3e.Content.Tiles
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

code :: ScenarioCode
code = "approach-of-azathoth"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Approach of Azathoth"
    , expansion = CoreSet
    , startingSpace = spaceIdFor "Train Station"
    , reckoningText =
        "For each cultist monster, place one doom in its space. (If it's in a street space, place the doom in an adjacent neighborhood space instead.)"
    , reckoning = Custom "aoa-reckoning"
    , setupMap =
        buildMap
          [nb "Northside", nb "Downtown", nb "Easttown", nb "Merchant District", nb "Rivertown"]
          [ StreetDef (nb "Northside") SideRight (nb "Downtown") Scenic
          , StreetDef (nb "Northside") BottomRight (nb "Merchant District") Scenic
          , StreetDef (nb "Downtown") BottomLeft (nb "Merchant District") Residential
          , StreetDef (nb "Downtown") BottomRight (nb "Rivertown") Bridge
          , StreetDef (nb "Downtown") SideRight (nb "Easttown") Residential
          , StreetDef (nb "Merchant District") SideRight (nb "Rivertown") Residential
          , StreetDef (nb "Rivertown") TopRight (nb "Easttown") Bridge
          ]
    , monsters =
        [ ("abyssal-servant", 1)
        , ("eyeless-watcher", 1)
        , ("high-priest", 1)
        , ("hooded-stalker", 2)
        , ("occult-ritualist", 2)
        , ("robed-figure", 3)
        , ("swift-byakhee", 1)
        , -- every Hound of Tindalos
          ("coursing-hound", 1)
        , ("keening-hound", 1)
        , ("ravenous-predator", 1)
        , ("tindalos-alpha", 1)
        ]
    , startingMonsters =
        [("robed-figure", spaceIdFor "Independence Square"), ("robed-figure", spaceIdFor "Black Cave")]
    , mythosCup =
        [ (SpreadDoomToken, 3)
        , (SpawnMonsterToken, 2)
        , (ReadHeadlineToken, 2)
        , (SpawnClueToken, 2)
        , (GateBurstToken, 1)
        , (ReckoningToken, 1)
        , (BlankToken, 3)
        ]
    , startingDoom =
        map
          spaceIdFor
          ["Arkham Advertiser", "Independence Square", "Velma's Diner", "Unvisited Isle", "Black Cave"]
    , eventCards = [CardCode ("aoa-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , codex = [2, 3]
    , anomalySet = Just "Temporal Fissure"
    , terrorSet = Nothing
    }

pad :: Int -> Text
pad n = T.justifyRight 2 '0' (tshow n)

cards :: [CardDef]
cards = archive <> events <> anomalies

-- effect vocabulary
clue, spell, ally, commonItem, curioItem :: Effect
clue = GainE ClueFromNeighborhood
spell = GainE (ASpell Nothing)
ally = GainE (AnAlly Nothing)
commonItem = GainE (AnItem (Just "Common"))
curioItem = GainE (AnItem (Just "Curio"))

money, remnants :: Int -> Effect
money n = GainE (Money (N n))
remnants n = GainE (Remnants (N n))

pass :: Skill -> Int -> Effect -> Effect
pass sk m e = Test sk m e NoEffect

mayPay :: Cost -> Effect -> Effect
mayPay c e = MayPay c e NoEffect

buyAny, buyOne :: Trait -> Effect
buyAny t = BuyFromDisplay (Just t) False Nothing NoEffect
buyOne t = BuyFromDisplay (Just t) False (Just 1) NoEffect

sanity, health :: Int -> Effect
sanity n = RecoverSanity YouOrAlly (N n)
health n = RecoverHealth YouOrAlly (N n)

horror, damage :: Int -> Effect
horror n = SufferHorror (N n)
damage n = SufferDamage (N n)

cursed :: Effect
cursed = GainE (Condition "CURSED")

-- archive
archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n title front back =
  CardDef
    (CardCode ("archive-" <> tshow n))
    title
    CoreSet
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

archive :: [CardDef]
archive =
  [ archiveCard
      2
      "Anomalies"
      "When a space has three doom or a neighborhood has a total of five doom, place an anomaly in that neighborhood. If additional doom would be placed in a neighborhood with an anomaly, place that doom on the scenario sheet instead. If you would resolve an encounter in a neighborhood with an anomaly, you resolve an anomaly encounter instead. When a neighborhood has zero doom, remove the anomaly from that neighborhood."
      Nothing
  , archiveCard
      3
      "Déjà Vu"
      "[Objective] When there are three or more clues on the scenario sheet, add card 4 to the codex. (Do not remove this card from the codex.)\n[Doom] When there is three or more doom on the scenario sheet, flip this card."
      ( Just
          "Reveal cards from the bottom of the monster deck until you reveal a cultist monster. Spawn it in the street space nearest the leader and put the rest on the top of the deck in a random order. Add card 5 to the codex and return this card to the archive."
      )
  , archiveCard
      4
      ""
      "Take five markers—one blue, one red, and three white—and randomize them facedown. Place one each facedown on the Arkham Advertiser, the Black Cave, Independence Square, the Unvisited Isle, and Velma's Diner. Then flip this card."
      ( Just
          "Profane Ritual\nAction: Reveal a marker at your location and resolve the effect below based on its color.\nWhen you reveal a white marker, you find nothing of interest; discard that marker.\nWhen you reveal the blue marker, add card 6 to the codex. (Do not remove this card from the codex.)\nWhen you reveal the red marker, if \"the barrier is destroyed,\" add card 7 to the codex facedown. Otherwise, add that card to the codex faceup. (Do not remove this card from the codex.)\nIf both the blue and red markers have been revealed, remove all other markers from the board and return this card to the archive."
      )
  , archiveCard
      5
      "Time Unstuck"
      "[Objective] When there are three or more clues on the scenario sheet, if there are no markers on the board, add card 4 to the codex. (Do not remove this card from the codex.)\n[Doom] When there is seven or more doom on the scenario sheet, add cards 8 and 9 to the codex and return this card to the archive."
      (Just "Investigators win the game!")
  , archiveCard
      6
      "Ritual Site"
      "The space with the blue marker is the \"ritual site.\" Reveal cards from the bottom of the monster deck until you reveal two cultist monsters. Spawn them at the ritual site and put the rest on the top of the deck in a random order.\n[Objective] Action: You may remove five clues from the scenario sheet to stop the ritual. If you do, remove the blue marker and flip this card. Perform this action only at the ritual site."
      (Just "Barrier Destroyed\nThe barrier is destroyed. (Do not remove this card from the codex.)")
  , archiveCard
      7
      "A Strange Barrier"
      "The space with the red marker is the \"epicenter.\" For the time being, a magical barrier prevents you from entering the area. Each investigator at the epicenter must move one space away. Investigators cannot move to the epicenter until the barrier is destroyed.\nWhen \"the barrier is destroyed,\" flip this card."
      ( Just
          "Distortion's Epicenter\nThe space with the red marker is the \"epicenter.\"\n[Objective] Action: You may remove three clues from the scenario sheet to erect a protective ward over Arkham. If you do, read the back of card 5. Perform this action only at the epicenter."
      )
  , archiveCard
      8
      ""
      "Add one spread doom and one gate burst token to the mythos cup. Reveal all facedown markers and remove all markers but the red one. If there are no markers, place one red marker faceup in the starting space. Return cards 4, 6, and 7 to the archive (if they are in the codex). Then flip this card."
      ( Just
          "Azathoth Awakens!\nThe space with the red marker is the \"epicenter.\"\nAction: You move to \"the future.\" Perform this action only at the epicenter. (The future is not adjacent to any other space. You will not be able to move back with a normal move action.)\n[Objective] When there are five clues on this card, flip card 9 and read the \"Lost in Time\" effect.\n[Doom] When there is thirteen or more doom on the scenario sheet, flip card 9 and read the \"Blind Chaos\" effect."
      )
  , archiveCard
      9
      "The Future"
      "This card is a space called \"the future.\" If doom would be placed on this space, place that doom on the scenario sheet instead.\n[Objective] Action: You prepare for a massive ritual, enclosing the entire city in a circle inscribed with the crooked star of the Elder Sign (lore −2). If you pass, move one clue from the scenario sheet to card 8. Perform this action only if you are in the future.\nInstead of resolving an encounter on this space, you fear this future is what awaits the Earth you call home; you suffer two horror."
      (Just "Lost in Time\nInvestigators win the game!\n\nBlind Chaos\nInvestigators lose the game.")
  ]

-- events
event :: Int -> Text -> Text -> [(Text, Text, Effect)] -> CardDef
event n hood doomAt encs =
  CardDef
    (CardCode ("aoa-event-" <> pad n))
    ("Event " <> tshow n <> "/24")
    CoreSet
    1
    ( EventCard
        EventDef
          { scenario = code
          , neighborhood = nb hood
          , encounters = Map.fromList [(spaceIdFor place, Encounter txt eff) | (place, txt, eff) <- encs]
          , doomSpaces = [spaceIdFor doomAt]
          }
    )

events :: [CardDef]
events =
  [ event
      1
      "Easttown"
      "Hibb's Roadhouse"
      [
        ( "Hibb's Roadhouse"
        , "The liquor and conversation in the roadhouse work wonders for your weary mind. You or an ally recovers two sanity. You hear a distinct clacking sound and look around for the source (observation). If you pass, you see a figure with cloven hooves slip out the door; you gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Police Station"
        , "\"They're a strange lot,\" says Sheriff Engle. \"We rounded 'em up by the river.\" You interrogate the robed figures in the holding cell (observation). If you pass, they won't talk, but you notice a symbol on their robes, a swirling ribbon threaded through a circle; you gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ,
        ( "Velma's Diner"
        , "\"Slice of pie, hon?\" offers Velma. You or an ally recovers two health. Velma returns a moment later. \"Slice of pie, hon?\" You may spend $1 to order another. If you do, the event repeats in a loop until you leave; you or an ally recovers two health and you gain one clue from your neighborhood."
        , -- the "loop" is flavor: $1 buys one more slice, once
          Seq [health 2, mayPay (SpendMoney 1) (Seq [health 2, clue])]
        )
      ]
  , event
      2
      "Rivertown"
      "Black Cave"
      [
        ( "Black Cave"
        , "There is evidence of recent habitation. The embers of the fire are still warm. You pick out half-burned pieces of paper (lore). If you pass, you decipher what you can: \"adonai Azat... magn... Salamandrae;\" you gain one spell and one clue from your neighborhood."
        , pass Lore 0 (Seq [spell, clue])
        )
      ,
        ( "General Store"
        , "Bunting and streamers decorate the front of the shop. A banner reads: \"Grand Opening!\" Mr. Hatle is young and spry, not the old man you know who plays checkers by the potbelly stove. You gain one clue from your neighborhood. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "The date on the grave is 1871, but the earth is freshly disturbed (strength). If you pass, you dig up—and then rebury—a still-fresh corpse from the 19th century with a coin on each eye; you gain $3 and one clue from your neighborhood. If you fail, the irate groundskeeper chases you away."
        , pass Strength 0 (Seq [money 3, clue])
        )
      ]
  , event
      3
      "Rivertown"
      "Graveyard"
      [
        ( "Black Cave"
        , "You emerge, blinking, into bright daylight, where settlers in 17th-century garb are putting a witch on trial. You gain one clue from your neighborhood. You listen intently to the witch (lore). If you pass, you understand her meaning; you gain one spell. If you fail, you climb back into the cave."
        , Seq [clue, pass Lore 0 spell]
        )
      ,
        ( "General Store"
        , "Davy Schoffner is looking rather glum, and old Mr. Hatle is nowhere to be seen. You may buy any number of common items from the display. If you buy something, Davy tells you it hasn't been the same since the \"re-re-opening\" without Uncle Hatle; you gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") False Nothing clue
        )
      ,
        ( "Graveyard"
        , "A few unidentified bodies are laid out to be buried. Either test (strength) to help with the burial or become delayed to be a pallbearer. If you pass or become delayed, you see signs the bodies were ritual sacrifices; you gain $3 and one clue from your neighborhood. If you fail, you're too tired to help."
        , Choose
            [ ("Test strength", pass Strength 0 (Seq [money 3, clue]))
            , ("Become delayed", Pay CostDelayed (Seq [money 3, clue]))
            ]
        )
      ]
  , event
      4
      "Easttown"
      "Velma's Diner"
      [
        ( "Hibb's Roadhouse"
        , "Your head throbbing, you consider a drink. You may spend $1 for you or an ally to recover three sanity. If you spend the money, you step outside for some fresh air and find three robed figures slicing open a horse's stomach in the stables; you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ,
        ( "Police Station"
        , "\"This woman says she's from the future.\" Either test (influence) to calm her or spend one remnant to prove she's right. If you pass or spend the remnant, she brings warnings from her time and gives you \"the key to your success;\" you gain one common item and one clue from your neighborhood."
        , Choose
            [ ("Test influence", pass Influence 0 (Seq [commonItem, clue]))
            , ("Spend one remnant", Pay (SpendRemnants 1) (Seq [commonItem, clue]))
            ]
        )
      ,
        ( "Velma's Diner"
        , "Deputy Dingby is taking Velma's statement. \"Robed figures. They painted that.\" She points to a symbol on the floor: a swirling ribbon threaded through a circle. You gain one clue from your neighborhood. Despite the mess, the diner is open. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ]
  , event
      5
      "Downtown"
      "Independence Square"
      [
        ( "Arkham Asylum"
        , "You hear a constant and unearthly piping melody and check yourself into the asylum. You or an ally recovers two sanity. The piping echoes through the halls (observation). If you pass, others can hear the sound and you realize it is not just your imagination; you gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Independence Square"
        , "Anna Kaslow, the old soothsayer, grips your hand and speaks of past and future as if they are one. You gain one clue from your neighborhood. Kaslow's predictions are chilling (will). If you pass, you accept her warnings and acquire what she says you need; you gain one curio item."
        , Seq [clue, pass Will 0 curioItem]
        )
      ,
        ( "La Bella Luna"
        , "You play cards with an alleged cultist. You buy him a drink hoping he lets slip some information (influence). If you pass, he's a lightweight drunk and he tells you what he knows while gambling away his money; you gain $3 and one clue from your neighborhood. If you fail, he leaves without saying anything."
        , pass Influence 0 (Seq [money 3, clue])
        )
      ]
  , event
      6
      "Northside"
      "Curiositie Shoppe"
      [
        ( "Arkham Advertiser"
        , "The editor pays you for a story you haven't written yet. You gain $2. You search for articles about Schoffner's (observation). If you pass, you find the details: established 1868, fire 1891, re-opened 1893, automobile accident 1937, re-re-opened 1938; you gain one clue from your neighborhood."
        , Seq [money 2, pass Observation 0 clue]
        )
      ,
        ( "Curiositie Shoppe"
        , "A young man introduces himself as Oliver Thomas and invites you into his shop, where a small tortoiseshell kitten frolics. \"That's The Baron,\" says Thomas. You gain one clue from your neighborhood. \"Anything catch your eye?\" You may buy any number of curio items from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "\"I got your message,\" says a stranger, \"and came as soon as I could.\" You gain one ally. You chat with your new friend to learn more (influence). If you pass, you recognize the handwriting on their note as your own, dated in the future; you gain one clue from your neighborhood."
        , Seq [ally, pass Influence 0 clue]
        )
      ]
  , event
      7
      "Easttown"
      "Velma's Diner"
      [
        ( "Hibb's Roadhouse"
        , "There's horse-drawn carriages and no cars tonight, but that doesn't affect the taste of the drink. You may spend $1 for you or an ally to recover three sanity. Something seems off (observation). If you pass, you realize the place is lit by gas lamps; you gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (sanity 3), pass Observation 0 clue]
        )
      ,
        ( "Police Station"
        , "This is where the police station should be, but all that's here now is the wreckage of an old red-brick building (observation). If you pass, you find the remains of the evidence room and discover the building collapsed in 1954; you gain one common item and one clue from your neighborhood."
        , pass Observation 0 (Seq [commonItem, clue])
        )
      ,
        ( "Velma's Diner"
        , "A sign in Velma's window shines, the word \"Open\" made of glowing glass tubes. You gain one clue from your neighborhood. The patrons sport flannel shirts and facial piercings while eating avocados on toast and the prices are outrageous. You may spend $5 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 5) (health 3)]
        )
      ]
  , event
      8
      "Merchant District"
      "Unvisited Isle"
      [
        ( "River Docks"
        , "The Japanese merchant vessel Sarutan Maru has been moored by the dock. The foreigners are willing to pay top dollar for any \"unusual findings.\" You may spend one remnant to sell what you have. If you do, they show you what they have collected so far; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "\"It's always midnight at the Tick-Tock,\" you remind yourself. Every clock is stuck at midnight and every person in the club is frozen in place. You gain one clue from your neighborhood. You'll have to pour your own drink and leave payment on the bar. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Unvisited Isle"
        , "You lose your way among the trees and try to remember how you got here (will). If you pass, retracing your steps causes time to flow backward and you find something you thought you had lost long ago; you gain one curio item and one clue from your neighborhood."
        , pass Will 0 (Seq [curioItem, clue])
        )
      ]
  , event
      9
      "Northside"
      "Arkham Advertiser"
      [
        ( "Arkham Advertiser"
        , "\"Extree! Extree!\" calls the newsboy. \"Schoffner's General burned down!\" The paper is dated 1891. You gain one clue from your neighborhood. A young-looking Doyle Jeffries asks if you've got a good tip to help get his foot in the door. You may spend one remnant to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "You find several interesting items for sale. You may buy any number of curio items from the display. Two robed figures are also making purchases (lore). If you pass, you note that the bronze flute and astrolabe they are buying could be used in an arcane ritual; you gain one clue from your neighborhood."
        , Seq [buyAny "Curio", pass Lore 0 clue]
        )
      ,
        ( "Train Station"
        , "The posters on the walls of the kiosks advertise local shows: The Jazz Singer, Lazarus Laughed, Antigone, Tipping the Velvet (influence). If you pass, you fall into conversation with the person next to you about the latest smash hit, Hamilton; you gain one ally and one clue from your neighborhood."
        , pass Influence 0 (Seq [ally, clue])
        )
      ]
  , event
      10
      "Merchant District"
      "Unvisited Isle"
      [
        ( "River Docks"
        , "Joey \"the Rat\" is dressing better these days and putting on magnanimous airs. \"I got a pal whose stock tips just don't quit. He says we got three years to make hay while the sun shines.\" You gain one clue from your neighborhood. \"You selling something or not?\" You may spend one remnant to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Tick-Tock Club"
        , "\"Private event tonight.\" Either test (influence) to insist you belong or spend $1 to bribe the bouncer. If you pass or spend the money, you are served wine and watch many of Arkham's richest people casually discuss the occult; you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , Choose
            [ ("Test influence", pass Influence 0 (Seq [sanity 3, clue]))
            , ("Spend $1", Pay (SpendMoney 1) (Seq [sanity 3, clue]))
            ]
        )
      ,
        ( "Unvisited Isle"
        , "Golden butterflies land on the branches of the surrounding trees and spin their countless cocoons. You may become delayed to watch what happens next. If you do, they hatch into a swarm of red and black caterpillars; you gain one clue from your neighborhood and one remnant."
        , mayPay CostDelayed (Seq [clue, remnants 1])
        )
      ]
  , event
      11
      "Downtown"
      "La Bella Luna"
      [
        ( "Arkham Asylum"
        , "\"The twin snakes of mind and matter! All barriers to health dissolved! The universal solvent Azoth!\" This new doctor seems unorthodox. You may spend $1 to try this method and ask for more details. If you do, you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ,
        ( "Independence Square"
        , "This monument features many names you recognize as current citizens of Arkham. A plaque states it was built in 1946 to honor those killed during \"the Second World War.\" You gain one clue from your neighborhood. There's a small market nearby. You may buy one common item from the display."
        , Seq [clue, buyOne "Common"]
        )
      ,
        ( "La Bella Luna"
        , "You're on a winning streak. You gain $2. Two robed figures slip into the back hall (observation). If you pass, you tail them and overhear them mention \"ritual sacrifice;\" you gain one clue from your neighborhood. If you fail, the bouncer catches you trying to sneak into the back hall."
        , Seq [money 2, pass Observation 0 clue]
        )
      ]
  , event
      12
      "Merchant District"
      "Tick-Tock Club"
      [
        ( "River Docks"
        , "You find something crawling with snails. You gain one common item. The spiral of their iridescent shells reminds you of something (observation). If you pass, you relate the spiral to that of the galaxy; you gain one clue from your neighborhood. If you fail, you contemplate the swirling abyss of the universe."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Tick-Tock Club"
        , "Some days, a little live music and hot food is the best tonic. You or an ally recovers one health and one sanity. While you rest, you notice that several of the clocks are stopped (lore). If you pass, you read the word \"Azathoth\" encoded in the stopped clock hands; you gain one clue from your neighborhood."
        , Seq [RecoverBoth YouOrAlly (N 1) (N 1), pass Lore 0 clue]
        )
      ,
        ( "Unvisited Isle"
        , "Seven robed figures stand in a loose circle around a fire and chant. \"Chaos leads us, and Chaos takes us!\" You gain one clue from your neighborhood. You do your best to remain quiet and observe the whole ritual (will). If you pass, they leave something behind when they're done; you gain one curio item."
        , Seq [clue, pass Will 0 curioItem]
        )
      ]
  , event
      13
      "Northside"
      "Curiositie Shoppe"
      [
        ( "Arkham Advertiser"
        , "You see tomorrow's paper roll off the presses—it's the same as yesterday's and today's. You may spend one remnant to shake everyone out of their repetition. If you do, the editor reworks the new edition to include your hot tip; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Curiositie Shoppe"
        , "The shop cat nudges something off the shelf, and Mr. Thomas offers it to you at a discount. You may buy one curio item from the display for half price (rounded up). If you do, the object whispers secrets to you of its long life in the hands of an old warlock; you gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Curio") True (Just 1) clue
        )
      ,
        ( "Train Station"
        , "The 7:30, 10:50, and 3:15 trains all arrive at the same time. You gain one clue from your neighborhood. You ask one of the travelers for details of their journey (influence). If you pass, the traveler agrees that something is odd; you gain one ally. If you fail, everyone is running late."
        , Seq [clue, pass Influence 0 ally]
        )
      ]
  , event
      14
      "Merchant District"
      "River Docks"
      [
        ( "River Docks"
        , "\"I collect strange stories and objects,\" says one of the oddly dressed sailors. He expounds at length about the occult and how the alignment of the stars affects magical forces. You gain one clue from your neighborhood. \"I'll buy whatever you've got,\" he says. You may spend one remnant to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Tick-Tock Club"
        , "The smell of something hot and covered in gravy comes from the kitchen. You may spend $1 for you or an ally to recover three health. Something about the clocks is off (observation). If you pass, the clocks aren't just set at different times, they're moving at different speeds; you gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (health 3), pass Observation 0 clue]
        )
      ,
        ( "Unvisited Isle"
        , "The book said that each stone in the circle must be touched in a certain order (lore). If you pass, the man in the robe approves and hands you his gift; you gain one curio item and one clue from your neighborhood. If you fail, a bubbling mass appears in the center of the circle; you suffer one horror."
        , Test Lore 0 (Seq [curioItem, clue]) (horror 1)
        )
      ]
  , event
      15
      "Northside"
      "Train Station"
      [
        ( "Arkham Advertiser"
        , "You flip through the obituaries: Grams, Pickman, Waite. They stretch back decades. You may become delayed to sift through the archives. If you do, you discover a pattern of deaths that you believe is linked to occult activity and write a story about it; you gain one clue from your neighborhood and $3."
        , mayPay CostDelayed (Seq [clue, money 3])
        )
      ,
        ( "Curiositie Shoppe"
        , "You find an odd item on the shelf, a plastic case with a silvery disk inside and a rubber cord running to small headphones. You gain one clue from your neighborhood. The wizened proprietor asks you if you'd like to buy anything. You may buy any number of curio items from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "You ask for help from a passerby when you are stumped by the turnstile (influence). If you pass, your new friend explains that a magnetic reader scans your ticket and shows you how to work the automated kiosk; you gain one clue from your neighborhood and one ally."
        , pass Influence 0 (Seq [clue, ally])
        )
      ]
  , event
      16
      "Downtown"
      "La Bella Luna"
      [
        ( "Arkham Asylum"
        , "You round a corner and come face to face with a young Charles Badoe before he became the asylum's chief psychiatrist. You gain one clue from your neighborhood. Charles is eager to make a name for himself and offers his services. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Independence Square"
        , "Today is the Arkham Art Fair. You may buy any number of curio items from the display. One artist is painting a futuristic cityscape (observation). If you pass, you recognize details that make you believe the city is Arkham; you gain one clue from your neighborhood."
        , Seq [buyAny "Curio", pass Observation 0 clue]
        )
      ,
        ( "La Bella Luna"
        , "The croupier rubs his ear and spins the wheel. The ball lands in the \"7\" pocket. The croupier rubs his ear, and again the ball lands on \"7\" (observation). If you pass, you realize the club is caught in a time loop and bet on \"7\" before leaving with your winnings; you gain $3 and one clue from your neighborhood."
        , pass Observation 0 (Seq [money 3, clue])
        )
      ]
  , event
      17
      "Northside"
      "Train Station"
      [
        ( "Arkham Advertiser"
        , "You find a newspaper in the archives that is dated 1937 and shows a car crashed through the front of Schoffner's General Store. You gain one clue from your neighborhood. You may spend one remnant to give Minnie Klein some evidence for a story she's writing. If you do, you gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "There are a number of items for sale today. You may buy any number of curio items from the display. If you buy something, the proprietor, Oliver Thomas, mentions the \"unusual clustering of stars\" he saw last night while he wraps up your purchase; you gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Curio") False Nothing clue
        )
      ,
        ( "Train Station"
        , "You find a traveler looking confused and frightened (influence). If you pass, your new friend asks you if you hear the music from beyond the stars and is relieved when you admit that you do; you gain one ally and one clue from your neighborhood. If you fail, the traveler stumbles off."
        , pass Influence 0 (Seq [ally, clue])
        )
      ]
  , event
      18
      "Rivertown"
      "Black Cave"
      [
        ( "Black Cave"
        , "You follow the sound of footsteps into the cavern, then hear footsteps behind you (will). If you pass, you duck into a shadow and watch yourself pass by; you gain one clue from your neighborhood. Whether you pass or not, there's something written here on the wall; you may become delayed to gain one spell."
        , Seq [pass Will 0 clue, mayPay CostDelayed spell]
        )
      ,
        ( "General Store"
        , "Mr. Hatle sits disconsolately on the crates he was able to rescue. The smell of woodsmoke still hangs in the air. \"I gotta get a stake together to rebuild.\" You may buy any number of common items. If you buy something, \"My nephew Davy will be taking over;\" you gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") False Nothing clue
        )
      ,
        ( "Graveyard"
        , "A man digs a grave, lies down inside it, and then an identical man comes and fills it up with dirt. After a rest, the man begins to dig the grave again. You gain one clue from your neighborhood. The groundskeeper tells you to stop gawking and get to work (strength). You gain $1 for each success you roll."
        , Seq [clue, Test Strength 0 (GainE (Money TestResult)) NoEffect]
        )
      ]
  , event
      19
      "Easttown"
      "Hibb's Roadhouse"
      [
        ( "Hibb's Roadhouse"
        , "\"Prohibition Repeal is Ratified\" claims the headline of a newspaper dated December 5, 1933. You gain one clue from your neighborhood. The patrons of the roadhouse celebrate with less-than-quiet restraint. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Police Station"
        , "You find yourself in the past, speaking to one Deputy Engle. You gain one clue from your neighborhood. Either test (influence) to convince him of your need or spend one remnant to prove how dire the situation is. If you pass or spend the remnant, he offers you some help; you gain one common item."
        , Seq
            [ clue
            , Choose
                [ ("Test influence", pass Influence 0 commonItem)
                , ("Spend one remnant", Pay (SpendRemnants 1) commonItem)
                ]
            ]
        )
      ,
        ( "Velma's Diner"
        , "The diner is as warm and cozy as you remember. You may spend $1 for you or an ally to recover three health. It feels like this place never changes (observation). If you pass, even the newspaper is the same one from last week and the week before that; you gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (health 3), pass Observation 0 clue]
        )
      ]
  , event
      20
      "Merchant District"
      "Tick-Tock Club"
      [
        ( "River Docks"
        , "\"I collect strange stories and objects,\" says one of the oddly dressed sailors. \"You got anything like that for trade?\" You may spend one remnant to add to his collection. If you do, he shares his knowledge of your offering; you gain one common item and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [commonItem, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "You're turned away by a bouncer. You may spend $1 to grease his palm. If you do, you eat and drink and listen as the strangely unmemorable band plays its jazzy rendition of \"The Blind Idiot Stomp;\" you or an ally recovers two health and two sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [RecoverBoth YouOrAlly (N 2) (N 2), clue])
        )
      ,
        ( "Unvisited Isle"
        , "You study the standing stones on the island, but when you glance across the river, Arkham is gone. You gain one clue from your neighborhood. Days and nights pass in eyeblinks as you watch the city grow (will). If you pass, you dig up something you watched buried years ago; you gain one curio item."
        , Seq [clue, pass Will 0 curioItem]
        )
      ]
  , event
      21
      "Rivertown"
      "Graveyard"
      [
        ( "Black Cave"
        , "A strange echo leads you deeper into the cave, where suddenly, music jumps into your mind. You gain one spell. You try to remember your way out (will). If you pass, you see yourself coming in and slip into the shadows so you aren't noticed; you gain one clue from your neighborhood."
        , Seq [spell, pass Will 0 clue]
        )
      ,
        ( "General Store"
        , "A large banner reads: \"Grand Re-Opening!\" Davy Schoffner looks younger than ever, and old Mr. Hatle isn't looking so old either. You gain one clue from your neighborhood. \"I can tell already, you're going to be a long time customer!\" You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "\"Thanks for your help,\" says the groundskeeper, handing you payment you did not earn. You gain $2. You try to play it cool (will). If you pass, you see yourself, sleeves rolled up and hard at work; you gain one clue from your neighborhood. If you fail, you just pocket the money and flee."
        , Seq [money 2, pass Will 0 clue]
        )
      ]
  , event
      22
      "Merchant District"
      "River Docks"
      [
        ( "River Docks"
        , "\"I collect strange stories and objects,\" says one of the oddly dressed sailors. Either test (lore) to tell him a good tale or spend one remnant to show him something he'll never forget. If you pass or spend the remnant, he shares what he's seen in Arkham; you gain one clue from your neighborhood and $3."
        , Choose
            [ ("Test lore", pass Lore 0 (Seq [clue, money 3]))
            , ("Spend one remnant", Pay (SpendRemnants 1) (Seq [clue, money 3]))
            ]
        )
      ,
        ( "Tick-Tock Club"
        , "The clocks are all moving backward. Even more alarming is the melody of the performers in reverse. You gain one clue from your neighborhood. Despite the unusual occurrence, Dainty Donohue tells you to buy something or get out. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "Unvisited Isle"
        , "The hut seems deserted except for a trinket on the table. You gain one curio item. A series of names and dates is scratched into the door (lore). If you pass, you notice a correlation between the dates and events throughout Arkham's history; you gain one clue from your neighborhood."
        , Seq [curioItem, pass Lore 0 clue]
        )
      ]
  , event
      23
      "Downtown"
      "Independence Square"
      [
        ( "Arkham Asylum"
        , "Doctor Mintz wants to give you electroshock therapy, which he promises is safe. You may spend $1 to accept the treatment. If you do, during the convulsions you glimpse the infinity beyond yourself; you recover two sanity and gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [RecoverSanity You (N 2), clue])
        )
      ,
        ( "Independence Square"
        , "A group of musicians play flutes and horns while dancing around a robed figure who sits on a throne. You may spend $1 to toss a few coins into their hat. If you do, the robed figure offers you a gift and you see that he has no face under his hood; you gain one curio item and one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [curioItem, clue])
        )
      ,
        ( "La Bella Luna"
        , "The storefront is boarded up. You ask a passerby what happened. \"You been outta town? It's been like that since the Sheldon Gang shot the place up a few months ago.\" You gain one clue from your neighborhood. You search the premises (observation). If you pass, you find some cash; you gain $3."
        , Seq [clue, pass Observation 0 (money 3)]
        )
      ]
  , event
      24
      "Northside"
      "Arkham Advertiser"
      [
        ( "Arkham Advertiser"
        , "Minnie Klein is desperate for a good story. You may spend one remnant to show her something that will wow her. If you do, she shares her findings with you and exults over your evidence, finally making it all click together; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Curiositie Shoppe"
        , "Oliver Thomas seems distracted today. You may buy any number of curio items from the display. As Oliver packages your purchases, you watch the store's tortoiseshell cat walk up the stairs (observation). If you pass, the cat ascends the stairs again a moment later; you gain one clue from your neighborhood."
        , Seq [buyAny "Curio", pass Observation 0 clue]
        )
      ,
        ( "Train Station"
        , "\"How much do you love your fellow man? Wouldn't the earth be cleaner without them?\" You whirl around, but no one is there. You gain one clue from your neighborhood. \"Are you alright? You look pale\" (influence). If you pass, you make a new friend; you gain one ally. If you fail, you flee the station."
        , Seq [clue, pass Influence 0 ally]
        )
      ]
  ]

-- Temporal Fissure anomalies
anomaly :: Int -> [((Int, Maybe Int), Text, Effect)] -> CardDef
anomaly n sections =
  CardDef
    (CardCode ("temporal-fissure-" <> pad n))
    "Temporal Fissure"
    CoreSet
    1
    ( AnomalyCard
        (AnomalyDef "Temporal Fissure" [(range, Encounter txt eff) | (range, txt, eff) <- sections])
    )

nearby :: Int -> Effect
nearby n = RemoveDoomFrom SpaceInYourNeighborhood (N n)

here :: Int -> Effect
here n = RemoveDoomFrom YourSpace (N n)

anomalies :: [CardDef]
anomalies =
  [ anomaly
      1
      [
        ( (0, Just 1)
        , "A one-armed beggar in a dirty silver military uniform rattles his tin at you and babbles about the moon. You may spend $2 to drop a few coins in his cup. If you do, he warns you about the \"Nemesis Moon\" that \"heralds the Daemon Sultan;\" remove one doom from any space in your neighborhood."
        , mayPay (SpendMoney 2) (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "Your vision blurs. Buildings fall. Cities quake and burn. You clutch your head and refocus (will). If you pass, it all vanishes like evaporating dew; you remove one doom from your space. If you fail, bodies fall through space and your headache and nausea worsen; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "You learn fascinating and helpful things from an obscure book (lore −2). If you pass, the book whispers secrets of the universe to you; you remove three doom from your space and gain one remnant. If you fail, the messages in the writing are haunting; you become CURSED."
        , Test Lore (-2) (Seq [here 3, remnants 1]) cursed
        )
      ]
  , anomaly
      2
      [
        ( (0, Just 1)
        , "You dream of a library orbiting some distant world. On the highest shelf is the book of your life (lore). If you pass, you read the words that you will intone tomorrow; you remove one doom from any space in your neighborhood. If you fail, you can't resist reading how your story ends; you suffer one horror."
        , Test Lore 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "A bolt of fire streaks from the sky and impacts near you. You suffer two damage. In the crater, you find a small, green, glowing rock that hurts to look at. Wrapping it as thoroughly as you can in spare cloth, you look for a way to dispose of it. You remove two doom from your space and gain one remnant."
        , Seq [damage 2, here 2, remnants 1]
        )
      ,
        ( (3, Nothing)
        , "The disassembled cabinet has instructions, but they're written as a series of cryptic and frustrating pictures (will −1). If you pass, you finish the cabinet, which proves to be a magic door that returns you to your own time; you remove two doom from your space and gain one remnant."
        , pass Will (-1) (Seq [here 2, remnants 1])
        )
      ]
  , anomaly
      3
      [
        ( (0, Just 1)
        , "\"Alef, Bet, Gimel...\" The crone walks you through the first lesson (lore). If you pass, you grasp the underlying power of language to define reality; you remove one doom from any space in your neighborhood. If you fail, her teachings are complete gibberish; you suffer one horror."
        , Test Lore 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "You chase a robed figure through yet another dark alley that looks just like the others (will −1). If you pass, you stop and wait for the loop to repeat to capture the target; you remove two doom from your space and gain one remnant. If you fail, the twisting corridors are never-ending; you become delayed."
        , Test Will (-1) (Seq [here 2, remnants 1]) BecomeDelayed
        )
      ,
        ( (3, Nothing)
        , "You can't see the shadowy figure's face nor discern their gender, but they are offering you a solution to your problem in exchange for a future favor. \"I understand much, and ask little,\" they say. You may gain a DARK PACT condition to remove three doom from your space and gain one remnant."
        , mayPay (CostCondition "DARK PACT") (Seq [here 3, remnants 1])
        )
      ]
  , anomaly
      4
      [
        ( (0, Just 1)
        , "You find yourself thinking of choices you've made, your successes and regrets (will). If you pass, you recall a mistake that you made in the past and race to correct it as time bends; you remove one doom from any space in your neighborhood. If you fail, you see your worst moments; you suffer one horror."
        , Test Will 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "This book contains details of events throughout history, but the longer you read, the less sense the story makes (lore). If you pass, you realize that the events are laid out in reverse, and you read the book beginning with the last entry and ending with the first; you remove one doom from your space."
        , pass Lore 0 (here 1)
        )
      ,
        ( (3, Nothing)
        , "A gleaming gateway reveals a coastal plain rich with pine trees and enormous, lizard-like beasts. You attempt to close the portal (lore −2). If you pass, you seal the gate; you remove three doom from your space and gain one remnant. If you fail, a beast takes a bite out of your arm; you suffer two damage."
        , Test Lore (-2) (Seq [here 3, remnants 1]) (damage 2)
        )
      ]
  , anomaly
      5
      [
        ( (0, Just 1)
        , "You see yourself in the past, about to make a terrible mistake (will). If you pass, you grit your teeth and let it happen so as not to create a paradox; you remove one doom from any space in your neighborhood. If you fail, the other you thinks you're a doppelganger and attacks you; you suffer two horror."
        , Test Will 0 (nearby 1) (horror 2)
        )
      ,
        ( (2, Just 2)
        , "A gleaming gateway reveals an Arkham with tall buildings and fascist iconography, so you try to seal the gate (lore −1). If you pass, you seal the gate and are forewarned; you remove two doom from your space and gain one remnant. If you fail, you realize this future is all too likely; you suffer two horror."
        , Test Lore (-1) (Seq [here 2, remnants 1]) (horror 2)
        )
      ,
        ( (3, Nothing)
        , "You stand in the ruin of your familiar city as the skies boil with chaos (will −1). If you pass, you still have a chance to return to your own time and prevent this future; you remove two doom from your space and gain one remnant. If you fail, despair takes hold of you; you suffer three horror."
        , Test Will (-1) (Seq [here 2, remnants 1]) (horror 3)
        )
      ]
  , anomaly
      6
      [
        ( (0, Just 0)
        , "The melody is as far beyond Strauss as Strauss is beyond a sixteenth-century madrigal (will). If you pass, you hum the intriguing tune; you remove one doom from any space in your neighborhood. If you fail, you hear the monstrous harmonies screaming in your head; you suffer two horror."
        , Test Will 0 (nearby 1) (horror 2)
        )
      ,
        ( (1, Just 2)
        , "You find yourself reliving some of the darkest moments of the American Revolution. You suffer two horror. Abruptly, you find yourself alongside General Washington and knock him out of the path of a bullet just in time. You remove two doom from your space and gain one remnant."
        , Seq [horror 2, here 2, remnants 1]
        )
      ,
        ( (3, Nothing)
        , "The earth and the sky. How many generations have seen these ever-changing-yet-always-the-same vistas? You drift into quiet contemplation (will −2). If you pass, you reflect on the marvelous unpredictable chance of your current existence; you remove three doom from your space and gain one remnant."
        , pass Will (-2) (Seq [here 3, remnants 1])
        )
      ]
  , anomaly
      7
      [
        ( (0, Just 0)
        , "You find yourself in an alternate timeline, where Arkham seems so filled with joy that you think you might stay (will). If you pass, you resolve to return and save your world; you remove one doom from any space in your neighborhood. If you fail, you are torn from this paradise; you suffer two horror."
        , Test Will 0 (nearby 1) (horror 2)
        )
      ,
        ( (1, Just 2)
        , "You exchange magical attacks with a robed figure as a screaming vortex siphons up the world around (lore). If you pass, you force the cultist through the vortex and watch it fade; you remove one doom from your space. If you fail, you are flung through the portal and back in time."
        , pass Lore 0 (here 1)
        )
      ,
        ( (3, Nothing)
        , "This close to the fissure, people are walking backwards and leaves are falling up toward the trees (lore −1). If you pass, you force time to run forward again; you remove two doom from your space and gain one remnant. If you fail, you age at a crippling pace; you suffer three damage."
        , Test Lore (-1) (Seq [here 2, remnants 1]) (damage 3)
        )
      ]
  , anomaly
      8
      [
        ( (0, Just 0)
        , "Through a shimmering gateway, you see a timeline just like yours, but without the hardships you now face. You attempt to seal the gate (lore). If you pass, you remove one doom from any space in your neighborhood. If you fail, you are struck by a longing for that other world; you become CURSED."
        , Test Lore 0 (nearby 1) cursed
        )
      ,
        ( (1, Just 1)
        , "You come upon a Persian mosaic that depicts men with strange weapons fighting among broken buildings (lore). If you pass, you identify obscurely with the struggle; you remove one doom from your space. If you fail, you can make no sense of what you are seeing and turn away."
        , pass Lore 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "You try to rescue some people from a collapsing building, but fail and die, then try again. You may become delayed to keep trying. If you do, each time through, you notice more details that get you closer to your goal until you succeed; remove two doom from your space and gain one remnant."
        , mayPay CostDelayed (Seq [here 2, remnants 1])
        )
      ]
  , anomaly
      9
      [
        ( (0, Just 0)
        , "You know the words of the incantation, but your speech comes out in reverse. You try to say the words backward (will). If you pass, dloh sekat cigam ruoy; you remove one doom from any space in your neighborhood. If you fail, you can't figure out how to make a specific necessary sound."
        , pass Will 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "You find a ritual circle, no doubt left by cultists, that you could use to reverse the local disturbance, but only if you have the necessary ritual components. You may spend one remnant to remove one doom from your space and one doom from another space in your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [here 1, RemoveDoomFrom OtherSpaceInYourNeighborhood (N 1)])
        )
      ,
        ( (2, Nothing)
        , "Time flows past you at an inconceivable speed, but the area around this old book is normal (lore −1). If you pass, you find a ritual in the book; you remove two doom from your space and gain one remnant. If you fail, the book siphons your lifeforce; you suffer one damage and one horror."
        , Test Lore (-1) (Seq [here 2, remnants 1]) (SufferHarmE (N 1) (N 1))
        )
      ]
  , anomaly
      10
      [
        ( (0, Just 0)
        , "The boys in the sandlot playing stickball seem to grow older by the moment (lore). If you pass, you create a ward that restores the natural flow of time; you remove one doom from any space in your neighborhood. If you fail, they age, die, and rot away before your eyes; you suffer two horror."
        , Test Lore 0 (nearby 1) (horror 2)
        )
      ,
        ( (1, Just 1)
        , "You find yourself in the future—an older you, now ancient and dying (will). If you pass, you realize this means you will survive your current troubles; you remove one doom from your space. If you fail, you can't stand to see yourself suffering; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (2, Nothing)
        , "A foreign-looking man with an Egyptian pendant and a glass machine lectures a crowd (will −2). If you pass, you sift truth from bombast; you remove three doom from your space and gain one remnant. If you fail, you are caught up in the awful spell of the man's words; you become CURSED."
        , Test Will (-2) (Seq [here 3, remnants 1]) cursed
        )
      ]
  , anomaly
      11
      [
        ( (0, Just 0)
        , "You stand in a beautiful field of wildflowers under the blue vault of the sky, uncertain if you are in the distant past or far future (will). If you pass, you tarry a while then return to save the Earth; you remove one doom from any space in your neighborhood. If you fail, you wake up as the dream evaporates."
        , pass Will 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "A vortex of images from the past and what you assume is the future swirls before you. You attempt to fix the present in place (lore). If you pass, your efforts cause the storm to fade; you remove one doom from your space. If you fail, the vortex swirls faster and faster, and appears to be spreading."
        , pass Lore 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "There are slender figures dancing down the street, contorted into impossible poses and trailing colors. Somewhere, someone is playing the flute (will −1). If you pass, you see the figures for what they truly are and banish them from your mind; you remove two doom from your space and gain one remnant."
        , pass Will (-1) (Seq [here 2, remnants 1])
        )
      ]
  , anomaly
      12
      [
        ( (0, Just 0)
        , "You commandeer a dark robe and join a chanting column of cultists in an effort to sabotage their ritual (lore). If you pass, you steal their supplies and flee; you remove one doom from any space in your neighborhood. If you fail, they see through your disguise and take revenge; you suffer two damage."
        , Test Lore 0 (nearby 1) (damage 2)
        )
      ,
        ( (1, Just 1)
        , "You read of Zewditu, the Empress of Ethiopia, Tribhuvan, King of Nepal, and Hildred de Calvados Castaigne, heir to the Imperial Dynasty of America (lore). If you pass, you realize it is an account of an alternate Earth and learn how to prevent it; you remove one doom from your space."
        , pass Lore 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "The traveler looks just like you and claims to have come back in time to prevent you from making a terrible mistake (will −2). If you pass, you avert the crisis; you remove three doom from your space and gain one remnant. If you fail, you destroy the obvious doppelganger; you suffer two horror."
        , Test Will (-2) (Seq [here 3, remnants 1]) (horror 2)
        )
      ]
  ]
