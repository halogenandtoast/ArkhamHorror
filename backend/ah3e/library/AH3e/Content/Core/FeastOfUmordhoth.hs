{- | Feast of Umôrdhoth.

The ghouls beneath Arkham are feeding, and their worshipers cover the tracks.
Clues on the sheet bring Lita Chantler and the hunt for the worshipers (card 10),
doom on the sheet brings Umôrdhoth itself (card 11), and the markers the
worshipers leave behind are what weakens it (cards 12 and 18).
-}
module AH3e.Content.Core.FeastOfUmordhoth (code, scenario, cards) where

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
code = "feast-of-umordhoth"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

-- | The monsters the sheet calls the worshipers of Umôrdhoth, held back at setup.
worshipers :: [CardCode]
worshipers =
  ["alma-hill", "billy-cooper", "herman-collins", "masked-hunter", "ruth-turner", "wolf-man-drew"]

{- | Cards 13-17 wait in the archive until card 10 shuffles them into the
neighborhood decks, and card 19 waits there until card 11 spawns it.
-}
heldBack :: [CardCode]
heldBack = [CardCode ("feast-" <> tshow n) | n <- [13 .. 17 :: Int]] <> ["feast-19"]

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Feast of Umôrdhoth"
    , expansion = CoreSet
    , startingSpace = spaceIdFor "General Store"
    , reckoningText = "Place one doom on this sheet."
    , reckoning = DoomOnSheet (N 1)
    , setupMap =
        buildMap
          [nb "Downtown", nb "Easttown", nb "Rivertown", nb "Uptown", nb "Southside"]
          [ StreetDef (nb "Downtown") SideRight (nb "Easttown") Residential
          , StreetDef (nb "Downtown") BottomRight (nb "Rivertown") Bridge
          , StreetDef (nb "Easttown") BottomLeft (nb "Rivertown") Bridge
          , StreetDef (nb "Rivertown") BottomLeft (nb "Uptown") Scenic
          , StreetDef (nb "Rivertown") BottomRight (nb "Southside") Residential
          , StreetDef (nb "Uptown") SideRight (nb "Southside") Scenic
          ]
    , monsters =
        [ ("abyssal-servant", 1)
        , ("corpse-taker", 1)
        , ("eyeless-watcher", 1)
        , ("hooded-stalker", 2)
        , ("masked-ones", 1)
        , -- every ghoul monster
          ("creeping-ghoul", 1)
        , ("flesh-eater", 2)
        , ("ghoul-acolyte", 2)
        , ("ghoul-priest", 1)
        , ("nightmarish-fiend", 1)
        , ("vicious-glutton", 2)
        ]
    , startingMonsters =
        [("vicious-glutton", spaceIdFor "Hangman's Hill"), ("hooded-stalker", spaceIdFor "Graveyard")]
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
          ["La Bella Luna", "Police Station", "Graveyard", "Hangman's Hill", "Historical Society"]
    , eventCards = [CardCode ("feast-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , setAside = worshipers <> heldBack
    , codex = [1, 10, 11]
    , anomalySet = Nothing
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = archive <> hunts <> events <> [umordhoth]

archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n title front back =
  CardDef
    (CardCode ("feast-" <> tshow n))
    title
    CoreSet
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

archive :: [CardDef]
archive =
  [ archiveCard
      1
      "Outbreak"
      "If a space has four or more doom, remove three doom from that space. Then place one doom in each other space in that neighborhood and one doom on the scenario sheet."
      Nothing
  , archiveCard
      10
      "Fresh Meat"
      "When there are three or more clues on the scenario sheet, discard all clues from the scenario sheet and flip this card."
      ( Just
          "The leader gains Lita Chantler. Spawn the set-aside masked hunter engaged with the leader. Take cards 13 through 17 from the archive. Shuffle each card into the top two cards of the corresponding neighborhood deck. Place one marker facedown in each neighborhood. Add card 12 to the codex and return this card to the archive."
      )
  , archiveCard
      11
      "The Hunger Below"
      "When there is eight or more doom on the scenario sheet, flip this card."
      ( Just
          "If card 10 is still in the codex, flip that card before continuing. Take card 19 and spawn it at Hangman's Hill. Add card 18 to the codex and return this card to the archive."
      )
  , archiveCard
      12
      "False Faces"
      "After a worshiper of Umôrdhoth spawns, discard the marker in its neighborhood. After a worshiper is defeated, return that worshiper to the game box and place one marker on the scenario sheet. After the fifth marker is placed on the scenario sheet, if card 11 is still in the codex, flip that card."
      (Just "Investigators win the game!")
  , archiveCard
      18
      "The Feast of Ghouls"
      "The Umôrdhoth epic monster's health is reduced by two for each marker on the scenario sheet. After the Umôrdhoth epic monster is defeated, flip card 12. When there is fifteen or more doom on the scenario sheet, flip this card."
      (Just "Investigators lose the game.")
  ]

{- | Card 19. Epic, so it goes back to the archive rather than the monster deck when
it leaves play, and nothing can hide from it. Massive engages and attacks everyone
in its space and cannot be exhausted; the clues it eats are read by card 18.
-}
umordhoth :: CardDef
umordhoth =
  CardDef
    "feast-19"
    "Umôrdhoth"
    CoreSet
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn = CustomSpaceRule "Spawned by card 11 at Hangman's Hill"
          , activation = Hunter NearestInvestigator
          , speed = 1
          , traits = ["Ancient One"]
          , health = 8
          , elite = 4
          , attackSkill = Strength
          , attackModifier = -2
          , evadeModifier = -1
          , damage = 3
          , horror = 3
          , remnant = True
          , keywords = [Massive]
          , epic = True
          , text =
              "Elite 4 (Has 4 additional health per investigator.) Massive (Umôrdhoth engages and attacks each investigator in its space. It cannot be exhausted.) As you attack Umôrdhoth, you may spend any number of clues from the scenario sheet to deal that much additional damage."
          }
    )

{- | Cards 13-17: one per neighborhood, each drawing out the worshiper who has been
covering the ghouls' tracks there. Whatever else happens, the worshiper spawns and
the card goes back to the archive, so each is drawn exactly once.
-}
hunts :: [CardDef]
hunts =
  [ hunt
      13
      "Downtown"
      [
        ( "Arkham Asylum"
        , "Charles Badoe has an unexpected opening in his schedule. You may spend $1 to recover three sanity. The opening is because one of his patients has escaped... or been set free. Spawn the set-aside \"Wolf-Man\" Drew monster engaged with you and return this card to the archive."
        , mayPay (SpendMoney 1) (mySanity 3)
        )
      ,
        ( "Independence Square"
        , "The wagons and stalls of the square's occasional market are packing up and preparing to leave. You may buy one curio item from the display. Soon, you discover why they are leaving. Spawn the set-aside \"Wolf-Man\" Drew monster engaged with you and return this card to the archive."
        , buyOne "Curio"
        )
      ,
        ( "La Bella Luna"
        , "The high-stakes poker game seems more tense than usual (observation). If you pass, you exploit the unease the other players feel; you gain $3. Whether you pass or not, a growling man leaps at you outside; spawn the set-aside Drew monster engaged with you and return this card to the archive."
        , pass Observation 0 (money 3)
        )
      ]
      "feast-drew"
  , hunt
      14
      "Easttown"
      [
        ( "Hibb's Roadhouse"
        , "Business is booming. You may spend $1 to recover three sanity. You overhear Old Man Hibbard wondering if Officer Cooper will be by to take his usual bribe \"or his pound of flesh.\" Spawn the set-aside Billy Cooper monster at the police station and return this card to the archive."
        , mayPay (SpendMoney 1) (mySanity 3)
        )
      ,
        ( "Police Station"
        , "You share your findings with Sheriff Engle (influence). If you pass, he offers you some \"off-the-books\" help; you gain one common item. Whether you pass or not, one particular cop was listening intently; spawn the set-aside Billy Cooper monster at the police station and return this card to the archive."
        , pass Influence 0 commonItem
        )
      ,
        ( "Velma's Diner"
        , "You ask for something without any meat. You may spend $1 to recover three health. Velma mentions that a police officer had been by earlier to confiscate all her pork, anyway. Spawn the set-aside Billy Cooper monster at the police station and return this card to the archive."
        , mayPay (SpendMoney 1) (myHealth 3)
        )
      ]
      "feast-cooper"
  , hunt
      15
      "Rivertown"
      [
        ( "Black Cave"
        , "Secrets are written on the walls in blood (lore). If you pass, you decipher them -- passages from Cultes des Goules; you gain one spell. Whether you pass or not, you find a discarded gravedigger's shovel; spawn the set-aside Herman Collins monster at the graveyard and return this card to the archive."
        , pass Lore 0 spell
        )
      ,
        ( "General Store"
        , "The store is open for business, but you're the only customer. You may buy any number of common items from the display. Davy Schoffner mentions that Herman Collins bought out all his meat last week. Spawn the set-aside Herman Collins monster at the graveyard and return this card to the archive."
        , buyAny "Common"
        )
      ,
        ( "Graveyard"
        , "You find a crate marked with a deer's skull (strength). If you pass, you break it open and find valuables stolen from corpses inside; you gain $3. Whether you pass or not, you spot someone fleeing from you; spawn the set-aside Herman Collins monster at the graveyard and return this card to the archive."
        , pass Strength 0 (money 3)
        )
      ]
      "feast-collins"
  , hunt
      16
      "Uptown"
      [
        ( "Hangman's Hill"
        , "Robed figures meet in the cemetery (will). If you pass, you find something they leave behind; you gain one common item. Whether you pass or not, you watch as one of them returns to the hospital; spawn the set-aside Ruth Turner monster at St. Mary's Hospital and return this card to the archive."
        , pass Will 0 commonItem
        )
      ,
        ( "St. Mary's Hospital"
        , "You finally get the exhausted Nurse Sharon's attention. You may spend $1 to recover three health. You sneak into the morgue and watch the mortician load a body into an unmarked car. Spawn the set-aside Ruth Turner monster at St. Mary's Hospital and return this card to the archive."
        , mayPay (SpendMoney 1) (myHealth 3)
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "You find a copy of Cultes des Goules. Look at the top three spells in the deck. You may spend $3 to gain one of them. Put the rest on the bottom of the deck. You're not the first person to read that book. Spawn the set-aside Ruth Turner monster at St. Mary's Hospital and return this card to the archive."
        , spells 3 (Just 1) (FlatPrice 3)
        )
      ]
      "feast-turner"
  , hunt
      17
      "Southside"
      [
        ( "Historical Society"
        , "The curator seems distracted (influence). If you pass, you get access to the collection; you gain one curio item. Whether you pass or not, historian Alma Hill is watching you with naked contempt; spawn the set-aside Alma Hill monster at the Historical Society and return this card to the archive."
        , pass Influence 0 curioItem
        )
      ,
        ( "Ma's Boarding House"
        , "Ma's got a free seat for dinner. You may spend $1 to recover three health. \"Ms. Hill keeps a room here,\" Ma explains, \"but she's been acting oddly these few weeks. Spends all her time at the museum.\" Spawn the set-aside Alma Hill monster at the Historical Society and return this card to the archive."
        , mayPay (SpendMoney 1) (myHealth 3)
        )
      ,
        ( "South Church"
        , "There's another funeral tonight, with a donation box for the family. You may spend $1 to recover three sanity. You notice a suspicious woman in the crowd. She slips out when she sees you. Spawn the set-aside Alma Hill monster at the Historical Society and return this card to the archive."
        , mayPay (SpendMoney 1) (mySanity 3)
        )
      ]
      "feast-hill"
  ]

-- | One of cards 13-17: its three encounters, each followed by the worshiper.
hunt :: Int -> Text -> [(Text, Text, Effect)] -> Text -> CardDef
hunt n hood encounters spawnKey =
  CardDef
    (CardCode ("feast-" <> tshow n))
    ("Feast of Umôrdhoth " <> tshow n <> "/17")
    CoreSet
    1
    ( NeighborhoodCard
        (nb hood)
        ( Map.fromList
            [ (spaceIdFor place, Encounter txt (Seq [eff, Custom spawnKey]))
            | (place, txt, eff) <- encounters
            ]
        )
    )

-- | Zero padded, so the event codes sort the way the cards are numbered.
pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

{- | One of the twenty-four event cards: the neighborhood it belongs to, the spaces
its doom symbols name, and an encounter for each of that neighborhood's spaces.
-}
event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("feast-event-" <> pad n))
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

-- events
events :: [CardDef]
events =
  [ event
      1
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "You speak to a patient named Joe Meir about what he found in the caves beneath Arkham where his expedition was lost. You gain one clue from your neighborhood. Seeing how this upsets you, he offers to sell you his medications. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Independence Square"
        , "The sinkhole in the square has revealed a rare object. You gain one curio item. Suddenly, the earth shakes (will). If you pass, you reach safety before dark tendrils burst out of the ground; you gain one clue from your neighborhood. If you fail, you are too frightened to move; you suffer one damage."
        , Seq [curioItem, Test Will 0 clue (damage 1)]
        )
      ,
        ( "La Bella Luna"
        , "You ask a big spender how he came into his new wealth (influence). If you pass, he confesses to selling corpses to a cult and then bribes you to \"keep it under your hat\"; you gain $3 and one clue from your neighborhood. If you fail, he calls for security to rough you up; you suffer one damage."
        , Test Influence 0 (Seq [money 3, clue]) (damage 1)
        )
      ]
  , event
      2
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "Charles Badoe counsels you about your investigation. You or an ally recovers two sanity. He wants your help finding an escaped inmate (observation). If you pass, the escaped patient tells you about the caves under Arkham; you gain one clue from your neighborhood. If you fail, you give up the search."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Independence Square"
        , "A ragged scavenger has descended upon the sinkhole that opened in the square. He tells you of the historic oddities he has found. You gain one clue from your neighborhood. You can tell that he is talking to you in the hopes of making a sale. You may buy one common item from the display."
        , Seq [clue, buyOne "Common"]
        )
      ,
        ( "La Bella Luna"
        , "A high-roller hires you to watch over him. You gain $2. You think he is being followed (observation). If you pass, you corner this shadowy figure and interrogate her about stolen corpses; you gain one clue from your neighborhood. If you fail, you're knocked out and your employer vanishes; you suffer two damage."
        , Seq [money 2, Test Observation 0 clue (damage 2)]
        )
      ]
  , event
      3
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "A janitor at the asylum can be bribed to unlock the offices. You may spend $1 to read their files about Joe Meir, a patient who explored the caves beneath Arkham. If you do, you verify that your speculations are accurate; you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ,
        ( "Independence Square"
        , "A rummage sale is held to fill the sinkhole. You may buy any number of common items from the display. You listen to the gossip about the hole (observation). If you pass, you learn about the caves beneath the city; you gain one clue from your neighborhood. If you fail, no one will speak of it."
        , Seq [buyAny "Common", pass Observation 0 clue]
        )
      ,
        ( "La Bella Luna"
        , "A stranger tells you that he needs corpses to feed the Devourer Below. You gain one clue from your neighborhood. You try to convince him that you can provide them (influence). If you pass, he pays you the money first; you gain $3. If you fail, he wields dark magic; you become CURSED."
        , Seq [clue, Test Influence 0 (money 3) cursed]
        )
      ]
  , event
      4
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "You ask to see Joe Meir, a patient who spoke about the caves below Arkham. The clerk looks at you with concern. \"There's no other reason you're here?\" You may spend $1 for you or an ally to recover three sanity. She then asks an orderly to take you to Meir. You gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (sanity 3), clue]
        )
      ,
        ( "Independence Square"
        , "The ground collapses and you tumble into a sunken pit. You try to stay calm and dig your way out (will). If you pass, you find a long-buried object as you emerge; you gain one curio item and one clue from your neighborhood. If you fail, you nearly suffocate before being rescued; you suffer one damage."
        , Test Will 0 (Seq [curioItem, clue]) (damage 1)
        )
      ,
        ( "La Bella Luna"
        , "Something in the club stinks of death. You search for the source of the smell (observation). If you pass, you find cadavers hidden in a cooler and report it to the police for a reward; you gain $3 and one clue from your neighborhood. If you fail, the stench befouls your spirit; you become CURSED."
        , Test Observation 0 (Seq [money 3, clue]) cursed
        )
      ]
  , event
      5
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "Patrons are upset about grave robberies and are recounting stories of their departed relatives. You gain one clue from your neighborhood. If you buy a few drinks for guests, they begin to share cherished memories that comfort you. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Police Station"
        , "A pile of evidence has been confiscated from a ring of grave robbers. You attempt to stealthily acquire their findings (observation). If you pass, you gain one common item and one clue from your neighborhood. If you fail, you are caught and questioned extensively; you become delayed."
        , Test Observation 0 (Seq [commonItem, clue]) delayed
        )
      ,
        ( "Velma's Diner"
        , "The food smells foul but it is surprisingly invigorating. You or an ally recovers two health. You try to identify the source of the smell (observation). If you pass, the scent is coming from the water pipes; you gain one clue from your neighborhood. If you fail, you can't find the source and decide to leave."
        , Seq [health 2, pass Observation 0 clue]
        )
      ]
  , event
      6
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "Old Man Hibbard seems knowledgeable, but he's only got time to pour. You may spend $1 to order a drink and get him talking. If you do, he keeps the drinks flowing while he discusses the old caves beneath Arkham; you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ,
        ( "Police Station"
        , "A member of a corpse-stealing conspiracy confesses. You gain one clue from your neighborhood. You tell him that you can contact his friends (influence). If you pass, he gives you something as thanks; you gain one common item. If you fail, he says you will die screaming; you suffer one horror."
        , Seq [clue, Test Influence 0 commonItem (horror 1)]
        )
      ,
        ( "Velma's Diner"
        , "You may spend $1 for you or an ally to recover three health. Patrons comment that the meat pie is especially delicious today (observation). If you pass, you find a finger in your pie; you gain one clue from your neighborhood. If you fail, it really is quite tasty; you become CURSED."
        , Seq [mayPay (SpendMoney 1) (health 3), Test Observation 0 clue cursed]
        )
      ]
  , event
      7
      "Easttown"
      ["Hibb's Roadhouse", "Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "Patrons inundate you with stories of how you inspired them to fight the threat beneath Arkham. You or an ally recovers two sanity. You listen to each story (observation). If you pass, their tales contain vital details; you gain one clue from your neighborhood. If you fail, they talk endlessly; you become delayed."
        , Seq [sanity 2, Test Observation 0 clue delayed]
        )
      ,
        ( "Police Station"
        , "An occultist bribes you to exonerate him. You gain one common item with value three or less. You examine the case file (observation). If you pass, you identify the real grave robbers; you gain one clue from your neighborhood. If you fail, you find him hanged in his cell; you become CURSED."
        , Seq [GainE (AnItemValued (Just "Common") (AtMost 3)), Test Observation 0 clue cursed]
        )
      ,
        ( "Velma's Diner"
        , "\"Glad you stopped by,\" says Velma. \"Customers keep complaining the water tastes of pennies.\" You gain one clue from your neighborhood. Given that she has so few other patrons, Velma seems keen for you to stick around and order something. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ]
  , event
      8
      "Easttown"
      ["Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "The piano player will play your favorite tune to cheer your heart if you tip. You may spend $1 for you or an ally to recover three sanity. Later in the evening, he sings an old tune about a gentleman who serves an unsavory feast in his basement. You gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (sanity 3), clue]
        )
      ,
        ( "Police Station"
        , "You interrogate an accused grave robber (influence). If you pass, the police use your information to recover stolen bodies and their possessions; you gain one common item and one clue from your neighborhood. If you fail, he tells you only tales of his dark work; you suffer one horror."
        , Test Influence 0 (Seq [commonItem, clue]) (horror 1)
        )
      ,
        ( "Velma's Diner"
        , "For a little compensation, Velma will let you examine the diner's plumbing. You may spend $1 to pull apart the sink. If you do, Velma brings you meatloaf and you find strange dark tendrils in the pipes; you or an ally recovers three health and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 3, clue])
        )
      ]
  , event
      9
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "You find a crude altar dedicated to a ghoul deity. You gain one clue from your neighborhood. The stone is covered in alien runes (lore). If you pass, you translate a language only known in the Underworld; you gain one spell. If you fail, ghouls pounce before you finish; you suffer two damage."
        , Seq [clue, Test Lore 0 spell (damage 2)]
        )
      ,
        ( "General Store"
        , "Davy Schoffner hands you a parcel and says, \"Take it.\" You gain one common item. You try to see who originally paid for the package (observation). If you pass, you read \"Joe Meir\" on the order; you gain one clue from your neighborhood. If you fail, you find Davy's reticence disturbing; you suffer one horror."
        , Seq [commonItem, Test Observation 0 clue (horror 1)]
        )
      ,
        ( "Graveyard"
        , "Ghouls have taken over the graveyard, attacking all intruders (strength). If you pass, the wounded creatures offer you money and information in exchange for mercy; you gain $3 and one clue from your neighborhood. If you fail, they wound you badly; you suffer two damage."
        , Test Strength 0 (Seq [money 3, clue]) (damage 2)
        )
      ]
  , event
      10
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "You squeeze through a narrow fissure to reach a yellowing text (will). If you pass, you inch through the crevice and find a 17th century volume about Umôrdhoth; you gain one spell and one clue from your neighborhood. If you fail, you get stuck and struggle to free yourself; you suffer one horror."
        , Test Will 0 (Seq [spell, clue]) (horror 1)
        )
      ,
        ( "General Store"
        , "Sheriff Engle is asking Davy Schoffner for a look at his records. \"It sure could help us track down those grave robbers,\" he says. You gain one clue from your neighborhood. When Davy finishes with the sheriff, he offers to help you. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "Two ghouls are fighting over a corpse, ignoring the money that had been buried with it. You gain $2. The sight is horrific (will). If you pass, the arguing creatures discuss their plans; you gain one clue from your neighborhood. If you fail, you shut your eyes in terror; you suffer one horror."
        , Seq [money 2, Test Will 0 clue (horror 1)]
        )
      ]
  , event
      11
      "Rivertown"
      ["General Store"]
      [
        ( "Black Cave"
        , "A pained voice in the dark whispers in a foreign language (lore). If you pass, you translate the horrid truths behind the rituals of ghouls; you gain one spell and one clue from your neighborhood. If you fail, you linger as the voice becomes louder and more angry; you suffer two horror."
        , Test Lore 0 (Seq [spell, clue]) (horror 2)
        )
      ,
        ( "General Store"
        , "Davy Schoffner says that the expedition into the caves ordered a lot of equipment that they didn't pick up. You may buy one common item from the display for half price (rounded up). If you do, he outlines what the expedition hoped to accomplish; you gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Common") True (Just 1) clue
        )
      ,
        ( "Graveyard"
        , "You spot a collapsed tunnel at the bottom of an open grave. You gain one clue from your neighborhood. You climb down and begin to burrow through the dirt (strength). If you pass, you find long-buried treasure; you gain $3. If you fail, hours of effort yield no results; you become delayed."
        , Seq [clue, Test Strength 0 (money 3) delayed]
        )
      ]
  , event
      12
      "Rivertown"
      ["General Store", "Graveyard"]
      [
        ( "Black Cave"
        , "Inky tendrils creep through the caves, evidence of otherworldly influence. You gain one clue from your neighborhood. The darkness covers you (will). If you pass, it vanishes, leaving behind an object; you gain one curio item. If you fail, the darkness seeps into your being; you become CURSED."
        , Seq [clue, Test Will 0 curioItem cursed]
        )
      ,
        ( "General Store"
        , "Davy turns away a fence trying to pawn her wares. You may spend $3 to buy what she offers. If you do, you discover she is a grave robber who has been selling corpses to a strange cult; you gain one common item with value four or less and one clue from your neighborhood."
        , mayPay (SpendMoney 3) (Seq [GainE (AnItemValued (Just "Common") (AtMost 4)), clue])
        )
      ,
        ( "Graveyard"
        , "A ghoul catches you but only wants to talk about its old life (will). If you pass, it is grateful to you for listening and offers you what treasure and information it can; you gain $3 and one clue from your neighborhood. If you fail, its bleak existence is too awful; you suffer one horror."
        , Test Will 0 (Seq [money 3, clue]) (horror 1)
        )
      ]
  , event
      13
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The lost notes and equipment from a failed expedition litter the caves. You gain one curio item. The notes include odd symbols (lore). If you pass, you uncover a hidden history of these caves; you gain one clue from your neighborhood."
        , Seq [curioItem, pass Lore 0 clue]
        )
      ,
        ( "General Store"
        , "The store is busy, but as a valued customer, you receive assistance before others. You may buy any number of common items from the display. As you shop, you hear plans to explore the complex system of caves beneath Arkham. You gain one clue from your neighborhood."
        , Seq [buyAny "Common", clue]
        )
      ,
        ( "Graveyard"
        , "You fall into an open grave and struggle to climb back out (strength)! If you pass, you discover the dismembered remains of ghouls embedded in the dirt walls; you gain one remnant and one clue from your neighborhood. If you fail, the effort is exhausting; you suffer one damage."
        , Test Strength 0 (Seq [remnants 1, clue]) (damage 1)
        )
      ]
  , event
      14
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The foul cavern is filled with half-eaten corpses (will). If you pass, you search the bodies and determine which cemetery they were taken from; you gain one curio item and one clue from your neighborhood. If you fail, you run screaming through the dark; you suffer one horror."
        , Test Will 0 (Seq [curioItem, clue]) (horror 1)
        )
      ,
        ( "General Store"
        , "You may buy any number of common items from the display. You secretly look through the store's ledger (observation). If you pass, someone has been buying an unusual number of shovels and canvas sacks; you gain one clue from your neighborhood. If you fail, Davy notices you and shoos you away."
        , Seq [buyAny "Common", pass Observation 0 clue]
        )
      ,
        ( "Graveyard"
        , "From your hiding place you watch the ghouls gather for a massive ritual. You gain one clue from your neighborhood. The invocation of their god is abhorrent (will). If you pass, you sneak away with a souvenir; you gain one remnant. If you fail, you are traumatized by the sight; you suffer one horror."
        , Seq [clue, Test Will 0 (remnants 1) (horror 1)]
        )
      ]
  , event
      15
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "The Society has gathered records of those who disappeared into the caves. You gain one clue from your neighborhood. You ask to take some of their recovered belongings (influence). If you pass, they agree; you gain one curio item. If you fail, they say you will not live to return it; you suffer one horror."
        , Seq [clue, Test Influence 0 curioItem (horror 1)]
        )
      ,
        ( "Ma's Boarding House"
        , "Ma left out snacks! You or an ally recovers two health. You hear several guests talking in the next room (observation). If you pass, you learn families are leaving their homes due to black tendrils in the pipes; you gain one clue from your neighborhood. If you fail, it sounds dire; you suffer one horror."
        , Seq [health 2, Test Observation 0 clue (horror 1)]
        )
      ,
        ( "South Church"
        , "Donations are sought to restore disturbed graves. You may spend $1 for you or an ally to recover three sanity. You search for more information (observation). If you pass, you learn bodies are taken to the caves; you gain one clue from your neighborhood. If you fail, you waste hours; you become delayed."
        , Seq [mayPay (SpendMoney 1) (sanity 3), Test Observation 0 clue delayed]
        )
      ]
  , event
      16
      "Southside"
      ["Historical Society", "Ma's Boarding House"]
      [
        ( "Historical Society"
        , "The historians are hesitant to discuss occult issues. You may spend one remnant to prove the validity of your claims. If you do, they reveal those items and notes that have been recovered from the caves; you gain one curio item and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [curioItem, clue])
        )
      ,
        ( "Ma's Boarding House"
        , "Ma tells you that her last guest left without paying due to something horrible he saw in the graveyard. You gain one clue from your neighborhood. You know that if you settled the stranger's bill, Ma would see to it that you were well fed. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "South Church"
        , "A homeless man has come to the church to pray for a miracle. You may spend $1 to help him. If you do, he thanks you and tells you of a dream he had about fiendish creatures flying through the woods; you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ]
  , event
      17
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "You meet a stranger looking through old geographic surveys. You gain one ally. You believe even older surveys exist. You may spend one remnant to gain access to these documents. If you do, you discover a huge subterranean chamber; you gain one clue from your neighborhood."
        , Seq [ally, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Ma's Boarding House"
        , "A scientist is staying in one of Ma's rooms. You may spend $1 to join him for dinner with Ma. If you do, you eat well and talk about the unique geological history and characteristics of Arkham; you or an ally recovers three health and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 3, clue])
        )
      ,
        ( "South Church"
        , "Father Michael tells you of a dream he had about a great darkness that lies beneath Arkham. You gain one clue from your neighborhood. You can see that the priest's faith has been shaken and a kind gesture on your part would help him feel better. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ]
  , event
      18
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "After a discussion about local legends, you speak to one of the society's guests (influence). If you pass, you make a new friend with insights about the Devourer Below; you gain one ally and one clue from your neighborhood. If you fail, the speaker tells the worst possible stories; you suffer one horror."
        , Test Influence 0 (Seq [ally, clue]) (horror 1)
        )
      ,
        ( "Ma's Boarding House"
        , "Ma tells you that since guests have been few and far between, she will offer you a discount on dinner. You may spend $1 for you or an ally to recover three health. She complains of all these sinkholes and foul stenches keeping away travelers. You gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (health 3), clue]
        )
      ,
        ( "South Church"
        , "You doze off in a pew and get a badly needed nap. You or an ally recovers two sanity. In your dreams, you are hunting for Nodens (observation). If you pass, you find him and he teaches you about ghouls; you gain one clue from your neighborhood. If you fail, you sleep for hours; you become delayed."
        , Seq [sanity 2, Test Observation 0 clue delayed]
        )
      ]
  , event
      19
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "You find a wounded hunter who has been searching the woods for a strange beast. You gain one clue from your neighborhood. You try to carry him to safety (strength). If you pass, he hands you a reward; you gain one common item. If you fail, you wait until help arrives; you become delayed."
        , Seq [clue, Test Strength 0 commonItem delayed]
        )
      ,
        ( "St. Mary's Hospital"
        , "While waiting for a doctor you help yourself to medical supplies. You or an ally recovers two health. You search several rooms for patient files (observation). If you pass, you see evidence of wide-spread ghoul bites; you gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam and a customer fall silent as you enter. You may spend $3 to set them at ease to continue their conversation. If you do, they admire your purchase and talk about something affecting the ley lines beneath Arkham; you gain one spell and one clue from your neighborhood."
        , mayPay (SpendMoney 3) (Seq [spell, clue])
        )
      ]
  , event
      20
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "A nightgaunt grabs you and carries you high into the air (will). If you pass, you remain calm until it sets you down safely near its nest of collected treasures; you gain one common item and one clue from your neighborhood. If you fail, you panic and plummet to the ground below; you suffer two damage."
        , Test Will 0 (Seq [commonItem, clue]) (damage 2)
        )
      ,
        ( "St. Mary's Hospital"
        , "You see a ghoul's body brought in for examination. \"This could be the source of those infected wounds,\" notes Doctor Maheswaran. You gain one clue from your neighborhood. She notices your interest and offers to help with your injuries. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Reveal the top three spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck. Miriam Beecher talks to you while you shop (influence). If you pass, she mentions mystical reverberations emanating from beneath the earth; you gain one clue from your neighborhood."
        , Seq [spells 3 Nothing FullPrice, pass Influence 0 clue]
        )
      ]
  , event
      21
      "Uptown"
      ["Hangman's Hill", "Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "Something has dug up graves, taking the bodies and leaving long-buried objects. You gain one common item. Now the ghoul has returned (strength)! If you pass, you subdue the beast and interrogate it; you gain one clue from your neighborhood. If you fail, it rips your flesh; you suffer one damage and one horror."
        , Seq [commonItem, Test Strength 0 clue (harm 1 1)]
        )
      ,
        ( "St. Mary's Hospital"
        , "Nurse Sharon offers you information for a price. You may spend $1 to find out what she knows. If you do, she bandages you as she tells you about strange pale people who stalk and feast on their hapless prey; you or an ally recovers three health and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 3, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The ley lines are pulsing here, and you can see their faint glow everywhere. You gain one clue from your neighborhood. They seem to guide you to a particular shelf in the store. Reveal the top three spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
        , Seq [clue, spells 3 Nothing FullPrice]
        )
      ]
  , event
      22
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "The sky is filled with ghouls astride nightgaunts, arriving from worlds beyond. You gain one clue from your neighborhood. Their unearthly shadows fall upon you (will). If you pass, a beast has shed a horn; you gain one remnant. If you fail, you flee into the night; you suffer two horror."
        , Seq [clue, Test Will 0 (remnants 1) (horror 2)]
        )
      ,
        ( "St. Mary's Hospital"
        , "As you linger, Nurse Sharon asks if you need help. You may spend $1 for you or an ally to recover three health. You eavesdrop on the doctors (observation). If you pass, they discuss dead tissue found in wounds; you gain one clue from your neighborhood. If you fail, their discussion of boils and pus is revolting."
        , Seq [mayPay (SpendMoney 1) (health 3), pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The ley lines are so strong, arcane power jumps into your mind. You gain one spell. You spy on other customers (observation). If you pass, they discuss magic across Arkham; you gain one clue from your neighborhood. If you fail, they catch you snooping; you become CURSED."
        , Seq [spell, Test Observation 0 clue cursed]
        )
      ]
  , event
      23
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "A large winged creature is wounded, unable to fly. You try to catch the creature alive (strength). If you pass, you send it to the university for study; you gain one remnant and one clue from your neighborhood. If you fail, the wounded nightgaunt slashes you and escapes; you suffer one damage."
        , Test Strength 0 (Seq [remnants 1, clue]) (damage 1)
        )
      ,
        ( "St. Mary's Hospital"
        , "The waiting room is full but a little cash can get you treated quickly. You may spend $1 for you or an ally to recover three health. You notice that most of the people waiting for a doctor have suffered similar bites, all of them infected. You gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (health 3), clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher seems to be in a trance from which she cannot awaken. You may spend $3 to have medical help brought in to revive her. If you do, she gratefully offers you a reward and a lesson about the powers beneath Arkham; you gain one spell and one clue from your neighborhood."
        , mayPay (SpendMoney 3) (Seq [spell, clue])
        )
      ]
  , event
      24
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "You find a small shrine to Nodens. You gain one remnant. You are struck by a vision of the Lord of the Great Abyss (will). If you pass, he shows you how ghouls have been using nightgaunts; you gain one clue from your neighborhood. If you fail, the vision shatters your mortal mind; you suffer two horror."
        , Seq [remnants 1, Test Will 0 clue (horror 2)]
        )
      ,
        ( "St. Mary's Hospital"
        , "Doctor Mortimore does not have time for you unless you are a paying customer seeking care. You may spend $1 to pay his fee. If you do, he stitches you up and recounts all the strange bites that he has seen lately; you or an ally recovers three health and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 3, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher says swollen ley lines have enhanced the shop's offerings. You gain one clue from your neighborhood. \"We don't usually have such a wide selection,\" she says. Reveal the top four spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
        , Seq [clue, spells 4 Nothing FullPrice]
        )
      ]
  ]
