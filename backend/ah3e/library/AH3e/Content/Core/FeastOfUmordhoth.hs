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
    , eventCards = []
    , setAside = worshipers <> heldBack
    , codex = [1, 10, 11]
    , anomalySet = Nothing
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = archive <> hunts <> [umordhoth]

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
