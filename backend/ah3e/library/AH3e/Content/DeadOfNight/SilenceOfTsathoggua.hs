{- | Silence of Tsathoggua.

The Mi-Go have bridged Arkham to Yuggoth, and something beneath the earth is
being called awake. Clues on the sheet buy the search for their beacons (card
54, then 55 and 56); doom on the sheet brings their augmented experiments (card
53) and finally Tsathoggua itself (cards 59, 60 and 58). The markers salvaged
from their machines are what builds the device on card 57 -- and what keeps
Tsathoggua from eating the city a tile at a time.
-}
module AH3e.Content.DeadOfNight.SilenceOfTsathoggua (code, scenario, cards) where

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
code = "silence-of-tsathoggua"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Silence of Tsathoggua"
    , expansion = DeadOfNight
    , startingSpace = spaceIdFor "Orne Library"
    , reckoningText = "If any neighborhood has two or more clues, place one doom on this sheet."
    , reckoning = Custom "sot-reckoning"
    , setupMap =
        buildMap
          [nb "Northside", nb "Merchant District", nb "Rivertown", nb "Miskatonic University", nb "Uptown"]
          [ StreetDef (nb "Northside") BottomRight (nb "Merchant District") Bridge
          , StreetDef (nb "Merchant District") SideRight (nb "Rivertown") Residential
          , StreetDef (nb "Merchant District") BottomRight (nb "Miskatonic University") Bridge
          , StreetDef (nb "Rivertown") BottomLeft (nb "Miskatonic University") Residential
          , StreetDef (nb "Miskatonic University") BottomLeft (nb "Uptown") Scenic
          ]
    , monsters =
        [ ("capricious-stalker", 1)
        , ("cerebral-extractor", 1)
        , ("eyeless-watcher", 1)
        , ("grasping-fungus", 2)
        , ("occult-ritualist", 2)
        , -- every aberration monster
          ("altered-beast", 2)
        , ("corpse-taker", 1)
        , ("crawling-one", 2)
        , ("tunneling-dhole", 1)
        , -- every formless spawn monster
          ("morphic-terror", 1)
        , ("undulating-mass", 1)
        ]
    , startingMonsters =
        [ ("grasping-fungus", spaceIdFor "Unvisited Isle")
        , ("occult-ritualist", spaceIdFor "Black Cave")
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
          ["Curiositie Shoppe", "Unvisited Isle", "Black Cave", "Science Building", "Hangman's Hill"]
    , startingMarkers = []
    , eventCards = [CardCode ("sot-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , setAside = ["sot-60"]
    , codex = [2, 53, 54]
    , anomalySet = Just "Yuggoth Emergent"
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = archive <> events <> anomalies <> [tsathoggua]

pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

nearby, here, anywhere :: Int -> Effect
nearby n = RemoveDoomFrom SpaceInYourNeighborhood (N n)
here n = RemoveDoomFrom YourSpace (N n)
anywhere n = RemoveDoomFrom AnySpace (N n)

buyOneHalfThen, buyAnyThen :: Trait -> Effect -> Effect
buyOneHalfThen t = BuyFromDisplay (Just t) HalfPrice (Just 1)
buyAnyThen t = BuyFromDisplay (Just t) FullPrice Nothing

remnant :: Effect
remnant = remnants 1

archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n title front back =
  CardDef
    (CardCode ("sot-" <> tshow n))
    title
    DeadOfNight
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

archive :: [CardDef]
archive =
  [ archiveCard
      53
      "A Sleeping City"
      "[Doom] When there is 2 or more doom on the scenario sheet, flip this card."
      ( Just
          "The Maw Opens\nSpawn one aberration monster at the Black Cave and place a white marker on that monster.\nA monster with a white marker has been augmented by the Mi-Go and gains Elite 1 (it has 1 additional health per investigator).\nAfter a monster with a white marker is defeated by an investigator in its space, move that marker to the scenario sheet.\nIf there are two or more markers on the scenario sheet, add card 57 to the codex.\n[Doom] When there is 5 or more doom on the scenario sheet, add card 59 to the codex.\n(Do not remove this card from the codex.)"
      )
  , archiveCard
      54
      "The Search"
      "[Objective] When there are two or more clues on the scenario sheet, flip this card."
      ( Just
          "Finding the Source\nTake three markers -- one green, one blue, and one red -- and randomize them facedown. Place one each facedown on the Black Cave, the Science Building, and the Unvisited Isle.\nAction: Reveal a marker at your location and resolve the effect below based on its color.\nWhen you reveal the red marker, you find no conclusive evidence; discard it.\nWhen you reveal the green marker, move it to Northside. Then take card 55 from the archive and attach it to the Northside neighborhood deck.\nWhen you reveal the blue marker, move it to Uptown. Then take card 56 from the archive and attach it to the Uptown neighborhood deck.\nWhen both the green and blue markers are revealed, discard the remaining marker and return this card to the archive."
      )
  , beacon 55 "Northside" "observation" "green"
  , beacon 56 "Uptown" "will" "blue"
  , archiveCard
      57
      "Alien Science"
      "[Objective] Action: Spend any number of clues from the scenario sheet to resolve a test using that number of dice. If you succeed, add one marker to the scenario sheet. This action may only be performed at the Science Building.\nWhen there are five markers on the scenario sheet, flip this card."
      (Just "Investigators win the game!")
  , archiveCard
      58
      "Arkham Ravaged"
      "[Objective] After Tsathoggua is defeated, flip this card and read the \"Arkham Salvaged\" effect.\n[Doom] When doom would be placed on the scenario sheet, instead move Tsathoggua and any investigators engaged with him one space toward Hangman's Hill. If you placed a marker on the scenario sheet this turn, instead discard that doom (and do not move Tsathoggua).\nWhen Tsathoggua leaves a neighborhood or street, that tile is devoured. Any tokens or monsters on that tile are discarded, and any investigators on that tile are devoured.\nAfter Tsathoggua moves to Uptown, flip this card and read the \"Arkham Devoured\" effect."
      ( Just
          "Arkham Salvaged\nInvestigators win the game!\n\nArkham Devoured\nInvestigators lose the game!"
      )
  , archiveCard
      59
      "The First Bite"
      "Return the bridge that connects the Merchant District and Miskatonic University to the game box. Any monster or investigator in that space suffers two damage and moves to an adjacent space.\nRemove one blank mythos token from the game and add one gate burst token to the mythos cup.\n[Doom] When there is 9 or more doom on the scenario sheet, flip this card."
      ( Just
          "Take card 60 (Tsathoggua epic monster) and spawn it at the Curiositie Shoppe.\nTsathoggua suffers two damage for each marker on the scenario sheet.\nSpawn one formless spawn monster at Hangman's Hill and place one white marker on that monster.\nRemove one blank mythos token from the game and add one spread doom token to the mythos cup.\nRemove all doom from the scenario sheet.\nAdd card 58 to the codex and return this card to the archive."
      )
  ]

{- | Cards 55 and 56: one beacon per neighborhood, dismantled by a test taken after
an encounter there and paid for with the sheet's clues. Each one taken apart is a
marker towards card 57's device.
-}
beacon :: Int -> Text -> Text -> Text -> CardDef
beacon n hood skill colour =
  archiveCard
    n
    ("Mi-Go Beacon - " <> hood)
    ( "After you resolve a "
        <> hood
        <> " encounter, you may test "
        <> skill
        <> ". If you pass, spend two clues from the scenario sheet to flip this card."
    )
    ( Just
        ( "Move the "
            <> colour
            <> " marker to the scenario sheet.\nIf there are two or more markers on the scenario sheet, add card 57 to the codex.\nReturn this card to the archive."
        )
    )

{- | Card 60. Spawned by the back of card 59 at the Curiositie Shoppe. Epic, so it
goes back to the archive rather than the monster deck when it leaves play; card
58 is what makes it walk, and what happens to the tiles it walks off.
-}
tsathoggua :: CardDef
tsathoggua =
  CardDef
    "sot-60"
    "Tsathoggua"
    DeadOfNight
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn = CustomSpaceRule "Spawned by card 59 at the Curiositie Shoppe"
          , activation = Lurker (Custom "sot-return-gate-token")
          , speed = 0
          , traits = ["Ancient One"]
          , health = 8
          , elite = 4
          , attackSkill = Strength
          , attackModifier = -3
          , evadeModifier = 0
          , damage = 2
          , horror = 2
          , remnant = True
          , keywords = [Massive]
          , epic = True
          , text =
              "Elite 4 (Has 4 additional health per investigator.) Massive (Tsathoggua engages and attacks each investigator in its space. It cannot be exhausted.) After Tsathoggua attacks, become delayed unless you suffer one additional horror. Lurker - Return one gate burst token to the mythos cup."
          }
    )

event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("sot-event-" <> pad n))
    ("Event " <> tshow n <> "/24")
    DeadOfNight
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
      "Merchant District"
      ["River Docks", "Unvisited Isle"]
      [
        ( "River Docks"
        , "Joey \"the Rat\" is fast asleep and doesn't notice the flying creature measuring his head with a caliper. You startle the Mi-Go easily and it flies off; gain a clue from your neighborhood. When you shake him awake, he's already eager to do business; you may spend one remnant to gain one common item."
        , Seq [clue, mayPay (SpendRemnants 1) commonItem]
        )
      ,
        ( "Tick-Tock Club"
        , "It's a slow night, so the doorman promises a free meal if you'll spend some time socializing at the club. You or an ally recovers two health. You may spend $1 to stay for a round of drinks. If you do, you notice an odd buzzing sound just before the entire band nods off mid-set. Gain a clue from your neighborhood."
        , Seq [health 2, mayPay (SpendMoney 1) clue]
        )
      ,
        ( "Unvisited Isle"
        , "A ropy tendril of black mucus hangs from the grasping branches of a withered tree (lore). If you pass, you recognize it as a piece of one of the formless creatures that prowls the city; gain one clue from your neighborhood. If you fail, it burrows into your skin when you touch it; the cold fire burns as you become CURSED."
        , Test Lore 0 clue cursed
        )
      ]
  , event
      2
      "Merchant District"
      ["River Docks"]
      [
        ( "River Docks"
        , "The man you were supposed to meet is nowhere to be found, and his rucksack lays unattended. Gain one common item. His friends arrive moments later, demanding an explanation. You may spend one remnant to show them the truth. If you do, gain one clue from your neighborhood."
        , Seq [commonItem, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Tick-Tock Club"
        , "The clocks on the walls never agree, but this time several of them are running even slower than usual (observation). If you pass, some quick calculations on your napkin find a geometric pattern to the affected timepieces; gain one clue from your neighborhood. If you fail, you tell the bartender to wind the clocks and resume drinking."
        , pass Observation 0 clue
        )
      ,
        ( "Unvisited Isle"
        , "A headless, crablike being putters around a strange metal machine, muttering to itself in an unknown tongue. You steel your nerves to watch its work (will). If you pass, you learn something about its machinery; gain one clue from your neighborhood. If you fail, you retreat from the alien creature."
        , pass Will 0 clue
        )
      ]
  , event
      3
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "A soft splash sounds in the long, quiet night (observation). If you pass, you find a body bobbing in the water, headless yet still twitching. Unable to help the man, you find a good home for his wallet. Gain $3 and a clue from your neighborhood. If you fail, the dark night remains impenetrable."
        , pass Observation 0 (Seq [money 3, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "Abner Weems is unsuccessfully trying to pay for a drink using a strange fragment of greenish metal. You take it from him and gain one remnant. You may spend $1 to share a drink with Abner. If you do, the whiskey calms your nerves as he tells you where he found the metal; gain a clue from your neighborhood and recover one sanity."
        , Seq [remnant, mayPay (SpendMoney 1) (Seq [clue, mySanity 1])]
        )
      ,
        ( "Unvisited Isle"
        , "You stifle a scream as you stumble across a hulking, formless creature devouring a robed figure. Gain one clue from your neighborhood and wait grimly until it moves on (will). If you pass, you search the remains and gain one curio. If you fail, the wet crunch of bone is too much for you; suffer one horror."
        , Seq [clue, Test Will 0 curioItem (horror 1)]
        )
      ]
  , event
      4
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "Johnny Valone is anxious to get back to Boston, but his partners want to know what's fouling up their business in Arkham. You may spend one remnant to show him. If you do, he thanks you with a gift and tells you about the things he's seen; gain a common item and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [commonItem, clue])
        )
      ,
        ( "Tick-Tock Club"
        , "The ticking clocks all over the walls reassure you that time is still passing. You may spend $1 to stay for a drink and chat with the other patrons. If you do, you or an ally recovers two sanity, while another patron tells you about the \"odd black cow\" she spotted near the train station; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 2, clue])
        )
      ,
        ( "Unvisited Isle"
        , "As you scrape the tar-like mess off of your shoe, you realize this is a part of one of those creatures. Gain one remnant. You study the uneven tracks it left in the mud (observation). If you pass, you follow the tracks to a clearing that affords a marvelous view of a green moon you've never seen before; gain one clue from your neighborhood."
        , Seq [remnant, pass Observation 0 clue]
        )
      ]
  , event
      5
      "Miskatonic University"
      ["Observatory"]
      [
        ( "Observatory"
        , "A pair of extra moons hangs in the air, peeking out from behind the heavy black clouds. Gain one clue from your neighborhood and study the city below you (observation). If you pass, you note the location of a handful of alien devices revealed by the light of the moons; you may remove one doom from any space."
        , Seq [clue, pass Observation 0 (anywhere 1)]
        )
      ,
        ( "Orne Library"
        , "You've seen the markings on this alien cylinder before, but in which book? You search the shelf carefully (observation). If you pass, you locate a diagram of bizarre theoretical machinery, drawn by a young woman who claims that she was abducted by aliens called the Mi-Go; gain one clue from your neighborhood. If you fail, your search is fruitless."
        , pass Observation 0 clue
        )
      ,
        ( "Science Building"
        , "Thin tendrils of black smoke coil from an unattended Bunsen burner. With no sign of the experimenter, you remove a beaker full of bubbling black fluid from the heat (observation). If you pass, you safely decant the solution; gain one clue from your neighborhood. If you fail, the glass cracks and poison gas fills the room; suffer two damage."
        , Test Observation 0 clue (damage 2)
        )
      ]
  , event
      6
      "Miskatonic University"
      ["Observatory"]
      [
        ( "Observatory"
        , "You find a strange metal device connected to the radio telemetry machine. Gain one clue from your neighborhood and try to open the metal case (observation). If you pass, you dismantle the device and collect the organic components within it; gain a remnant. If you fail, you cannot find any seams on the casing."
        , Seq [clue, pass Observation 0 remnant]
        )
      ,
        ( "Orne Library"
        , "The card catalogue is strewn around a sleeping intern, caught in the middle of his filing by an unnatural slumber. You pick through the mess to try to find the book you need anyway (lore). If you pass, you also find an arcane text that is missing from the catalogue; gain one clue from your neighborhood and one spell. If you fail, you give up in frustration."
        , pass Lore 0 (Seq [clue, spell])
        )
      ,
        ( "Science Building"
        , "An anxious student working late requests your help after something made off with her lab work. You may spend one remnant to help her finish the project on time. If you do, she gratefully gives you $2 and tells you all about the black oily beast that ate her homework; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 2, clue])
        )
      ]
  , event
      7
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "At the top of Crane Hill, a startled Mi-Go flies off, dropping an unusual hand tool. Gain one remnant and study the peculiar device (lore). If you succeed, you recognize that the symbols on the handle are actually alien text; gain one clue from your neighborhood."
        , Seq [remnant, pass Lore 0 clue]
        )
      ,
        ( "Orne Library"
        , "When you awake to the orange glow of the reading lamp in your study carrel, you find an odd folio open in front of you. Gain one spell; the contents have crept unbidden into your mind (lore). If you pass, you realize the hypnotic electric hum suffusing the room is emanating from an alien device; gain one clue from your neighborhood."
        , Seq [spell, pass Lore 0 clue]
        )
      ,
        ( "Science Building"
        , "An associate professor from the alchemy department claims she can help you in exchange for alien matter; you may spend one remnant and breathe in the incense she mixes. If you do, you see visions of huge black stone buildings under a dim and distant sun. Gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ]
  , event
      8
      "Miskatonic University"
      ["Orne Library", "Science Building"]
      [
        ( "Observatory"
        , "The device you found is inscribed with an unfamiliar script, and the buzzing sound is getting louder (lore). If you pass, you decipher the script and deactivate the machine; gain one clue in your neighborhood. If you fail, the buzzing stops abruptly on its own; place one doom in your space."
        , Test Lore 0 clue (PlaceDoomAt YourSpace (N 1))
        )
      ,
        ( "Orne Library"
        , "Professor Krosnowski sits at a reading table, her tea long tepid, reciting a poem in her sleep. Gain one clue in your neighborhood and try to analyze the poem's syntax (lore). If you pass, you realize that transposing a few words transforms the poem into potent magic; gain one spell. If you fail, you find the poem to be deeply unsettling, but mundane."
        , Seq [clue, pass Lore 0 spell]
        )
      ,
        ( "Science Building"
        , "An eager student shows you into the morgue at the College of Medicine, where several cadavers have had their brains removed. Gain one clue from your neighborhood. \"I'll bet a dollar that you don't know what happened here!\" You may spend one remnant to show him the truth. If you do, he hands you $2 and locks himself in a closet."
        , Seq [clue, mayPay (SpendRemnants 1) (money 2)]
        )
      ]
  , event
      9
      "Miskatonic University"
      ["Science Building"]
      [
        ( "Observatory"
        , "From atop Crane Hill, you spot the still surface of a lake, like a black mirror in the endless gloam of twilight (observation). If you pass, you realize there isn't supposed to be a lake on the east campus and gain one clue from your neighborhood. If you fail, you think it looks very pretty."
        , pass Observation 0 clue
        )
      ,
        ( "Orne Library"
        , "Most of the Ruggles Rare Book Room is unnaturally still, including the sleeping form of Abigail Foreman. Gain one clue from your neighborhood and study her books (lore). If you pass, you discover a secret in the tome she's reading; gain one spell. If you fail, you feel yourself sinking into torpor as well; become delayed."
        , Seq [clue, Test Lore 0 spell delayed]
        )
      ,
        ( "Science Building"
        , "You participate in a sleep study at the College of Medicine and gain $3. The experimenter nods off in the middle of the session, giving you a chance to review his notes (observation). If you pass, you confirm that the people all over Arkham are sleeping extraordinarily long hours; gain one clue from your neighborhood."
        , Seq [money 3, pass Observation 0 clue]
        )
      ]
  , event
      10
      "Northside"
      ["Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "Doyle Jeffries hires you to review a story about missing persons before the morning edition goes to print (observation). If you pass, you notice a pattern in the disappearances; gain one clue from your neighborhood. If you fail, you miss a major typo. Whether you pass or not, gain $3 for your time."
        , Seq [pass Observation 0 clue, money 3]
        )
      ,
        ( "Curiositie Shoppe"
        , "You browse the shelves looking for something useful in your fight against the creatures that prowl the long night; you may buy any number of curios from the display. If you buy anything, Oliver shows you the remains of a small, black, formless spawn that another customer brought in; gain one clue from your neighborhood."
        , buyAnyThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "You try desperately to rouse a sleeping traveler before the swarming black shapes fall upon you both (influence). If you pass, the two of you make your escape; gain an ally and one clue from your neighborhood. If you fail, you are forced to leave them behind and take cover as the creatures swarm over the platform; become CURSED."
        , Test Influence 0 (Seq [ally, clue]) cursed
        )
      ]
  , event
      11
      "Northside"
      ["Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "The editor of the Advertiser reports that his staff didn't show up to work today, and he can barely stay awake. Gain one clue from your neighborhood. You may spend one remnant to help him research a story about the malaise afflicting Arkham. If you do, he pays you $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "\"Get this thing out of here!\" Oliver Thomas thrusts an odd metal cylinder into your hands. \"That infernal hum is making my teeth rattle.\" Gain one clue from your neighborhood as you dispose of it for him. Happy to be rid of it, the shopkeep offers you a bargain. You may purchase one curio for half price (rounded up)."
        , Seq [clue, buyOneHalf "Curio"]
        )
      ,
        ( "Train Station"
        , "The metal device in front of you is humming softly, at a frequency that you feel deep in your bones. The closer you get to it, the heavier your limbs become (will). If you pass, you shut down the machine and study it carefully; gain one clue from your neighborhood. If you fail, you pass out; when you awake, the machine is gone."
        , pass Will 0 clue
        )
      ]
  , event
      12
      "Northside"
      ["Arkham Advertiser"]
      [
        ( "Arkham Advertiser"
        , "Minnie Klein, staff writer at the Advertiser, says she'll scratch your back if you scratch hers. You may give her a remnant to help her with a story. If you do, she tells you about how Deputy Dingby fell asleep on duty and woke up handcuffed to a park bench; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ,
        ( "Curiositie Shoppe"
        , "You help Oliver Thomas chase a flying creature with crablike claws out of the shop; gain one clue from your neighborhood (observation). If you pass, you spot and chase off a second Mi-Go preparing to seize his cat, The Baron, and the relieved shopkeep presses an item into your hands; gain one curio. If you fail, his unimpressed cat crawls under an old wardrobe."
        , Seq [clue, pass Observation 0 curioItem]
        )
      ,
        ( "Train Station"
        , "There is an odd glass cylinder mounted on a pair of metal, spidery legs; gain one clue from your neighborhood. It speaks to you (will). If you pass, you learn it used to have a human body; MILES CROWN joins you. If you fail, you spot the human brain sloshing around inside it and flee before it can grab you; suffer two horror."
        , Seq [clue, Test Will 0 (named "MILES CROWN") (horror 2)]
        )
      ]
  , event
      13
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "There's an odd sound coming from the printing press (observation). If you pass, you stop a Mi-Go from tampering with it; gain one clue from your neighborhood. If you fail, the strange text on the evening edition of the paper fills you with dread; become CURSED."
        , Test Observation 0 clue cursed
        )
      ,
        ( "Curiositie Shoppe"
        , "You receive a discount on the object because it is covered in tarry black sputum. You may purchase one curio for half price (rounded up). If you purchase something, you are able to study the slime further; it wriggles away from you and you gain one clue from your neighborhood."
        , buyOneHalfThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "You reach the platform just as a harried passenger smashes a small crawling beast with a stanchion; it bears a resemblance to the larger creatures that stalk the streets. Gain one clue from your neighborhood. You attempt to calm the frantic passenger (influence). If you pass, they set down their makeshift weapon; gain one ally."
        , Seq [clue, pass Influence 0 ally]
        )
      ]
  , event
      14
      "Rivertown"
      ["Black Cave", "Black Cave"]
      [
        ( "Black Cave"
        , "Stumbling upon an altar slick with blood and black effluvium, you gain one spell from the cultists' wretched tome before you study the remains of their ritual (lore). If you pass, you find a foul-smelling effigy of a large and gaping mouth; gain one clue from your neighborhood."
        , Seq [spell, pass Lore 0 clue]
        )
      ,
        ( "General Store"
        , "Mr. Hatle jabbers on about the small black animal he found under his porch, claiming that it keeps growing new faces and eats everything except metal; gain one clue from your neighborhood. Davy Schoffner rolls his eyes and tells you the old man probably just caught another raccoon. You may buy any number of common items."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "The dark violet miasma spreading through the gravestones lulls you to rest your head (will). If you pass, you resist the effect and move away from the area; gain one clue from your neighborhood. If you fail, you curl up against the sculpture of a winged satyr and awaken to a ghoul tearing at your leg; suffer two damage."
        , Test Will 0 clue (damage 2)
        )
      ]
  , event
      15
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "The slowly dripping water echoes through the cave; the rhythm is hypnotic (will). If you pass, you power through the soporific trance to find the remains of some foul ritual; gain one curio and one clue from your neighborhood. If you fail, you dream of bony, spined tongues in gaping mouths; suffer two horror."
        , Test Will 0 (Seq [curioItem, clue]) (horror 2)
        )
      ,
        ( "General Store"
        , "You know something is terribly wrong the minute you step into Schoffner's general store (observation). If you pass, you realize after a long, tense pause that Mr. Hatle isn't talking someone's ear off over a game of checkers; gain one clue from your neighborhood as you notice the old man sleeping in his chair. If you fail, you back slowly out of the store."
        , pass Observation 0 clue
        )
      ,
        ( "Graveyard"
        , "\"Help!\" The man waves frantically with a rod of greenish metal, his torso emerging from the inky blackness of an open grave (strength). If you pass, you pull him out and he explains that the bar is made of tok'l, a metal used by the Mi-Go; gain one clue from your neighborhood and one remnant. If you fail, something in the hole engulfs him."
        , pass Strength 0 (Seq [clue, remnant])
        )
      ]
  , event
      16
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "The robed figure still twitches, even clamped lifelessly into the alien device; gain one clue from your neighborhood. His lips move silently as the filaments of the machine hum in the air (will). If you pass, you understand the words his trapped consciousness is trying to speak; gain one spell."
        , Seq [clue, pass Will 0 spell]
        )
      ,
        ( "General Store"
        , "Nathan is running the store, and seems eager to please the customers in Mr. Schoffner's absence; you may buy one common item from the display at half price (rounded up). If you do, he explains that the shop's owner is asleep in the back room, and is finally letting Nathan work the register; gain one clue from your neighborhood."
        , buyOneHalfThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "A splatter of sooty ooze seeps out from underneath a toppled tombstone (strength). If you pass, you shift the heavy granite slab to the side and scrape up a sample of the smashed creature; gain one remnant. Whether you pass or not, after a short time the largest blob begins to move toward you; gain one clue from your neighborhood."
        , Seq [pass Strength 0 remnant, clue]
        )
      ]
  , event
      17
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The moist walls of the cave glisten with some kind of black slime that quivers when you get near to it (lore). If you pass, you recall a few words from a soothing charm used by Hellenic shepherds to calm their animals; when you recite them, the ooze goes still and you gain one clue from your neighborhood."
        , pass Lore 0 clue
        )
      ,
        ( "General Store"
        , "On the street outside, Nathan the delivery boy claims he's being followed by a big bug. When you scan the dark night around you, you don't see anything, but escort the teenager into the store and lock the door anyway. Gain one clue from your neighborhood. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "The partially digested body reeks of sulfur. Gain one clue from your neighborhood. As you lean in to investigate, a sticky mass of oily slime quivers and leaps toward your open mouth (strength). If you pass, you bat it away with the corpse's handbag; gain $2. If you fail, it squirms down your throat before you can scream; become CURSED."
        , Seq [clue, Test Strength 0 (money 2) cursed]
        )
      ]
  , event
      18
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The chanting echoes through the cold dark air of the cave until the words settle in your mind. Gain one spell. You try to locate the speaker (observation). If you pass, you find a basin of viscous, soot-colored liquid that reaches an arm toward you when your light draws near; gain one clue from your neighborhood."
        , Seq [spell, pass Observation 0 clue]
        )
      ,
        ( "General Store"
        , "Ryan Dean stops you at the door of the General Store. \"You wanna go in there and browse a bunch of over-priced junk, or do you want a bargain that's out of this world?\" You may spend $2 for the sack he waves in your face. If you do, you find some kind of sophisticated metal tool in the bag; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) clue
        )
      ,
        ( "Graveyard"
        , "Robed figures scatter into the darkness as the beam of your flashlight sweeps through the tombstones. The remains of a ritual are strewn across a blood-stained crypt (observation). If you pass, you spot a scrawled manuscript referencing the \"co-termination of Yuggoth;\" gain one clue from your neighborhood."
        , pass Observation 0 clue
        )
      ]
  , event
      19
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "The partially digested remains nestled among the witchweed still drip with a foul-smelling black ichor. Gain one clue from your neighborhood as you study the body (will). If you pass, you collect a sample of the sputum; gain one remnant. If you fail, you recognize the dead woman; suffer one horror."
        , Seq [clue, Test Will 0 remnant (horror 1)]
        )
      ,
        ( "St. Mary's Hospital"
        , "There's a free bed in one of the wards. You may spend $2 for you or an ally to recover two health. If you do, the one-armed man in the next bed tells you how he tried to hide from the beast that chased him through the night, but it just flowed right under his locked door and grabbed him by the wrist; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [health 2, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher claims to know a particularly effective ancient ritual used to prevent portals from opening into other worlds; you may spend $3 for the ritual supplies she offers. If you do, you gain one clue from your neighborhood and may remove one doom from any space."
        , mayPay (SpendMoney 3) (Seq [clue, anywhere 1])
        )
      ]
  , event
      20
      "Uptown"
      ["Hangman's Hill", "Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "You crouch low and watch a robed figure converse silently with a flying, crablike humanoid. Gain one clue from your neighborhood as the alien hands off a small object and flies away (strength). If you pass, you overpower the cultist and take the prize she earned from the Mi-Go; gain one curio."
        , Seq [clue, pass Strength 0 curioItem]
        )
      ,
        ( "St. Mary's Hospital"
        , "The orderly who greets you in the stairwell is dressed in a hooded smock and wearing heavy gloves (observation). If you pass, you pull off the man's rubber mask to reveal the fleshy fungal orb of a Mi-Go's head; gain one clue from your neighborhood. If you fail, the man seizes you in a strangely claw-like grip and throws you out of the hospital; suffer two horror."
        , Test Observation 0 clue (horror 2)
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The shopkeep's eyes glow a vibrant purple as she intones in a voice that is not her own: \"The Sleeper waits in divine slothfulness for the sacrifice.\" Gain one clue from your neighborhood. Miriam shakes her head and smiles, ready to do business once more. Reveal the top three cards of the spell deck; you may buy one of them."
        , Seq [clue, spells 3 (Just 1) FullPrice]
        )
      ]
  , event
      21
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "You find the remains of some kind of ritual spread across the floor of the old chapel and take an item off the altar. Gain one curio as you hear something big moving outside (will). If you pass, you stay still until the oily-black formless spawn passes by; gain one clue from your neighborhood."
        , Seq [curioItem, pass Will 0 clue]
        )
      ,
        ( "St. Mary's Hospital"
        , "Nurse Sharon has the time to see you; you may spend $2 for you or an ally to recover two health. Whether you spend or not, on your way out of the ward, you notice a man with a severed hand (observation). If you pass, you see a wriggling tendril of misshapen flesh emerging from the bandaged wound; gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 2) (health 2), pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "When you touch the book's cover, your mind is flooded with visions of a black stone temple; gain one spell. Miriam Beecher offers to sell you the book for $2 so you can read it in full. If you buy it, you find that it claims the cult of The Sleeper of N'Kai came to Earth from a far-off world; gain one clue from your neighborhood."
        , Seq [spell, mayPay (SpendMoney 2) clue]
        )
      ]
  , event
      22
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "The dissonant hum is coming from somewhere around here, but the noise is making your eyelids droop (will). If you pass, you find a robed cultist slumped against a green metal device, asleep with her hand on the power switch; gain a remnant and one clue from your neighborhood."
        , pass Will 0 (Seq [remnant, clue])
        )
      ,
        ( "St. Mary's Hospital"
        , "The nurse adeptly dresses your wounds; you or an ally recovers two health. As she finishes you both watch a volunteer topple a stack of kidney bowls with a prolonged clatter. \"That one's been an odd duck lately,\" she says (observation). If you pass, you notice a subtle scar around the circumference of his cranium; gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher shows you a carved stone tablet that she acquired for a client who never collected it (observation). If you pass, you decipher text describing an ancient god, his followers on a distant world, and the magic they used to do the bidding of their sleeping master; gain one clue from your neighborhood and one spell."
        , pass Observation 0 (Seq [clue, spell])
        )
      ]
  , event
      23
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "The two moons overhead cast multi-colored light into the clearing on top of the hill (observation). If you pass, you spot a bit of metal glinting in the green light from a small ritual site amidst the derelict graves; gain one clue from your neighborhood and one curio as you search the abandoned altar."
        , pass Observation 0 (Seq [clue, curioItem])
        )
      ,
        ( "St. Mary's Hospital"
        , "Dr. Maheswaren seems to be handling this ward on her own. She has deep bags under her eyes, and tells you that she sent much of her staff home. \"The poor dears just couldn't stay awake, for some reason.\" Gain one clue from your neighborhood. You may spend $2 for her to treat you. If you do, you or an ally recovers two health."
        , Seq [clue, mayPay (SpendMoney 2) (health 2)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher mumbles groggily as you enter the shoppe and leads you toward her library. Reveal the top three spells in the deck. You may buy one of them. Put the rest on the bottom of the deck. If you buy a spell, the woman nods off after your lesson, and will not stir when you attempt to wake her; gain one clue from your neighborhood."
        , Seq [spells 3 (Just 1) FullPrice, clue]
        )
      ]
  , event
      24
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "As you wend your way through the worn tombstones of the old cemetery, you feel a metal claw clamp onto your head (strength). If you pass, you shake the Mi-Go free and flee with its device; gain one clue from your neighborhood. If you fail, the drill bores into your cranium; suffer two damage."
        , Test Strength 0 clue (damage 2)
        )
      ,
        ( "St. Mary's Hospital"
        , "The orderly seems very groggy, but he offers to treat your injuries. You or an ally recovers two health. You may buy him a pot of strong coffee for $1. If you do, he tells you that he's been sleeping all the time lately, and he often dreams of giant bugs injecting him with hooked needles; gain one clue from your neighborhood."
        , Seq [health 2, mayPay (SpendMoney 1) clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The soot-colored statue you pulled from the back of the shelf begins to shift and move in the green moonlight (observation). If you pass, the text appearing on it imparts dangerous knowledge; gain one spell. If you fail, you stare too long and it starts to drip with caustic fluid; suffer one damage. Either way, gain one clue from your neighborhood."
        , Seq [Test Observation 0 spell (damage 1), clue]
        )
      ]
  ]

-- Yuggoth Emergent anomalies
anomaly :: Int -> [((Int, Maybe Int), Text, Effect)] -> CardDef
anomaly n sections =
  CardDef
    (CardCode ("yuggoth-emergent-" <> pad n))
    "Yuggoth Emergent"
    DeadOfNight
    1
    ( AnomalyCard
        (AnomalyDef "Yuggoth Emergent" [(range, Encounter txt eff) | (range, txt, eff) <- sections])
    )

anomalies :: [CardDef]
anomalies =
  [ anomaly
      1
      [
        ( (0, Just 1)
        , "Joey \"the Rat\" offers to sell you a whirring, otherworldly machine, but won't say where he found it. You may spend $2 to buy it. If you do, you crack it open and disconnect the wires inside from a strange, pulsing mass of alien tissue; remove one doom from any space in your neighborhood."
        , mayPay (SpendMoney 2) (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "The cold wind whistles down from the black mountain on the horizon (will). If you pass, you withstand the chill and disable the alien device; remove two doom from your space and gain one remnant. If you fail, you hear voices howling in the wind; suffer one damage and one horror."
        , Test Will 0 (Seq [here 2, remnant]) (harm 1 1)
        )
      ,
        ( (3, Nothing)
        , "You run your fingers through the grooves on the black cyclopean ruins (lore -2). If you pass, you erect magical barriers against the formless beasts that stalk the night; remove three doom from your space and gain one remnant. If you fail, the symbols glow suddenly with baleful light; suffer two horror."
        , Test Lore (-2) (Seq [here 3, remnant]) (horror 2)
        )
      ]
  , anomaly
      2
      [
        ( (0, Just 1)
        , "The basalt altar doesn't belong here (lore). If you pass, you safely scratch away the circle of alien symbols; remove one doom from any space in your neighborhood. If you fail, a dark energy arcs through you when you touch the cold ebon stone; become CURSED."
        , Test Lore 0 (nearby 1) cursed
        )
      ,
        ( (2, Just 2)
        , "The Mi-Go stares at you from the branches of a twisted, skeletal tree (will -1). If you pass, you stare back into its faceless head until it flies off to find an easier specimen to collect; remove two doom from your space and gain one remnant. If you fail, you flee before it can catch you; suffer one horror."
        , Test Will (-1) (Seq [here 2, remnant]) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "When the shapeless creature emerges from the night, it calls out to your mind with a bewildering intellect; you may gain a DARK PACT to persuade it to leave Arkham in peace and return to Yuggoth. If you do, remove three doom from your space and gain one remnant."
        , mayPay (CostCondition "DARK PACT") (Seq [here 3, remnant])
        )
      ]
  , anomaly
      3
      [
        ( (0, Just 1)
        , "A light flares on top of an unknown black mountain far in the distance, drawing your focus (will). If you pass, you shake off its hypnotic effect and move to safety; remove one doom from a space in your neighborhood. If you fail, you feel a pull on your spirit as you stare into the light; suffer two horror."
        , Test Will 0 (nearby 1) (horror 2)
        )
      ,
        ( (2, Just 2)
        , "To reach the shelter up ahead, you must press on through a slow-moving river of frigid, thick muck. Something brushes past you in the morass and you rush to safety in a panic. Suffer two horror and remove two doom from your space before you close the door of the structure firmly behind you."
        , Seq [horror 2, here 2]
        )
      ,
        ( (3, Nothing)
        , "The huge black buildings are not as empty as you'd hoped. If you stay calm, you might make it to the Mi-Go machine in the center (will -1). If you pass, you manage to keep quiet and shut it down; remove three doom from your space. If you fail, the guardian beasts spot you; suffer three damage."
        , Test Will (-1) (here 3) (damage 3)
        )
      ]
  , anomaly
      4
      [
        ( (0, Just 1)
        , "The bridge, inscribed with alien text, stretches high over a dark, icy ravine (lore). If you pass, you read the words aloud as you cross the bridge; remove one doom from any space in your neighborhood. If you fail, the bridge demands your supplication and buckles beneath you; suffer two damage."
        , Test Lore 0 (nearby 1) (damage 2)
        )
      ,
        ( (2, Just 2)
        , "Deep in the cold black fog, you study the alien sky in an attempt to find your way home (lore). If you pass, you orient yourself with the baleful green moon and navigate to safety; remove two doom from your space and gain one remnant. If you fail, you become hopelessly lost; suffer two horror."
        , Test Lore 0 (Seq [here 2, remnant]) (horror 2)
        )
      ,
        ( (3, Nothing)
        , "The cold wind whispers your name as you work to drive the influence of Yuggoth from the area around you (lore -1). If you pass, you block out the harsh voice and complete your protective ritual; remove two doom from your space. If you fail, the voice foretells your failure; become CURSED."
        , Test Lore (-1) (here 2) cursed
        )
      ]
  , anomaly
      5
      [
        ( (0, Just 1)
        , "The symbols adorning the black tablet appear to hold the secret to driving back the encroaching alien world (lore). If you pass, a shining light drives back the black mist of Yuggoth; remove one doom from any space in your neighborhood. If you fail, suffer two horror as the stone fades to black smoke."
        , Test Lore 0 (nearby 1) (horror 2)
        )
      ,
        ( (2, Just 2)
        , "The Mi-Go readies its scalpels and explains that it never experiments on intelligent life (lore -1). If you pass, you are able to convince it of your sentience with a truly esoteric display of minutiae; remove one doom from any space in your neighborhood. If you fail, it slices into your arm; suffer one damage."
        , Test Lore (-1) (nearby 1) (damage 1)
        )
      ,
        ( (3, Nothing)
        , "The bulky metal crab's crackly voice pleads for mercy (will -2). If you pass, you get close enough to disconnect the human brain within; remove three doom from your space and gain one remnant. If you fail, it cries in anguish as it beats you with its shovel-like claws; suffer two damage."
        , Test Will (-2) (Seq [here 3, remnant]) (damage 2)
        )
      ]
  , anomaly
      6
      [
        ( (0, Just 0)
        , "Your eyelids grow heavy as you move closer to the green metal orb (will). If you pass, you seize it just before it flies away; remove one doom from any space in your neighborhood. If you fail, you fall asleep; when you wake, you see the two halves of the orb on the ground, but the contents are gone."
        , pass Will 0 (nearby 1)
        )
      ,
        ( (1, Just 2)
        , "The doorway in front of you appears to lead to a black stone circle instead of the familiar streets of Arkham (will). If you pass, you step slowly through the portal with your mind locked on your destination; remove two doom from your space and gain one remnant."
        , pass Will 0 (Seq [here 2, remnant])
        )
      ,
        ( (3, Nothing)
        , "This massive building appears to be a library of metal rings, etched in an unknown tongue (lore -1). If you pass, you find that one of them contains a history of the Mi-Go; remove two doom from your space and gain one remnant. If you fail, the text you read is warded; suffer two damage."
        , Test Lore (-1) (Seq [here 2, remnant]) (damage 2)
        )
      ]
  , anomaly
      7
      [
        ( (0, Just 0)
        , "You press yourself against a wall as the shapeless mass of cold flesh writhes past you (will). If you pass, the creature moves on; remove one doom from any space in your neighborhood. If you fail, you let loose a single whimper before it crashes its bulk upon you; suffer two damage."
        , Test Will 0 (nearby 1) (damage 2)
        )
      ,
        ( (1, Just 2)
        , "The stone blocking the corridor is coated in thick hoarfrost, but there is no other way to escape the gibbering mass of mouths and flesh. When you put your shoulder into the stone and push it out of your way, the cold burns deep into your skin. Suffer two damage and remove one doom from your space."
        , Seq [damage 2, here 1]
        )
      ,
        ( (3, Nothing)
        , "Something is watching you from the black fog (will -1). If you pass, you steel your nerves and walk resolutely back to the Arkham streetlights in the distance; remove three doom from your space and gain one remnant. If you fail, the fog grows thick, holding you in place; become delayed."
        , Test Will (-1) (Seq [here 3, remnant]) delayed
        )
      ]
  , anomaly
      8
      [
        ( (0, Just 0)
        , "The stone obelisk is so dark that your lantern seems to dim in its proximity (lore). If you pass, you extinguish your lamp and instead study the stone with your fingertips; remove one doom from any space in your neighborhood. If you fail, you are filled with hopeless dread; suffer one horror."
        , Test Lore 0 (nearby 1) (horror 1)
        )
      ,
        ( (1, Just 1)
        , "The cold metal brazier in the decrepit shrine on top of the hill is filled with a deep violet oil (lore). If you pass, you speak the words etched into the green metal and the brazier flares to life, driving back the darkness with its cool blue flame; remove one doom from your space."
        , pass Lore 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "Something breaks the glassy black surface of the unfamiliar lake (will -2). If you pass, you hold your ground and recite a litany that holds the unseen beast at bay; remove up to three doom from your space and gain one remnant. If you fail, it wraps you in its cold black mass; suffer two horror."
        , Test Will (-2) (Seq [here 3, remnant]) (horror 2)
        )
      ]
  , anomaly
      9
      [
        ( (0, Just 0)
        , "You see spectral faces in the dark fog all around you, beckoning you to follow them (will). If you pass, they lead you to a grisly ritual site where you can lay their bodies to rest; remove one doom from any space in your neighborhood. If you fail, you make a torch out of a twisted branch to dispel the mist."
        , pass Will 0 (nearby 1)
        )
      ,
        ( (1, Just 1)
        , "You hear chanting in the darkness, but cannot find the source (lore). If you pass, you call out the counter charm needed to prevent the ritual's magic from taking hold; remove one doom from your space. If you fail, the cultist in the dark finds you with her crooked blade; suffer one damage."
        , Test Lore 0 (here 1) (damage 1)
        )
      ,
        ( (2, Nothing)
        , "There is a buzzing alien device at the heart of this ebon stone labyrinth. You may become delayed to find your way through. If you do, you tear the exposed wires out of the fleshy core of the device; remove two doom from your neighborhood and gain one remnant."
        , mayPay CostDelayed (Seq [nearby 2, remnant])
        )
      ]
  , anomaly
      10
      [
        ( (0, Just 0)
        , "The traveler points down the road and rants at you in an unfamiliar tongue (lore). If you pass, you understand his directions and find the gate back to Arkham; remove one doom from any space in your neighborhood. If you fail, you go down the wrong fork; suffer one damage and one horror."
        , Test Lore 0 (nearby 1) (harm 1 1)
        )
      ,
        ( (1, Just 1)
        , "The woman in the long coat offers to guide you through the ruins (will). If you pass, you realize that her face is actually a rubber mask and reveal her to be a Mi-Go in disguise; remove one doom from your space. If you fail, she leads you astray in the cold black fog; become delayed."
        , Test Will 0 (here 1) delayed
        )
      ,
        ( (2, Nothing)
        , "The Mi-Go's curiosity allows you to negotiate with it, despite your lack of a shared language (lore -1). If you pass, it leaves you a rancid gift; remove two doom from your space and gain one remnant. If you fail, it grows tired of your conversation; suffer one damage and one horror."
        , Test Lore (-1) (Seq [here 2, remnant]) (harm 1 1)
        )
      ]
  , anomaly
      11
      [
        ( (0, Just 0)
        , "The unfamiliar sky rumbles with thunder (lore). If you pass, you read the sign at the crossroads correctly and find shelter in time; remove one doom from any space in your neighborhood. If you fail, the acrid rain catches you on the road, the vapor burning your eyes and throat; suffer one damage."
        , Test Lore 0 (nearby 1) (damage 1)
        )
      ,
        ( (1, Just 1)
        , "The Mi-Go seems wholly focused on retrieving specimens for its grim research, and has a young couple imprisoned in a metal cage. You may give it one remnant to provide it with alternate materials. If you do, it releases them and takes its new prize away with it; remove one doom from your space."
        , mayPay (SpendRemnants 1) (here 1)
        )
      ,
        ( (2, Nothing)
        , "The formless spawn rises from the dark water all around you, threatening to envelop you with its bulk (lore -2). If you pass, you identify a weak point and escape; remove up to three doom from your space and gain one remnant. If you fail, it wraps you in clammy black flesh; suffer three horror."
        , Test Lore (-2) (Seq [here 3, remnant]) (horror 3)
        )
      ]
  , anomaly
      12
      [
        ( (0, Just 0)
        , "The silent forest before you is filled with stone trees and dark fog (will). If you pass, you pick your way through the black stone branches and find your way back to Arkham; remove one doom from any space in your neighborhood. If you fail, something follows you through the fog; suffer one horror."
        , Test Will 0 (nearby 1) (horror 1)
        )
      ,
        ( (1, Just 1)
        , "The crawling monstrosity has backed you into a corner, and you try desperately to invoke a barrier charm (lore). If you pass, you hold the putrescent creature at bay; remove one doom from your space. If you fail, it exudes a buzzing swarm of grave flies that crawl down your throat; suffer one horror."
        , Test Lore 0 (here 1) (horror 1)
        )
      ,
        ( (2, Nothing)
        , "The black crypt is a mausoleum dedicated to some massive creature. The stones shift as the beast entombed within stirs (will -1). If you pass, you lay a hand on the altar and will the creature to return to its slumber; remove two doom from your space and gain one remnant."
        , pass Will (-1) (Seq [here 2, remnant])
        )
      ]
  ]
