{- | Shots in the Dark.

Arkham's gangs are tearing each other apart, and something is feeding on it.
Tokens on the sheet bring the Cruel Hunger into view (cards 41 and 51); the
alliance the investigators strike, or refuse to strike, decides which gang they
break (cards 42 to 50); doom on the sheet brings the Dark Pharaoh's shadow fully
into the world (card 52).
-}
module AH3e.Content.DeadOfNight.ShotsInTheDark (code, scenario, cards, setAsideMonsters) where

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
code = "shots-in-the-dark"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

{- | The gang muscle the sheet holds back. Cards 42 and 43 shuffle one side's worth
into the deck when their offer is refused, and card 46 lets the rest in at once.
-}
setAsideMonsters :: [CardCode]
setAsideMonsters =
  [ "brutal-goons"
  , "corben-bouchard"
  , "hit-squad"
  , "mob-enforcer"
  , "rough-bootlegger"
  , "siobhan-riley"
  ]

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Shots in the Dark"
    , expansion = DeadOfNight
    , startingSpace = spaceIdFor "Police Station"
    , reckoningText = "If no gang is hostile, spawn one cultist monster."
    , reckoning = Custom "sitd-reckoning"
    , setupMap =
        buildMap
          [nb "Downtown", nb "Northside", nb "Easttown", nb "Merchant District", nb "Rivertown"]
          [ StreetDef (nb "Downtown") BottomLeft (nb "Northside") Scenic
          , StreetDef (nb "Downtown") BottomRight (nb "Easttown") Residential
          , StreetDef (nb "Northside") SideRight (nb "Easttown") Residential
          , StreetDef (nb "Easttown") BottomLeft (nb "Merchant District") Bridge
          , StreetDef (nb "Easttown") BottomRight (nb "Rivertown") Bridge
          , StreetDef (nb "Merchant District") SideRight (nb "Rivertown") Scenic
          ]
    , monsters =
        [ ("brawling-riot", 1)
        , ("feckless-agitator", 1)
        , ("ghoul-acolyte", 2)
        , ("mouthy-raconteur", 1)
        , ("occult-ritualist", 2)
        , ("vicious-glutton", 2)
        , -- every nightgaunt monster
          ("abyssal-servant", 1)
        , ("capricious-stalker", 1)
        , ("eyeless-watcher", 1)
        ]
    , startingMonsters =
        [ ("feckless-agitator", spaceIdFor "Train Station")
        , ("mouthy-raconteur", spaceIdFor "Black Cave")
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
          ["La Bella Luna", "Curiositie Shoppe", "Hibb's Roadhouse", "River Docks", "Graveyard"]
    , -- the red marker is the O'Bannion Stronghold, the blue one the Sheldons'
      startingMarkers = [(spaceIdFor "La Bella Luna", "red"), (spaceIdFor "River Docks", "blue")]
    , eventCards = [CardCode ("sitd-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , setAside = setAsideMonsters
    , codex = [1, 41, 42, 43]
    , anomalySet = Nothing
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = archive <> events <> strongholds

pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

wanted :: Effect
wanted = GainE (Condition "WANTED")

darkPact :: Cost
darkPact = CostCondition "DARK PACT"

buyOneHalfThen, buyAnyThen :: Trait -> Effect -> Effect
buyOneHalfThen t = BuyFromDisplay (Just t) HalfPrice (Just 1)
buyAnyThen t = BuyFromDisplay (Just t) FullPrice Nothing

{- | Cards 48 and 49 turn the gangs' own property into something you can hit. The
rules give them a monster's statistics, so they are monsters: massive, so they
cannot be exhausted or walked past, and with no health of their own beyond what
the table brings.
-}
strongholds :: [CardDef]
strongholds =
  [ stronghold "sitd-sheldon-base" "Sheldon Base" 1 0
  , stronghold "sitd-obannion-stronghold" "O'Bannion Stronghold" 4 (-1)
  ]

stronghold :: CardCode -> Text -> Int -> Int -> CardDef
stronghold c n elite atk =
  CardDef
    c
    n
    DeadOfNight
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn = CustomSpaceRule "Placed by the codex"
          , activation = Lurker NoEffect
          , speed = 0
          , traits = ["Structure"]
          , health = 0
          , elite = elite
          , attackSkill = Strength
          , attackModifier = atk
          , evadeModifier = 0
          , damage = 0
          , horror = 0
          , remnant = False
          , keywords = [Massive]
          , epic = False
          , text =
              "An investigator may attack, damage and defeat this as though it were a massive monster. It does not attack."
          }
    )

archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n title front back =
  CardDef
    (CardCode ("sitd-" <> tshow n))
    title
    DeadOfNight
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

archive :: [CardDef]
archive =
  [ archiveCard
      41
      "One More Spark"
      "[Objective][Doom] When there is a total of three or more tokens on the scenario sheet (clues, doom, or any other type), each investigator suffers one damage or one horror for each doom on the scenario sheet. Then, add card 51 to the codex and flip this card."
      ( Just
          "The Knife's Edge\nReckoning - Place one doom in each space with one or more human monsters.\n[Doom] When there is six or more doom on the scenario sheet, add card 52 to the codex and return this card to the archive."
      )
  , archiveCard
      42
      "The Sheldon Gang"
      "The Sheldon gang is \"unfriendly.\"\nAfter an O'Bannion monster is defeated, you may propose an alliance with the Sheldon Gang to flip this card.\nIf you do not, shuffle a random set-aside O'Bannion monster into the monster deck. Then, the lead investigator draws and resolves one token from the mythos cup."
      ( Just
          "Shuffle all set-aside O'Bannion monsters into the monster deck.\nAdd one spread doom token to the mythos cup.\nChoose one investigator to become a LEGBREAKER for the Sheldons.\nAdd cards 44 and 45 to the codex with the \"Sheldon Alliance\" side up. Then, return this card and card 43 to the archive."
      )
  , archiveCard
      43
      "The O'Bannions"
      "The O'Bannion gang is \"unfriendly.\"\nAfter a Sheldon monster is defeated, you may propose an alliance with the O'Bannions to flip this card.\nIf you do not, shuffle a random set-aside Sheldon monster into the monster deck. Then, the lead investigator draws and resolves one token from the mythos cup."
      ( Just
          "Shuffle the set-aside Sheldon monsters into the monster deck.\nAdd one spawn monster token to the mythos cup.\nChoose one investigator to become a CLEANER for the O'Bannions.\nAdd cards 44 and 45 to the codex with the \"O'Bannion Alliance\" side up. Then, return this card and card 42 to the archive."
      )
  , archiveCard
      44
      "Sheldon Alliance"
      "The Sheldon gang is \"wary.\"\nAction: Tell your Sheldon contact where they can find a few jobs that need doing (influence). If you pass, deal two damage to an O'Bannion monster in any space and gain $1. Perform this action only at the Sheldon Stronghold.\nAfter you defeat an O'Bannion enemy, place one damage token on the scenario sheet.\n[Objective] When there are four damage tokens on the scenario sheet, remove them, add card 49 to the codex, and return this card and card 45 to the archive."
      ( Just
          "O'Bannion Alliance\nThe O'Bannion gang is \"wary.\"\nAction: Put the finger on a few Sheldon tough guys (influence). If you pass, deal two damage to a Sheldon monster in any space and focus one skill. Perform this action only at the O'Bannion Stronghold.\nAfter you defeat a Sheldon enemy, place one horror token on the scenario sheet.\n[Objective] When there are four horror tokens on the scenario sheet, remove them, add card 48 to the codex, and return this card and card 45 to the archive."
      )
  , archiveCard
      45
      "Sheldon Alliance"
      "The O'Bannion gang is \"hostile.\"\nAction: You attempt to play both sides by convincing the O'Bannions that you are willing to work for them (influence -1). If you pass, add card 46 to the codex. This action may only be performed at the O'Bannion Stronghold.\nReckoning - Spawn one O'Bannion monster unless you place one doom at La Bella Luna."
      ( Just
          "O'Bannion Alliance\nThe Sheldon gang is \"hostile.\"\nAction: You attempt to play both sides by convincing the Sheldons that you are willing to work for them (influence -1). If you pass, add card 46 to the codex. This action may only be performed at the Sheldon Stronghold.\nReckoning - Spawn one Sheldon monster unless you place one doom at the River Docks."
      )
  , archiveCard
      46
      "Working an Angle"
      "Return cards 44 and 45 to the archive, and shuffle all set-aside monsters into the monster deck.\nBoth gangs are \"wary.\"\nAfter a Sheldon monster is defeated, place one horror on the scenario sheet.\nAfter an O'Bannion monster is defeated, place one damage on the scenario sheet.\nAction: Test influence. You may discard from your space a number of doom or human monsters up to your test result. (Monsters discarded in this way are not defeated.)\nReckoning - Flip this card."
      ( Just
          "One investigator tests influence. You may remove a number of damage or horror tokens from the scenario sheet up to your test result.\nIf there are an equal number of damage and horror tokens on the scenario sheet, remove all damage and horror tokens from it and flip this card.\nOtherwise, each investigator and ally suffers one damage and one horror. Then, remove all damage and horror tokens from the scenario sheet.\nIf there is a white marker on the scenario sheet, add card 47 to the codex and return this card to the archive. Otherwise, place a white marker on the scenario sheet and flip this card."
      )
  , archiveCard
      47
      "Caught!"
      "Both gangs are \"hostile.\"\nSpawn one servitor monster.\nAfter an ally or investigator is defeated, place one white marker on the scenario sheet. White markers on the scenario sheet are \"casualties.\"\n[Objective] If there are four or more casualties, flip this card.\nReckoning - The top card of the ally deck is defeated."
      (Just "Investigators lose the game!")
  , archiveCard
      48
      "Hit Them Hard"
      "Reckoning - If Corben Bouchard is in play, heal all damage from him. Otherwise, spawn him.\nThe O'Bannion gang is \"friendly.\" Remove all O'Bannion monsters from the game and shuffle the monster deck.\nThe Sheldon gang is \"hostile.\"\nPlace one white marker in each non-Scenic street space. These are \"Sheldon Bases.\" An investigator may attack, damage, and defeat a Sheldon Base as though it is a massive monster with one health per investigator.\n[Objective] After all Sheldon Bases are defeated, flip this card."
      (Just "The O'Bannion Gang and their loyal allies win the game!")
  , archiveCard
      49
      "Wipe Them Out"
      "Reckoning - If Siobhan Riley is in play, an investigator in her space suffers one damage and one horror. Otherwise, spawn her.\nThe Sheldon gang is \"friendly.\" Remove all Sheldon monsters from the game and shuffle the monster deck.\nThe O'Bannion gang is \"hostile.\"\nAn investigator may attack, damage, and defeat the O'Bannion Stronghold as though it is a massive monster with a -1 attack modifier and 4 health per investigator.\n[Objective] After the O'Bannion Stronghold is defeated, flip this card."
      (Just "The Sheldon Gang and their loyal allies win the game!")
  , archiveCard
      50
      "Breaking Bonds"
      "[Objective] Action: An investigator at a gang's stronghold may attempt to break the cult's hold (will -1). If that gang is wary or if you have an asset with that gang's name in either the trait or the title, roll one additional die. If that gang is friendly, roll two additional dice instead.\nYou may move a number of clues up to your test result from the scenario sheet to a stronghold in your space.\nWhen each stronghold has four or more clues on it, flip this card."
      (Just "Investigators win the game!")
  , archiveCard
      51
      "Seeing Red"
      "[Objective] When there are four or more clues on the scenario sheet, flip this card."
      ( Just
          "A New Power\nAdd one blank token to the mythos cup.\n[Objective] When there are six or more clues on the scenario sheet, add card 50 to the codex and return this card to the archive."
      )
  , archiveCard
      52
      "Shadow of Death"
      "Add one gate burst token to the mythos cup.\nReckoning - For each Human monster, place one doom in its space and deal one horror to each investigator and ally in its neighborhood.\n[Doom] When there is 12 or more doom on the scenario sheet, flip this card."
      (Just "Investigators lose the game!")
  ]

event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("sitd-event-" <> pad n))
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
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "The patient in the lobby is ranting about the hungry shadow with the glowing eyes, just like the man you saw raving on the street this morning; gain one clue from your neighborhood. You leave her behind and go to your appointment; you may spend $2 for you or an ally to recover two sanity."
        , Seq [clue, mayPay (SpendMoney 2) (sanity 2)]
        )
      ,
        ( "Independence Square"
        , "While the police rope off a section of the park, an urchin claims to know what happened. You may spend $1 to get her to share her secrets. If you do, she explains that a man from the bank was sharing an ice cream with his secretary when she suddenly stabbed him with a fountain pen; gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) clue
        )
      ,
        ( "La Bella Luna"
        , "As you order a drink in the Clover Club, you notice a pair of O'Bannion gangsters speaking in hushed tones (observation). If you pass, you overhear one of them describe his futile attempts to calm down when confronted by a now-deceased truck driver; gain one clue from your neighborhood. If you fail, you back off before they notice you."
        , pass Observation 0 clue
        )
      ]
  , event
      2
      "Downtown"
      ["Arkham Asylum"]
      [
        ( "Arkham Asylum"
        , "Nurse Heather listens intently while you describe your situation; you or an ally recovers two sanity. After your session, you watch an orderly push a bloody mop bucket down the hallway (observation). If you pass, you spot the evidence of a terrible fight in the staffroom; gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ,
        ( "Independence Square"
        , "The newsie has been pestering you ever since you arrived. As you pause by Founder's Rock, he offers you the paper one time too many (will). If you pass, you catch yourself and realize that there is something unnatural about your temper; gain one clue from your neighborhood. If you fail, the boy runs off when you start to shout."
        , pass Will 0 clue
        )
      ,
        ( "La Bella Luna"
        , "You can faintly hear a conversation from one of the private rooms in the Clover Club (observation). If you pass, you listen to a city councilman agree to stay out of the O'Bannion gang's way when they move on a Sheldon warehouse; gain one clue from your neighborhood. If you fail, a beefy thug shows you the door; suffer one damage."
        , Test Observation 0 clue (damage 1)
        )
      ]
  , event
      3
      "Downtown"
      ["Independence Square"]
      [
        ( "Arkham Asylum"
        , "A frustrated Dr. Mintz is explaining to a nurse that even a partial lobotomy didn't work to calm one of his more problematic cases; gain one clue from your neighborhood. You may spend $1 for you or an ally to recover two sanity after you decide to consult one of the other doctors."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 2)]
        )
      ,
        ( "Independence Square"
        , "The vendors in the flea market are fighting over \"the good spot.\" A bewildered Anna Kaslow confides in you that the two men arguing the loudest are usually the best of friends; gain one clue from your neighborhood. You decide to make your selections before things escalate; you may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "La Bella Luna"
        , "The older woman stares grimly into her drink and pushes a meatball around her plate. You attempt to strike up a conversation (influence). If you succeed, she confides in you that she and her partner are on the outs after a surprisingly vicious fight; gain one clue from your neighborhood. If you fail, you duck the flying meatball and back away."
        , pass Influence 0 clue
        )
      ]
  , event
      4
      "Downtown"
      ["Independence Square", "La Bella Luna"]
      [
        ( "Arkham Asylum"
        , "The orderly bears down on you with a heavy truncheon in his hand. He's red in the face and refuses to believe that you aren't a patient (influence). If you pass, you calm him down and gain one clue from your neighborhood. If you fail, you flee and he reports your escape; become WANTED."
        , Test Influence 0 clue wanted
        )
      ,
        ( "Independence Square"
        , "A vendor in the flea market presents you with a sample of his wares; gain one common item. When one of his neighbors accuses you of theft, you begin to lose your temper (will). If you pass, you calm yourself down before things get out of hand and quietly explain the misunderstanding; gain one clue from your neighborhood."
        , Seq [commonItem, pass Will 0 clue]
        )
      ,
        ( "La Bella Luna"
        , "Peter Clover quietly warns the dealer at your table that the Sheldons made a move on the bagman last night; gain one clue from your neighborhood. You may attempt to convince Clover that you'd be a reliable guard for the courier (influence). If you pass, he pays you $3 for the job. If you fail, he tells you to keep your eyes on the cards."
        , Seq [clue, pass Influence 0 (money 3)]
        )
      ]
  , event
      5
      "Downtown"
      ["La Bella Luna"]
      [
        ( "Arkham Asylum"
        , "The doctor has an opening for you; you may spend $2 for you or an ally to recover two sanity. If you do, she helps you realize that the anger you've been feeling is not only unhealthy, but likely triggered by some unnatural outside force; you gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [sanity 2, clue])
        )
      ,
        ( "Independence Square"
        , "You spot a ragged form leaned against a birch tree, clutching a package; gain one curio. He isn't moving (observation). If you pass, you quickly notice a tear across his abdomen shaped like a grinning mouth; gain one clue from your neighborhood. If you fail, Deputy Galeas Morgan catches you standing over the mutilated body; become WANTED."
        , Seq [curioItem, Test Observation 0 clue wanted]
        )
      ,
        ( "La Bella Luna"
        , "You cash out from a hot streak; gain $2. The woman you won it from doesn't seem ready to let it go (influence). If you pass, you talk her down and she describes a voice in her head encouraging her to kill you; gain one clue from your neighborhood. If you fail, you suffer two damage as her fingernails find purchase just below your brow."
        , Seq [money 2, Test Influence 0 clue (damage 2)]
        )
      ]
  , event
      6
      "Downtown"
      ["La Bella Luna"]
      [
        ( "Arkham Asylum"
        , "You stare into the candle flame while the doctor speaks peacefully (observation). If you pass, the hypnotherapy allows you to recognize the strange shadow that's been following you for days; gain one clue from your neighborhood and you or an ally recovers two sanity."
        , pass Observation 0 (Seq [clue, sanity 2])
        )
      ,
        ( "Independence Square"
        , "Anna Kaslow calls to you from across the park; you may spend $2 for a tarot reading. If you do, her body shivers as she intones a proclamation about \"a mask of pure fury\" that will consume the world. She angrily thrusts an object into your hands and stalks off across the square; you gain a curio and one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [curioItem, clue])
        )
      ,
        ( "La Bella Luna"
        , "You overhear McTyre tearing into a detective on the payroll about a deal that went south. Gain one clue from your neighborhood and try to remain discreet (observation). If you pass, you calmly finish your drink; you or an ally recovers one sanity. If you fail, you beat feet when McTyre stares straight at you, his jaw tight; become WANTED."
        , Seq [clue, Test Observation 0 (sanity 1) wanted]
        )
      ]
  , event
      7
      "Easttown"
      ["Hibb's Roadhouse", "Velma's Diner"]
      [
        ( "Hibb's Roadhouse"
        , "The two men at the table behind you sound like they're talking about feeding some kind of pet or animal, but \"Nephren-Ka\" is an odd name for a dog; gain one clue from your neighborhood. As you drink and puzzle over their conversation, you may spend $1 for you or an ally to recover two sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 2)]
        )
      ,
        ( "Police Station"
        , "You ask Deputy \"Gal\" Morgan why he isn't wearing his sidearm (influence). If you pass, he confesses that he locked it up after a voice in his head nearly persuaded him to shoot a fellow officer during an argument; gain one clue from your neighborhood. If you fail, he prods you in the chest with a truncheon and sends you on your way."
        , pass Influence 0 clue
        )
      ,
        ( "Velma's Diner"
        , "You recover two health as you finish your pie and sit for a spell, while the man at the next table loudly slurps his soup (will). If you pass, you prise the fork out of your hand and resist the sudden temptation to stop him; gain one clue from your neighborhood. If you fail, you picture your fork jabbing into his neck too vividly; you suffer one horror."
        , Seq [myHealth 2, Test Will 0 clue (horror 1)]
        )
      ]
  , event
      8
      "Easttown"
      ["Hibb's Roadhouse", "Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "You enjoy a hearty meal before the trouble starts; you or an ally recovers two sanity (observation). If you pass, you can identify the truck driver who started the brawl from his mask tattoo; gain one clue from your neighborhood. If you fail, the Sheldon gang holds you responsible for the fight; become WANTED."
        , Seq [sanity 2, Test Observation 0 clue wanted]
        )
      ,
        ( "Police Station"
        , "Deputy Dingby raises a brick to smash in the window of his own patrol wagon (observation). If you pass, you spot his lost keys and calm him down before he gets himself into trouble; gain one clue from your neighborhood. If you fail, you watch him smash the window, drop the brick on his own foot, and angrily shoot out his own tires."
        , pass Observation 0 clue
        )
      ,
        ( "Velma's Diner"
        , "Velma's front window is boarded up. When you get inside, you may spend $2 for you or an ally to recover two health with a hot beef sandwich. If you do, the man next to you tells you how David Packard lost his temper over getting the wrong change and threw a chair through the glass; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [health 2, clue])
        )
      ]
  , event
      9
      "Easttown"
      ["Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "A careless elbow spills your whiskey on a stranger who seems quite intent on proving how much a stool over the head can hurt. You may spend $1 to buy him a drink. If you do, he calms down and proves rather amiable; gain one clue from your neighborhood. Otherwise, suffer two damage."
        , MayPay (SpendMoney 1) clue (damage 2)
        )
      ,
        ( "Police Station"
        , "The man in the holding cell sobbingly confesses that he didn't mean to hurt anyone, but the shadow in the alley wouldn't stop whispering to him; gain one clue from your neighborhood. You try to convince him that you can help (influence). If you pass, he tells the officer on duty to give you his personal effects; gain one common item."
        , Seq [clue, pass Influence 0 commonItem]
        )
      ,
        ( "Velma's Diner"
        , "As you wash up in the restroom of the diner, you feel your skin begin to crawl (observation). If you pass, you realize that you are casting two shadows in the light from the dirty electric lamp, and one of them is far too long; gain one clue from your neighborhood as you beat a hasty retreat. If you fail, the dingy light makes you lose your appetite."
        , pass Observation 0 clue
        )
      ]
  , event
      10
      "Easttown"
      ["Velma's Diner"]
      [
        ( "Hibb's Roadhouse"
        , "Old Man Hibbard opens the door with an axe handle in his hand. You may spend $2 to have a drink and a meal; if you do, you or an ally recovers two sanity. If you pay, Hibbard tells you that he's had to run off quite a few surly patrons in the past week; gain one clue from your neighborhood."
        , mayPay (SpendMoney 2) (Seq [sanity 2, clue])
        )
      ,
        ( "Police Station"
        , "The light outside the station flickers as you spot an object abandoned on the road. Gain one common item and look around for the owner (observation). If you pass, you spot a winged humanoid flying off into the dark sky with a struggling figure gripped in its talons; gain one clue from your neighborhood as you fruitlessly attempt to follow the beast."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Velma's Diner"
        , "Velma explains that just the other day a half-dressed man with a strange scar across his belly stood outside staring at the wall for two hours before she chased him off with a baseball bat; gain one clue from your neighborhood. She shrugs and shows you a menu; you may spend $1 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ]
  , event
      11
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "You spot Joey \"the Rat\" arguing with a shadow on the wall; gain one clue from your neighborhood. You wait for a beat before gently clearing your throat. He jumps a bit and looks around nervously before offering to do business. You may spend one remnant to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Tick-Tock Club"
        , "\"Dainty\" Donohue is stalking the club, tossing out guests he accuses of loitering. You may spend $1 to order a drink. If you do, you listen to the band while you notice a tall shadow following the small man around the room; gain one clue from your neighborhood. Otherwise, you join the other disgruntled guests outside, muttering in the heat."
        , mayPay (SpendMoney 1) clue
        )
      ,
        ( "Unvisited Isle"
        , "The bodies in the clearing are all that remains of a bloody gang skirmish, but they seem unnaturally gaunt and several sport masks of black paint around their eyes (lore). If you pass, you realize that the foul smell is coming from a ritual oil anointing bodies on both sides; gain one clue from your neighborhood and one remnant."
        , pass Lore 0 (Seq [clue, remnants 1])
        )
      ]
  , event
      12
      "Merchant District"
      ["River Docks"]
      [
        ( "River Docks"
        , "The body laying on the pier is clutching a handful of cash. You gain $3 as you search the scene (observation). If you pass, you conclude that this Sheldon goon was killed by his partner; gain one clue from your neighborhood. If you fail, you can't explain it to the enforcer who finds you there; become WANTED."
        , Seq [money 3, Test Observation 0 clue wanted]
        )
      ,
        ( "Tick-Tock Club"
        , "After the set, you notice the band hurriedly withdraw to the green room. You may spend $2 to buy the musicians a meal and join them in the back. If you do, they tell you over roast beef about the fist fights that break out in the club all too often these days; you gain one clue from your neighborhood and you or an ally recovers two health."
        , mayPay (SpendMoney 2) (Seq [clue, health 2])
        )
      ,
        ( "Unvisited Isle"
        , "The clearing is slick with the blood of a badly mutilated animal; gain one clue from your neighborhood as you study the body (lore). If you pass, you can tell from the patterns in the mud that this was a ritual, not just random violence; gain one remnant. If you fail, the gruesome scene is too senseless for you; suffer one horror."
        , Seq [clue, Test Lore 0 (remnants 1) (horror 1)]
        )
      ]
  , event
      13
      "Merchant District"
      ["River Docks", "River Docks"]
      [
        ( "River Docks"
        , "The Sheldon woman with the shotgun offers you a bribe to send you away from the alley; gain one common item. You may give her a remnant to demonstrate that you're used to handling this kind of thing. If you do, she shows you the dead ghouls; gain one clue from your neighborhood."
        , Seq [commonItem, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Tick-Tock Club"
        , "\"Dainty\" Donohue scowls at you from the end of the bar (observation). If you pass, you notice that an extra shadow looms on the wall behind him; gain one clue from your neighborhood. If you fail, he takes affront to the way you're staring at him; you suffer two damage when the doorman hauls you outside and works you over."
        , Test Observation 0 clue (damage 2)
        )
      ,
        ( "Unvisited Isle"
        , "The circle of robed figures chants about a Great Messenger \"robed in the waxen visage of a man.\" The words fill you with a terrible rage (will). If you pass, you stay calm and record what they're saying; gain one clue from your neighborhood. If you fail, your vision blurs and you awaken hours later with blood on your face; suffer one horror."
        , Test Will 0 clue (horror 1)
        )
      ]
  , event
      14
      "Merchant District"
      ["Tick-Tock Club"]
      [
        ( "River Docks"
        , "The shadows between the warehouses stretch around you like black pools (observation). If you pass, you notice the frenzied man before he tackles you; gain one clue from your neighborhood as you wrestle him away. If you fail, he pins you to the ground and claws at your face; suffer one damage."
        , Test Observation 0 clue (damage 1)
        )
      ,
        ( "Tick-Tock Club"
        , "There are two more palookas watching the door than usual. They fidget anxiously and seem like they want an excuse to bloody somebody's face; gain one clue from your neighborhood. You may spend $2 to buy your way past them and into the speakeasy. If you do, you or an ally recovers two sanity. If you refuse, one of them growls and chases you off."
        , Seq [clue, mayPay (SpendMoney 2) (sanity 2)]
        )
      ,
        ( "Unvisited Isle"
        , "The hot air is unnaturally still in this clearing, and someone has etched arcane symbols into the parched ground (lore). If you pass, you recognize that the purpose of this ritual site is to incorporate some entity into a physical form; gain one remnant and one clue from your neighborhood. If you fail, you leave the puzzle behind you."
        , pass Lore 0 (Seq [remnants 1, clue])
        )
      ]
  , event
      15
      "Merchant District"
      ["Tick-Tock Club"]
      [
        ( "River Docks"
        , "Joey Vigil lets you know that he's got a buyer looking for unusual merchandise; you may give him one remnant to satisfy his customer. If you do, he gives you the lowdown on a shootout in Uptown while he offers you something in trade; gain one clue from your neighborhood and a common item."
        , mayPay (SpendRemnants 1) (Seq [clue, commonItem])
        )
      ,
        ( "Tick-Tock Club"
        , "You sit for a while, letting the atmosphere of the club distract you from the chaos out on the streets. You or an ally recovers one health and one sanity while you sit and listen to the other patrons (observation). If you pass, you overhear two men discussing a creature they saw in an alleyway; gain one clue from your neighborhood."
        , Seq [RecoverBoth YouOrAlly (N 1) (N 1), pass Observation 0 clue]
        )
      ,
        ( "Unvisited Isle"
        , "You hide while a pair of O'Bannion toughs put the screws to a raving Sheldon smuggler (observation). If you pass, you watch a tall humanoid shadow materialize over the torture scene before you snatch a package from the smuggler's rowboat; gain one clue from your neighborhood and one curio. If you fail, you've been made; become WANTED."
        , Test Observation 0 (Seq [clue, curioItem]) wanted
        )
      ]
  , event
      16
      "Merchant District"
      ["Unvisited Isle"]
      [
        ( "River Docks"
        , "The Sheldon enforcer demands to know why you're prowling around; you may give him one remnant to prove that there's something deeply wrong in Arkham. If you do, he confides in you that some of his fellows have taken to wearing robes and chanting; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) clue
        )
      ,
        ( "Tick-Tock Club"
        , "The waitress confides in you that the bouncer has already had to toss six people for fighting tonight, \"so you'd better not start any trouble; he's looking for an excuse to hurt somebody real bad.\" Gain one clue from your neighborhood as she takes your order. You may spend $2 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 2) (health 2)]
        )
      ,
        ( "Unvisited Isle"
        , "Your stomach twists into a knot as the smooth, seductive voice reminds you of the injustices you've faced. Gain one clue from your neighborhood. Where did you get the glass knife in your hand (will)? If you pass, you wrap it in a handkerchief; gain one remnant. If you fail, you cast it wildly to the mud; suffer one horror."
        , Seq [clue, Test Will 0 (remnants 1) (horror 1)]
        )
      ]
  , event
      17
      "Northside"
      ["Arkham Advertiser"]
      [
        ( "Arkham Advertiser"
        , "You watch Doyle Jeffries set the typeface for a story about bodies torn apart by wild dogs; gain one clue from your neighborhood. You may give him one remnant to prove that the \"dogs\" were actually something much worse. If you do, he pays you $3 before incorrectly changing the story to \"bears.\""
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "Oliver offers you a deal: \"My cat won't go near this blasted thing; do you want it?\" You may buy one curio from the display for half price (rounded up). If you do, you find an odd symbol on the object that looks like a tall, thin figure with a curved line across its abdomen; gain one clue from your neighborhood."
        , buyOneHalfThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "You pick up your parcel from Bill Washington, the station's porter; gain one common item. As you walk away, a voice in your head tells you that Bill was rude to you (will). If you pass, you shake off the urge to teach the old man some manners; gain one clue from your neighborhood. If you fail, you nurse a grudge against him for hours."
        , Seq [commonItem, pass Will 0 clue]
        )
      ]
  , event
      18
      "Northside"
      ["Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "A.P. Jessup, staff writer, hires you as a bodyguard while he interviews a tipster out in the alley. Gain $3 and keep an eye out (observation). If you pass, you listen in on the woman's tale about a Sheldon heavy fleecing her husband for protection money; gain one clue from your neighborhood."
        , Seq [money 3, pass Observation 0 clue]
        )
      ,
        ( "Curiositie Shoppe"
        , "The woman at the counter is raving about a ravenous shadow crossing a river of blood. Gain one clue from your neighborhood. Oliver Thomas refuses to tell you what her business here was; he seems rattled, but snaps at you when you ask further. You may buy any number of curios from the display before he shows you the door."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "When you hear the gunshots, you duck behind a bench on the train platform (observation). If you pass, you still don't see the shooter, but you do see the package that the target left behind when they escaped; gain one common item and one clue from your neighborhood."
        , pass Observation 0 (Seq [commonItem, clue])
        )
      ]
  , event
      19
      "Northside"
      ["Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "Minnie Klein demands that you supply her with evidence to support her latest story. You may give her one remnant. If you do, she lets you read the article; gain one clue from your neighborhood. If you don't, she curses and throws a heavy box of printing plates at you; suffer one damage."
        , MayPay (SpendRemnants 1) clue (damage 1)
        )
      ,
        ( "Curiositie Shoppe"
        , "A woman hurls an object at you before she runs off into the night. Gain one curio and try to describe her to Oliver Thomas, the proprietor (observation). If you pass, he tells you that she tried to sell him the stolen object and lost her temper when he wouldn't buy; gain one clue from your neighborhood. If you fail, he suspects you as her accomplice; become WANTED."
        , Seq [curioItem, Test Observation 0 clue wanted]
        )
      ,
        ( "Train Station"
        , "The stranger pounds on the glass of the closed train office, pleading for a ticket out of town. You attempt to reassure them that you can help (influence). If you pass, they tell you about the shadow in their hotel room commanding that they kill the chamber maid; gain one clue from your neighborhood and one ally."
        , pass Influence 0 (Seq [clue, ally])
        )
      ]
  , event
      20
      "Northside"
      ["Arkham Advertiser", "Train Station"]
      [
        ( "Arkham Advertiser"
        , "Amelia Baxter, a visiting reporter, asks you a few questions to corroborate a story, and you try to recall useful details (observation). If you pass, she shares the other witness's account of the violent incident; gain one clue from your neighborhood and $3. If you fail, she sighs and sends you on your way."
        , pass Observation 0 (Seq [clue, money 3])
        )
      ,
        ( "Curiositie Shoppe"
        , "Ryan Dean stops you outside the store, his hair wildly disheveled and his face bruised. \"Hey, friend! Can I interest you in a real quality find?\" You may spend $2 for the bone amulet. If you do, gain one clue from your neighborhood while he explains that some guy jumped him on the road, screeching about \"the Shadow of the Great Messenger.\""
        , mayPay (SpendMoney 2) clue
        )
      ,
        ( "Train Station"
        , "You spot someone on the train platform staring at a humanoid shadow on the wall. Gain one clue from your neighborhood as you try to get their attention (influence). If you pass, they smile weakly, happy for a friendly voice; gain one ally. If you fail, they turn on you suddenly and attempt to bite you; suffer one damage and one horror."
        , Seq [clue, Test Influence 0 ally (harm 1 1)]
        )
      ]
  , event
      21
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "The shadow next to yours promises even more power as it implants the words in your mind; gain one spell. You may gain a DARK PACT. If you do, a wound opens across your stomach like a jack-o'-lantern's smile; suffer one damage, gain one more spell and gain one clue from your neighborhood."
        , Seq [spell, mayPay darkPact (Seq [damage 1, spell, clue])]
        )
      ,
        ( "General Store"
        , "Davy Schoffner watches you warily while you browse the shelves; you may buy any number of common items from the display. If you do, he explains that people have been trying to rob him all week, and the next person gets a bloody nose; gain one clue from your neighborhood as you reassure him that you're on the level."
        , buyAnyThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "The emaciated corpse leers at you, the rictus grin exposing teeth sharpened to points (will). If you pass, you steel yourself to search the cold body thoroughly and find ritual scarring across its belly that matches the grin on its face; gain one clue from your neighborhood and $3 as you pocket a few bits of jewelry."
        , pass Will 0 (Seq [clue, money 3])
        )
      ]
  , event
      22
      "Rivertown"
      ["General Store"]
      [
        ( "Black Cave"
        , "The ritual site still thrums with magical energy; gain one clue from your neighborhood as you test the wards (lore). If you pass, you successfully bypass the arcane protections and search the site; gain one curio. If you fail, the rage overwhelms you; an ally in your space suffers one damage and one horror."
        , Seq [clue, Test Lore 0 curioItem (Custom "sitd-ally-lashes-out")]
        )
      ,
        ( "General Store"
        , "The two men in front of the store are trying to kill each other (observation). If you pass, you see that they are fighting over a simple tin of oatmeal; gain one clue from your neighborhood. You realize that in the confusion, you could probably lift some merchandise from the store; you may become WANTED to gain one common item."
        , Seq [pass Observation 0 clue, mayPay (CostCondition "WANTED") commonItem]
        )
      ,
        ( "Graveyard"
        , "You can feel the eyes watching you from the thick scrub around the overgrown mausoleum (will). If you pass, you remain calm and stare down the hungry ghoul that lurks there while you search the crypt; gain a remnant and one clue from your neighborhood. If you fail, the creature senses your fear and closes in, prompting you to flee."
        , pass Will 0 (Seq [remnants 1, clue])
        )
      ]
  , event
      23
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The cave is deafeningly quiet, but you feel like there's someone here with you (observation). If you pass, you find a body sprawled across a gruesome ritual site, bearing a jagged cut across his belly and with some kind of object placed in the wound; gain one clue from your neighborhood and one curio."
        , pass Observation 0 (Seq [clue, curioItem])
        )
      ,
        ( "General Store"
        , "Nathan, the normally-gregarious delivery boy, glowers at you through quite a shiner on his left eye. When you ask Davy Schoffner about it, he shakes his head sadly and tells you about the boy's fistfight. Gain one clue from your neighborhood. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "The ghoul ahead of you is tearing into a robed figure. Even as he is ripped apart, the man continues chanting about \"the Cruel Hunger.\" Gain one clue from your neighborhood as you try to chase the carrion beast off (strength). If you pass, you drive it away and find a strange totem in the dying man's tight grip; gain one remnant."
        , Seq [clue, pass Strength 0 (remnants 1)]
        )
      ]
  , event
      24
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The markings across the walls are in a script you don't understand, but you can feel the words eroding your composure (will). If you pass, you copy down the symbols before you scrape them away; gain one clue from your neighborhood. If you fail, you are overwhelmed with a terrible rage."
        , pass Will 0 clue
        )
      ,
        ( "General Store"
        , "Davy Schoffner is eager to close before dark so he offers you a bargain to complete the sale swiftly; you may buy one common item from the display for half price (rounded up). If you buy something, he swears that a giant bat stalked him all the way home yesterday; gain one clue from your neighborhood."
        , buyOneHalfThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "You come across the remains of an unknown ritual strewn across the grave markers. Gain one remnant as you gather up the strange supplies (observation). If you pass, you realize that the humanoid shadow among the tombstones isn't your own; gain one clue from your neighborhood. If you fail, you feel a chill creep up your spine, but don't know why."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ]
  ]
