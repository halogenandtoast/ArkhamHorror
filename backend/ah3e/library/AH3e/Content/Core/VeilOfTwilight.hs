{- | Veil of Twilight.

The veil between the worlds is torn, and every tear leaves a scar. Tokens on the
sheet bring the Silver Twilight Lodge's offer (card 20) and the means of mending
a scar (card 21); from there the investigators either work with Carl Sanford
(cards 24-26) or against him (cards 22-23), and doom on the sheet brings the
Lurker at the Threshold through in his place (cards 27 and 28).
-}
module AH3e.Content.Core.VeilOfTwilight (code, scenario, cards, lodgeMonsters, servitor) where

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
code = "veil-of-twilight"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

{- | The monsters the sheet holds back at setup. Card 20 shuffles them into the
deck if nobody takes Carl Sanford's offer; otherwise they stay out of the game.
-}
lodgeMonsters :: [CardCode]
lodgeMonsters =
  [ "lodge-enforcer"
  , "lodge-guardian"
  , "lodge-loyalist"
  , "lodge-seer"
  , "simon-carter"
  , "twilight-sentry"
  , "twilight-supplicant"
  ]

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "Veil of Twilight"
    , expansion = CoreSet
    , startingSpace = spaceIdFor "Ma's Boarding House"
    , reckoningText = "Place one doom in each space with a scar."
    , reckoning = Custom "vot-reckoning"
    , setupMap =
        buildMap
          [nb "Northside", nb "Miskatonic University", nb "Rivertown", nb "Uptown", nb "Southside"]
          [ StreetDef (nb "Northside") BottomRight (nb "Rivertown") Bridge
          , StreetDef (nb "Miskatonic University") SideRight (nb "Rivertown") Scenic
          , StreetDef (nb "Miskatonic University") BottomRight (nb "Uptown") Scenic
          , StreetDef (nb "Rivertown") BottomRight (nb "Southside") Residential
          , StreetDef (nb "Uptown") SideRight (nb "Southside") Residential
          ]
    , monsters =
        [ ("altered-beast", 2)
        , ("whippoorwill", 2)
        , -- every Thrall monster; setup keeps back the boxes that are not in play
          ("altered-servant", 2)
        , ("avian-thrall", 1)
        , ("hulking-thrall", 2)
        , ("icebound-captive", 2)
        , ("lupine-thrall", 1)
        , ("void-touched", 2)
        ]
    , startingMonsters =
        [ ("hulking-thrall", spaceIdFor "Black Cave")
        , ("void-touched", spaceIdFor "Ye Olde Magick Shoppe")
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
          ["Train Station", "Science Building", "Graveyard", "Hangman's Hill", "Ma's Boarding House"]
    , startingMarkers = [(spaceIdFor "Black Cave", "white")]
    , eventCards = [CardCode ("vot-event-" <> pad n) | n <- [1 .. 24 :: Int]]
    , setAside = lodgeMonsters <> ["vot-28"]
    , codex = [2, 20, 21]
    , anomalySet = Just "Fractured Reality"
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = archive <> events <> anomalies <> [servitor]

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
buyOneThen, buyOneHalfThen, buyAnyThen :: Trait -> Effect -> Effect
buyOneThen t = BuyFromDisplay (Just t) FullPrice (Just 1)
buyOneHalfThen t = BuyFromDisplay (Just t) HalfPrice (Just 1)
buyAnyThen t = BuyFromDisplay (Just t) FullPrice Nothing

remnant :: Effect
remnant = remnants 1

archiveCard :: Int -> Text -> Text -> Maybe Text -> CardDef
archiveCard n title front back =
  CardDef
    (CardCode ("vot-" <> tshow n))
    title
    CoreSet
    1
    (ArchiveCard (ArchiveDef (ArchiveNumber n) (ArchiveSide front) (ArchiveSide <$> back)))

archive :: [CardDef]
archive =
  [ archiveCard
      20
      "Threshold"
      "[Objective][Doom] When there is a total of two or more tokens on the scenario sheet (clues and/or doom), flip this card."
      ( Just
          "You have discovered a means of mending the scars in the veil. Flip card 21 before continuing.\nAny investigator may gain a DARK PACT to join the Silver Twilight Lodge. If you join, add cards 24-25 to the codex.\nIf no investigator joins, Carl Sanford declares you an enemy of the Lodge; shuffle the set-aside Lodge monsters into the deck, add one spawn monster and one gate burst token to the mythos cup, and add card 22 to the codex.\nWhether you join or not, return this card to the archive."
      )
  , archiveCard
      21
      "Lasting Scars"
      "White markers are \"scars in the veil.\"\nAfter an anomaly appears, if there is not a scar in that neighborhood, place one scar in the space with the most doom in that neighborhood."
      ( Just
          "To Mend a Scar\nWhite markers are \"scars in the veil.\" After an anomaly appears, if there is not a scar or mended scar in that neighborhood, place one scar in the space with the most doom in that neighborhood.\nFacedown markers are \"mended scars.\"\n[Objective] Action: You may spend two clues from the scenario sheet to mend a scar in your space (flip it facedown). A mended scar does not cause doom during reckoning."
      )
  , archiveCard
      22
      "Go It Alone"
      "[Objective] When a scar is mended for the first time, flip this card and read the \"Eureka Moment!\" effect.\n[Doom] When there is six or more doom on the scenario sheet, flip this card and read the \"Crack in Reality\" effect."
      ( Just
          "Eureka Moment!\nChoose two neighborhoods that do not contain scars or mended scars; place a scar in any space of each of those neighborhoods. Add card 23 to the codex and return this card to the archive.\n\nCrack in Reality\nPlace one scar in any space of a neighborhood that does not contain a scar or mended scar. Read the back of card 26 and return this card to the archive."
      )
  , archiveCard
      23
      "Some Progress"
      "[Objective] When three or more scars have been mended, flip this card and read the \"The Way is Closed\" effect.\n[Doom] When there is eight or more doom on the scenario sheet, flip this card and read the \"Come Undone\" effect."
      ( Just
          "The Way is Closed\nInvestigators win the game!\n\nCome Undone\nRead the back of card 26 and return this card to the archive."
      )
  , archiveCard
      24
      "Twilight Gathers"
      "[Objective] When a scar is mended for the first time, flip this card and read the \"Gathering Power\" effect.\n[Doom] When there is six or more doom on the scenario sheet, flip this card and read the \"The Silver Key\" effect."
      ( Just
          "Gathering Power\nThe group must choose -- stay loyal members of the Lodge, or betray Carl Sanford and leave. If you stay, add card 26 to the codex.\nIf you leave, read the back of card 26.\nRegardless, return this card to the archive.\n\nThe Silver Key\nChoose a neighborhood that does not contain a scar or mended scar; place one scar in any space of that neighborhood. Read the back of card 26 and return this card to the archive."
      )
  , archiveCard
      25
      "Plumb the Void"
      "Action: If you are in a neighborhood with an anomaly, you may step into the void (lore -1). If you pass, you traverse the universe; you move to any space in a neighborhood with an anomaly. If you fail, you become lost in the void; you move to the unstable space and suffer one horror."
      ( Just
          "The Silver Twilight Lodge wins the game! (Investigators who are truly loyal to the Lodge win the game.)"
      )
  , archiveCard
      26
      "Sanford Revealed"
      "After this card is added to the codex, place two scars, each in a neighborhood that does not contain a scar or a mended scar; they may be placed in any space in that neighborhood.\n[Objective] When three or more scars have been mended, flip card 25.\n[Doom] When there is nine or more doom on the scenario sheet, flip this card and read the \"Like Unto a God\" effect."
      ( Just
          "Like Unto a God\nSpawn card 28 (Servitor of Yog-Sothoth epic monster) at the Historical Society. Add card 27 to the codex and return this card to the archive."
      )
  , archiveCard
      27
      "Back to the Wall"
      "Whenever your encounter text includes \"Carl Sanford,\" stop reading that encounter. Instead, the Servitor of Yog-Sothoth moves to your space, engages you, and deals damage and horror.\n[Objective] After the Servitor of Yog-Sothoth has been defeated, flip this card and read the \"Arkham Scarred\" effect.\n[Doom] When there is thirteen or more doom on the scenario sheet, flip this card and read the \"The Key and the Gate\" effect."
      ( Just
          "Arkham Scarred\nInvestigators win the game!\n\nThe Key and the Gate\nInvestigators lose the game!"
      )
  ]

{- | Card 28. Spawned by the back of card 26 at the Historical Society, and three
health heavier for every scar still open. Epic, so it goes back to the archive
rather than to the monster deck when it leaves play.
-}
servitor :: CardDef
servitor =
  CardDef
    "vot-28"
    "Servitor of Yog-Sothoth"
    CoreSet
    1
    ( MonsterCard
        MonsterDef
          { readyName = Nothing
          , spawn = CustomSpaceRule "Spawned by card 26 at the Historical Society"
          , activation = Lurker (PlaceDoomAt SourceSpace (N 1))
          , speed = 0
          , traits = ["Human", "Thrall"]
          , health = 2
          , elite = 2
          , attackSkill = Strength
          , attackModifier = 0
          , evadeModifier = 0
          , damage = 2
          , horror = 2
          , remnant = True
          , keywords = []
          , epic = True
          , text =
              "Elite 2 (Has 2 additional health per investigator.) The Servitor of Yog-Sothoth has an additional three health for each scar that has not been mended. After you disengage this monster, place one doom in your space. Lurker - Place one doom in this space."
          }
    )

{- | One of the twenty-four event cards: the neighborhood it belongs to, the spaces
its doom symbols name, and an encounter for each of that neighborhood's spaces.
-}
event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (CardCode ("vot-event-" <> pad n))
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
      "Miskatonic University"
      ["Observatory"]
      [
        ( "Observatory"
        , "You linger at the observatory after it closes and keep watch for any intruders (observation). If you pass, you spot a member of the Silver Twilight Lodge trying to sneak in and you chase her away; you gain one clue from your neighborhood and remove one doom from any space. If you fail, you fall asleep."
        , pass Observation 0 (Seq [clue, anywhere 1])
        )
      ,
        ( "Orne Library"
        , "Abigail Foreman refuses to grant access to the restricted section of books (influence). If you pass, you convince her to make an exception; you gain one spell and one clue from your neighborhood. If you fail, you waste hours without result; you suffer one horror and become delayed."
        , Test Influence 0 (Seq [spell, clue]) (Seq [horror 1, delayed])
        )
      ,
        ( "Science Building"
        , "The research team shows you the blueprints for a prototype designed to prevent holes in reality. You gain one clue from your neighborhood. If you are willing to describe an otherworldly experience, they will pay you for your time. You may spend one remnant to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ]
  , event
      2
      "Miskatonic University"
      ["Observatory"]
      [
        ( "Observatory"
        , "One of the astronomers has been unconscious for hours. You are not sure how long he will take to recover, but you think he might have some insight into what is happening. You may become delayed to gain one clue from your neighborhood and spawn one clue."
        , mayPay CostDelayed (Seq [clue, SpawnOneClue])
        )
      ,
        ( "Orne Library"
        , "You notice strangers from Dunwich loitering in the library. You gain one clue from your neighborhood. You eavesdrop on the odd visitors (observation). If you pass, you hear their discussion of the Necronomicon; you gain one spell. If you fail, you overhear blasphemous secrets; you become CURSED."
        , Seq [clue, Test Observation 0 spell cursed]
        )
      ,
        ( "Science Building"
        , "It would require a lengthy explanation, but the scientists can show you how the weakening reality has been diminishing you and how to undo those effects. You may become delayed to gain one clue from your neighborhood and focus two skills of your choice, even if this exceeds your focus limit."
        , mayPay CostDelayed (Seq [clue, focusExceed, focusExceed])
        )
      ]
  , event
      3
      "Miskatonic University"
      ["Orne Library", "Orne Library"]
      [
        ( "Observatory"
        , "Professor Tremaine eagerly shares her research notes with you. \"I count several odd disturbances throughout the city,\" she confesses. You remove one doom from up to two different spaces. You examine her notes (observation). If you pass, you spot something she missed; you gain one clue from your neighborhood."
        , Seq [RemoveDoomFrom (DifferentSpaces 2 []) (N 1), pass Observation 0 clue]
        )
      ,
        ( "Orne Library"
        , "The locked gate leading to the restricted section of the library has been pried open. Inside, a few pages remain from a book that has been torn apart. You gain one spell and one clue from your neighborhood. As you read, you grasp what secrets were stolen; you suffer one horror."
        , Seq [spell, clue, horror 1]
        )
      ,
        ( "Science Building"
        , "You agree to do some testing for the researchers. You gain $2. They show you photographs taken of other worlds (observation). If you pass, you make out the forms of specific creatures; you gain one clue from your neighborhood. If you fail, the images are vague and unsettling; you suffer one horror."
        , Seq [money 2, Test Observation 0 clue (horror 1)]
        )
      ]
  , event
      4
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "The observatory is transformed for a fundraiser with elegant decorations and red velvet ropes. You can pay $2 to enter. If you do, you realize the Silver Twilight Lodge has their own motives for hosting the event and help yourself to a \"souvenir;\" you gain one clue from your neighborhood and one remnant."
        , mayPay (SpendMoney 2) (Seq [clue, remnant])
        )
      ,
        ( "Orne Library"
        , "You find a stolen tome next to the body of a thrall that has been torn apart by a dog. You gain one spell. You investigate how the intruder got in (observation). If you pass, you find an underground tunnel; you gain one clue from your neighborhood. If you fail, you find no answers."
        , Seq [spell, pass Observation 0 clue]
        )
      ,
        ( "Science Building"
        , "A visiting professor whose overlarge lab coat and hat hide their face offers to pay you for your findings. You may spend one remnant to gain $3. You try to get a good look at the professor (observation). If you pass, you realize that they are visiting from very far away indeed; you gain one clue from your neighborhood."
        , Seq [mayPay (SpendRemnants 1) (money 3), pass Observation 0 clue]
        )
      ]
  , event
      5
      "Miskatonic University"
      ["Science Building"]
      [
        ( "Observatory"
        , "A cluster of astronomers mutters at the telescope, complaining that their results cannot be correct. You gain one clue from your neighborhood. You sneak a look in the telescope for yourself (lore). If you pass, you note down the unusual configuration of the stars and an extra planet; you gain one remnant."
        , Seq [clue, pass Lore 0 remnant]
        )
      ,
        ( "Orne Library"
        , "Whippoorwills have gathered around the library. You try to interpret what these birds may portend (lore). If you pass, the birds have made a nest of rare books; you gain one spell and one clue from your neighborhood. If you fail, you fear they are here to claim your soul; you become CURSED."
        , Test Lore 0 (Seq [spell, clue]) cursed
        )
      ,
        ( "Science Building"
        , "Without evidence, the scientists cannot establish how the doors between worlds are being opened. These specialists are desperate for a collaborator to provide the materials they need for their research. You may spend one remnant to gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ]
  , event
      6
      "Northside"
      ["Arkham Advertiser"]
      [
        ( "Arkham Advertiser"
        , "You sell a few examples of what you've seen so far to the editor and collect your check. You gain $2. As you're leaving, you hear footsteps (observation). If you pass, you find a terrified copy boy who tells you what he knows; you gain one clue from your neighborhood. If you fail, the footsteps stop suddenly."
        , Seq [money 2, pass Observation 0 clue]
        )
      ,
        ( "Curiositie Shoppe"
        , "As you walk up and down the aisles, each object seems to whisper to you. You may buy any number of curio items from the display. If you buy something, the hushed voice tells you its own history and how it came to be in this shop; you gain one clue from your neighborhood."
        , buyAnyThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "The train passengers are terrified by the portal that has appeared in the station. You gain one clue from your neighborhood. You calm the passengers (influence). If you pass, one of the passengers is intrigued by your investigation; you gain one ally. If you fail, they will never recover; you suffer one horror."
        , Seq [clue, Test Influence 0 ally (horror 1)]
        )
      ]
  , event
      7
      "Northside"
      ["Arkham Advertiser"]
      [
        ( "Arkham Advertiser"
        , "\"I can't just print a story because the whispers I hear whenever I close my eyes tell me so,\" confesses a red-eyed Doyle Jefferies. You may spend one remnant to show Jefferies he isn't going mad. If you do, he shares his story; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Curiositie Shoppe"
        , "You find yourself holding an unfamiliar object. \"It likes you,\" Oliver says, \"it wants me to give you a discount.\" You may buy one curio item from the display for half price (rounded up). If you do, you find a message etched into the object; you gain one clue from your neighborhood."
        , buyOneHalfThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "The train that arrives in a crash of green lightning is clearly from another world. You gain one clue from your neighborhood. The train seems abandoned, and you steel yourself to board (will). If you pass, you find a small box resting in the center of an empty car; you gain one common item."
        , Seq [clue, pass Will 0 commonItem]
        )
      ]
  , event
      8
      "Northside"
      ["Curiositie Shoppe", "Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "\"My travel reporter sent this from someplace called the Plateau of Leng,\" grumbles editor Doyle Jefferies. \"I can't print this! I've never heard of it!\" You gain one clue from your neighborhood. You may spend one remnant to share your own story. If you do, you gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "A woman at the counter is trying to sell objects from her home that she claims have been talking to her. You gain one clue from your neighborhood. She invites you to look through what she's brought and make her an offer. You may buy any number of curio items from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "A passing traveler presses something into your hands. You gain one common item. You stop the traveler and ask for an explanation (influence). If you pass, the traveler explains that the whispers from beyond the threshold wanted you to have it; you gain one clue from your neighborhood."
        , Seq [commonItem, pass Influence 0 clue]
        )
      ]
  , event
      9
      "Northside"
      ["Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "When you step across the threshold, a creature of quivering tendrils looks up from its glass typewriter and asks you a question in an alien tongue. You gain one clue from your neighborhood. You may spend one remnant to deliver your story to the new editor. If you do, it pays with gold coins; you gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "Somewhere in the store, a voice calls to you (observation). If you pass, you find a doll dressed like you next to a map; you gain one clue from your neighborhood. If you fail, you search frantically but find nothing. Whether you pass or not, you find something unexpected in your bag; you gain one curio item."
        , Seq [pass Observation 0 clue, curioItem]
        )
      ,
        ( "Train Station"
        , "Two identical passengers emerge from an otherworldly portal, each claiming the other is a fake (influence). If you pass, you trick the mimic into revealing itself; you gain one ally and one clue from your neighborhood. If you fail, the doppelganger attacks; you suffer one damage and one horror."
        , Test Influence 0 (Seq [ally, clue]) (harm 1 1)
        )
      ]
  , event
      10
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "Minnie Klein snatches her notes from your hand. \"Don't read that,\" she says. \"My camera burned out or something, I can't prove it.\" You may spend one remnant to show her the proof. If you do, she lets you collaborate on the article; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Curiositie Shoppe"
        , "Oliver Thomas notices that a particular object catches your eye. \"Interested? Half off. Blasted thing won't shut up. I can't concentrate!\" You may buy one curio item from the display for half price (rounded up). If you do, it whispers secrets; you gain one clue from your neighborhood."
        , buyOneHalfThen "Curio" clue
        )
      ,
        ( "Train Station"
        , "An improbable train arrives, and an improbable being disembarks and approaches you (will). If you pass, you keep your calm as the being speaks in an alien tongue and hands you something; you gain one common item and one clue from your neighborhood. If you fail, you turn and run."
        , pass Will 0 (Seq [commonItem, clue])
        )
      ]
  , event
      11
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "\"You got anything interesting for me to shoot?\" Minnie Klein asks, hoisting her camera. You may spend one remnant to provide an interesting subject. If you do, she shares similar photos from her collection as she buys the photo rights; you gain $3 and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ,
        ( "Curiositie Shoppe"
        , "You notice a man wearing a Silver Twilight Lodge ring purchasing a collection of oddities from the proprietor, Oliver Thomas. You gain one clue from your neighborhood. \"Are you buying anything, or just spying on my customers?\" asks Oliver. You may buy one curio item from the display."
        , Seq [clue, buyOne "Curio"]
        )
      ,
        ( "Train Station"
        , "An incoming train disappears through a portal to another world. Either test will to immediately jump through the portal or become delayed to take precautions first. If you pass or become delayed, you recover the missing passengers; you gain one ally and one clue from your neighborhood."
        , orPay Will "Become delayed" CostDelayed (Seq [ally, clue])
        )
      ]
  , event
      12
      "Rivertown"
      ["Black Cave", "Black Cave"]
      [
        ( "Black Cave"
        , "Luminescent spheres float all around you, filling your mind with words of an ancient language (lore). If you pass, you understand the eldritch power of these caves; you gain one spell and one clue from your neighborhood. If you fail, the words drive you toward lunacy; you suffer two horror."
        , Test Lore 0 (Seq [spell, clue]) (horror 2)
        )
      ,
        ( "General Store"
        , "Davy Schoffner implores you to come into the empty store and look around. You may buy any number of common items from the display. If you buy something, he tells you the trucks from Boston can't make it through to Arkham; you gain one clue from your neighborhood."
        , buyAnyThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "You see several old graves standing empty, their bodies carefully exhumed. You gain one clue from your neighborhood. One of those decaying bodies shuffles past you, its eyes white and unseeing (strength). If you pass, you bring the thing down; you gain one remnant. If you fail, it escapes you."
        , Seq [clue, pass Strength 0 remnant]
        )
      ]
  , event
      13
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "A group of robed figures performs a profane ritual, and glowing spheres begin to fill the cave. You gain one clue from your neighborhood. The spheres whisper to you (will). If you pass, you're able to both listen to the voice and retain your sense of self; you gain one spell. If you fail, you remember nothing."
        , Seq [clue, pass Will 0 spell]
        )
      ,
        ( "General Store"
        , "People are desperate for food, willing to sell their belongings for little money. You may buy one common item from the display for half price (rounded up). If you do, they tell you about the vermin with milky-white eyes infesting their homes; you gain one clue from your neighborhood."
        , buyOneHalfThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "Something goes \"thump\" inside a coffin under a sycamore tree. You attempt to pry the lid off (strength). If you pass, you find the gravedigger, covered in sweat and raving about white-eyed corpses; you gain $3 and one clue from your neighborhood. If you fail, the coffin lid stays shut."
        , pass Strength 0 (Seq [money 3, clue])
        )
      ]
  , event
      14
      "Rivertown"
      ["Black Cave"]
      [
        ( "Black Cave"
        , "A voice from the void beyond surrounds you and fills your thoughts. You gain one spell. You struggle to understand the connection between this new presence in your mind and the larger mystery (lore). If you pass, it all becomes clear; you gain one clue from your neighborhood."
        , Seq [spell, pass Lore 0 clue]
        )
      ,
        ( "General Store"
        , "A crowd has gathered at the store, telling stories about finding rats everywhere. You gain one clue from your neighborhood. You look around the store, searching for anything that might help. You may buy any number of common items from the display."
        , Seq [clue, buyAny "Common"]
        )
      ,
        ( "Graveyard"
        , "The gravedigger's feet drag as he shuffles toward you, his eyes white as milk. You gain one clue from your neighborhood. The gravedigger holds up a shovel and points at a patch of earth -- apparently the usual job offer is still on (will). If you pass, you dig; you gain $3. If you fail, you flee."
        , Seq [clue, pass Will 0 (money 3)]
        )
      ]
  , event
      15
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The strange glowing presence in the cave calls to you, offering you untold knowledge if you'll venture deeper inside (will). If you pass, you master your own mind and find the remains of an arcane ritual; you gain one remnant and one clue from your neighborhood. If you fail, you wander in a daze."
        , pass Will 0 (Seq [remnant, clue])
        )
      ,
        ( "General Store"
        , "\"This one's been gnawed a little,\" says Davy Schoffner. \"Why don't you just take it;\" you gain one common item with value three or less. You look around for who or what is doing the gnawing (observation). If you pass, you find a large rat with milk-white eyes; you gain one clue from your neighborhood."
        , Seq [GainE (AnItemValued (Just "Common") (AtMost 3)), pass Observation 0 clue]
        )
      ,
        ( "Graveyard"
        , "A man with silver-arrow cufflinks asks you to dig up a particular grave (strength). If you pass, when you hear the thing in the coffin moving the man gives you your payment; you gain $3 and one clue from your neighborhood. If you fail, the man checks his watch and leaves before you're done."
        , pass Strength 0 (Seq [money 3, clue])
        )
      ]
  , event
      16
      "Rivertown"
      ["Graveyard"]
      [
        ( "Black Cave"
        , "The cultist you are following vanishes through an impossible door carved into the cavern wall. You gain one clue from your neighborhood. There must be some way to open it (lore). If you pass, you open the door and find a trove of cult materials; you gain one curio item. If you fail, you give up in disgust."
        , Seq [clue, pass Lore 0 curioItem]
        )
      ,
        ( "General Store"
        , "The item looks fairly normal, but it's labeled in a script you don't recognize -- you're not even sure it's human. You may buy one common item from the display. If you do, Davy Schoffner tells you that the truck that delivered it \"wasn't from Boston, or Earth;\" you gain one clue from your neighborhood."
        , buyOneThen "Common" clue
        )
      ,
        ( "Graveyard"
        , "You find a thrall that isn't moving anymore. You gain one remnant. You search the corpse (will). If you pass, you find a faded telegram; you gain one clue from your neighborhood. If you fail, the whole situation makes your stomach churn; you suffer one horror."
        , Seq [remnant, Test Will 0 clue (horror 1)]
        )
      ]
  , event
      17
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "An occultist is examining the symbols that appeared on one of the artifacts (influence). If you pass, she carefully guides you through their significance; you gain one clue from your neighborhood and one curio item. If you fail, the sight of the symbols burns your eyes; you suffer two damage."
        , Test Influence 0 (Seq [clue, curioItem]) (damage 2)
        )
      ,
        ( "Ma's Boarding House"
        , "Ma tells you a lot of suspicious-looking strangers have been around lately. She is happy to give you a discounted meal just so she feels safe and has someone to talk to about what's been happening. You may spend $1 to recover three health and gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [myHealth 3, clue])
        )
      ,
        ( "South Church"
        , "A starving family is praying within the church. You may spend $1 to buy them a meal. If you do, Father Michael sees your generosity and shares a document from 1704 that you may find helpful, \"the Whately Bible;\" you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ]
  , event
      18
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "Mr. Peabody is struggling to translate a historical text that is written with unusually exotic symbolism. You may spend one remnant to offer up some of your own findings to aid the translation. If you do, the curator is grateful; you gain one curio item and one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [curioItem, clue])
        )
      ,
        ( "Ma's Boarding House"
        , "A doctor is examining a body found in one of the rooms. \"You could use a little help,\" he says. You or an ally recovers two health. In return you investigate the mysterious death (observation). If you pass, you find evidence of a hex; you gain one clue from your neighborhood. If you fail, you have no answers."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "South Church"
        , "Several parishioners are gathered around a fallen gargoyle, talking in hushed tones. \"I'd swear I saw it flap its wings before it fell.\" You gain one clue from your neighborhood. Father Michael is looking for funds to repair the damaged roof. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ]
  , event
      19
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "The museum is closed, but the crackling lights emanating from the windows speak for themselves. You gain one clue from your neighborhood. Someone else skulking about in the dark spots you and flees (influence). If you pass, you convince the person to stop and talk; you gain one ally."
        , Seq [clue, pass Influence 0 ally]
        )
      ,
        ( "Ma's Boarding House"
        , "You open the door to your room and find it seems to lead to the audient void. After closing it and opening it again, all seems normal. You gain one clue from your neighborhood. You stagger downstairs and ask for dinner. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "South Church"
        , "You seek refuge in the church. You or an ally recovers two sanity. You watch the colored light from the windows play on the floor (observation). If you pass, you realize the sun is streaming in windows on both sides of the church at once; you gain one clue from your neighborhood."
        , Seq [sanity 2, pass Observation 0 clue]
        )
      ]
  , event
      20
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "\"Don't bother,\" says a person coming down the front steps. \"Everyone inside has gone mad -- it's just you and me now.\" You gain one ally. You may spend one remnant to compare notes with your new friend. If you do, you learn the lunatics have milky eyes; you gain one clue from your neighborhood."
        , Seq [ally, mayPay (SpendRemnants 1) clue]
        )
      ,
        ( "Ma's Boarding House"
        , "You may spend $1 for you or an ally to recover three health. The other guests are odd, listening to voices only they can hear, their bodies spasming and bulging in unnatural ways, and their eyes turning milky white. You gain one clue from your neighborhood."
        , Seq [mayPay (SpendMoney 1) (health 3), clue]
        )
      ,
        ( "South Church"
        , "The church is quietly humming with congregants. You may spend $1 to buy and light a votive candle. If you do, you feel a sense of peace and notice that the candles are arrayed in the shape of a crooked star; you or an ally recovers three sanity and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [sanity 3, clue])
        )
      ]
  , event
      21
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "A dozen hooded cultists leap out of the woods, armed with knives (strength)! If you pass, they scatter and one of them drops his personal possessions; you gain one common item and one clue from your neighborhood. If you fail, they leave you cut up and bleeding; you suffer two damage."
        , Test Strength 0 (Seq [commonItem, clue]) (damage 2)
        )
      ,
        ( "St. Mary's Hospital"
        , "A woman with milky eyes leaves the pharmacy, laden with drugs. You may spend $1 to bribe the orderly. If you do, she fetches some painkillers and tells you which psychoactive drug the woman bought; you or an ally recovers three health and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 3, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The buildings on either side of the shop keep flickering and changing. You gain one clue from your neighborhood. Miriam Beecher is behind the counter as if nothing is wrong. Reveal the top three spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
        , Seq [clue, spells 3 Nothing FullPrice]
        )
      ]
  , event
      22
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "You disguise yourself as a cultist and spy on their ritual. You gain one clue from your neighborhood. Their leader starts asking you questions (will). If you pass, he entrusts an item to you for safekeeping; you gain one common item. If you fail, you lose your cool and flee; you suffer one horror."
        , Seq [clue, Test Will 0 commonItem (horror 1)]
        )
      ,
        ( "St. Mary's Hospital"
        , "There's a problem with your admissions paperwork. You may spend $1 to ask Nurse Sharon to make the problem go away. If you do, while you receive treatment a patient bellows \"Ia Yog-Sothoth!\" and goes berserk; you or an ally recovers three health and you gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 3, clue])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Carl Sanford is loitering outside and appears to be in an ebullient mood. He offers to teach you \"a few tricks;\" you gain one spell. The well-dressed men that follow him around are suspicious (observation). If you pass, you notice their eyes are white as milk; you gain one clue from your neighborhood."
        , Seq [spell, pass Observation 0 clue]
        )
      ]
  , event
      23
      "Uptown"
      ["Ye Olde Magick Shoppe", "Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "There might be something underneath this enormous stone altar in the woods (strength). If you pass, you shift the altar and find a misshapen corpse and evidence of a ritual sacrifice; you gain one remnant and one clue from your neighborhood. If you fail, the altar will not budge."
        , pass Strength 0 (Seq [remnant, clue])
        )
      ,
        ( "St. Mary's Hospital"
        , "You pause on the threshold, where a voice whispers of unlimited potential and a union of all fractured worlds. You gain one clue from your neighborhood. Nurse Sharon snaps you from your reverie by shoving paperwork into your hands. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "A man brings in journals stolen from the Silver Twilight Lodge. The shop will not buy illicit goods, but you can. Reveal the top spell in the deck. You may buy it. If you do, many passages in the journal hint at Carl Sanford's true intentions; you gain one clue from your neighborhood."
        , Seq [spells 1 (Just 1) FullPrice, clue]
        )
      ]
  , event
      24
      "Uptown"
      ["Ye Olde Magick Shoppe"]
      [
        ( "Hangman's Hill"
        , "You find a hidden ritual site where you find artifacts of glass and bronze. You gain one remnant. A column of chanting, robed figures approaches. You duck behind a stone pillar and keep as quiet as you can (will). If you pass, you hear the whole ritual; you gain one clue from your neighborhood."
        , Seq [remnant, pass Will 0 clue]
        )
      ,
        ( "St. Mary's Hospital"
        , "Doctor Maheswaran is happy to treat you. You or an ally recovers two health. While she works you spy some notes peeking out of her bag that interest you (observation). If you pass, you sneak a copy of her description of \"enthralled\" patients; you gain one clue from your neighborhood."
        , Seq [health 2, pass Observation 0 clue]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "The shop's crystal ball has grown very powerful. You gain one clue from your neighborhood. You find out that the proprietor has been using it to expand the shop's inventory. Reveal the top three spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
        , Seq [clue, spells 3 Nothing FullPrice]
        )
      ]
  ]

-- Fractured Reality anomalies
anomaly :: Int -> [((Int, Maybe Int), Text, Effect)] -> CardDef
anomaly n sections =
  CardDef
    (CardCode ("fractured-reality-" <> pad n))
    "Fractured Reality"
    CoreSet
    1
    ( AnomalyCard
        (AnomalyDef "Fractured Reality" [(range, Encounter txt eff) | (range, txt, eff) <- sections])
    )

anomalies :: [CardDef]
anomalies =
  [ anomaly
      1
      [
        ( (0, Just 1)
        , "Lights flicker and pop briefly before every electrical device stops working. Repairs will be costly, but once the lights are restored, you see where the arcane forces broke through the barriers between worlds. You may spend $1 to remove one doom from any space in your neighborhood."
        , mayPay (SpendMoney 1) (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "Spiders, insects, and worms begin crawling out of your clothes, covering the entire neighborhood (will). If you pass, you wait calmly until they all burrow down into the ground; you remove one doom from your space. If you fail, you cover your eyes and scream for hours; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "With each step, you flicker to another alien world beneath a boiling sky (lore -2). If you pass, you find a pattern to your movements and navigate safely home; you remove three doom from your space and gain one remnant. If you fail, you feel a presence travel with you; you become CURSED."
        , Test Lore (-2) (Seq [here 3, remnant]) cursed
        )
      ]
  , anomaly
      2
      [
        ( (0, Just 1)
        , "You find various objects laid out in the shape of a glyph. You think you could change the shape of the glyph to be helpful, rather than harmful (lore). If you pass, you form the Elder Sign; you remove one doom from any space in your neighborhood. If you fail, you neither help nor harm the situation."
        , pass Lore 0 (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "The reports of an enormous wolf in the area are true. As the massive creature pounces on you, you see that its eyes are covered by a purplish milky film. You suffer two damage. The beast then falls apart, leaving only chunks of itself. You remove two doom from your space and gain one remnant."
        , Seq [damage 2, here 2, remnant]
        )
      ,
        ( (3, Nothing)
        , "Luminous spheres fill the air and you feel the presence of the Lurker at the Threshold. It offers you power at a price. You may gain a DARK PACT condition to accept the price. If you do, you gain an alien stone as a record of the deal; remove three doom from your space and gain one remnant."
        , mayPay (CostCondition "DARK PACT") (Seq [here 3, remnant])
        )
      ]
  , anomaly
      3
      [
        ( (0, Just 1)
        , "A strange person stands half in this world, half in another. Their shining mirror holds a crowd of citizens enthralled. You attempt to break the mirror (will). If you succeed, the wizard shrieks and vanishes; you remove one doom from any space in your neighborhood."
        , pass Will 0 (nearby 1)
        )
      ,
        ( (2, Just 2)
        , "This creature clearly did not come from Earth. You examine the corpse of a spider-headed, bear-like beast (will -1). If you pass, you dispose of it before anyone sees; you remove two doom from your space and gain one remnant. If you fail, you cannot bear to look at the abomination; you suffer two horror."
        , Test Will (-1) (Seq [here 2, remnant]) (horror 2)
        )
      ,
        ( (3, Nothing)
        , "You turn a corner and find yourself inside a six-dimensional maze twisting in all directions (lore -1). If you pass, you find your way to the center and take hold of the golden orb there; you remove two doom from your space and gain one remnant. If you fail, you become lost; you become delayed."
        , Test Lore (-1) (Seq [here 2, remnant]) delayed
        )
      ]
  , anomaly
      4
      [
        ( (0, Just 1)
        , "A strange mist permeates this area, making people ill (will). If you succeed, you manage to keep your head clear and get people out of the area; you may remove one doom from any space in your neighborhood. If you fail, you are attacked by white-eyed citizens; you suffer one damage."
        , Test Will 0 (nearby 1) (damage 1)
        )
      ,
        ( (2, Just 2)
        , "The air shimmers and you catch a glimpse of an alien horizon. You invoke ancient powers to seal this rip in reality (lore). If you pass, the disruption fades; you remove one doom from your space. If you fail, something in that other world looks you squarely in the eyes; you suffer one horror."
        , Test Lore 0 (here 1) (horror 1)
        )
      ,
        ( (3, Nothing)
        , "A huge, invisible creature stampedes through the streets. You invoke the ancient words to banish the horror (lore -2). If you pass, it vanishes, leaving only footprints; you remove three doom from your space and gain one remnant. If you fail, you are crushed as it passes by; you suffer two damage."
        , Test Lore (-2) (Seq [here 3, remnant]) (damage 2)
        )
      ]
  , anomaly
      5
      [
        ( (0, Just 1)
        , "The mirrors here each reflect another world (lore). If you succeed, you cover each mirror you can find with cloth until things normalize; you remove one doom from any space in your neighborhood. If you fail, there are always more mirrors; you suffer one horror."
        , Test Lore 0 (nearby 1) (horror 1)
        )
      ,
        ( (2, Just 2)
        , "You try to prove your membership in the Silver Twilight Lodge (lore -1). If you pass, you steal the tools of their invocation; you remove two doom from your space and gain one remnant. If you fail, they use your blood to invoke the Key and the Gate; you suffer one damage and one horror."
        , Test Lore (-1) (Seq [here 2, remnant]) (harm 1 1)
        )
      ,
        ( (3, Nothing)
        , "You watch your hand vanish as it presses through air thick as taffy (will -1). If you pass, you step through the weak spot between worlds and banish the evil there; you remove two doom from your space and gain one remnant. If you fail, you feel as if all of reality is tearing apart; you suffer three horror."
        , Test Will (-1) (Seq [here 2, remnant]) (horror 3)
        )
      ]
  , anomaly
      6
      [
        ( (0, Just 0)
        , "You slowly and carefully try to open the puzzle box that mysteriously appeared here (lore). If you pass, the box opens revealing its odd contents; you remove one doom from any space in your neighborhood. If you fail, a small hidden needle injects you with poison; you suffer two damage."
        , Test Lore 0 (nearby 1) (damage 2)
        )
      ,
        ( (1, Just 2)
        , "Independent of your control, one of your hands begins writing and drawing abominable things. You suffer two horror. When you regain control again you burn the worst of the results and catalog the rest. You remove two doom from your space and gain one remnant."
        , Seq [horror 2, here 2, remnant]
        )
      ,
        ( (3, Nothing)
        , "Yog-Sothoth himself abruptly opens your mind to the infinite horrors of all of time and space (will -2). If you pass, you resist by focusing on a trivial object; you remove three doom from your space and gain one remnant. If you fail, your mind is shattered by the vision; you suffer two horror."
        , Test Will (-2) (Seq [here 3, remnant]) (horror 2)
        )
      ]
  , anomaly
      7
      [
        ( (0, Just 0)
        , "The radio plays a sickly, hypnotic melody (will). If you pass, you hear the secrets hidden in the music; you remove one doom from any space in your neighborhood. If you fail, you wake up covered in wounds you can't explain; you suffer one damage and one horror."
        , Test Will 0 (nearby 1) (harm 1 1)
        )
      ,
        ( (1, Just 1)
        , "A crowd has gathered, but when they speak to each other, the words are gibberish. You suspect they are speaking in code (lore). If you pass, you unlock the Silver Twilight Lodge's cipher and spoil their plans; you remove one doom from your space. If you fail, the words remain impenetrable."
        , pass Lore 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "Every door opens onto the same hallway, itself lined with dozens of doors (lore -1). If you pass, you find the one door that leads you out and smash it down behind you; you remove two doom from your space and gain one remnant. If you fail, you're not alone in here; you suffer three damage."
        , Test Lore (-1) (Seq [here 2, remnant]) (damage 3)
        )
      ]
  , anomaly
      8
      [
        ( (0, Just 0)
        , "Everything seems normal here, but a hunched, white-eyed figure passes by, stopping periodically to draw sigils in chalk (lore). If you pass, you expertly break the sigils; you remove one doom from any space in your neighborhood. If you fail, touching them was unwise; you become CURSED."
        , Test Lore 0 (nearby 1) cursed
        )
      ,
        ( (1, Just 1)
        , "Your thoughts are filled with thousands of voices, each trying to compel you to submit to their arcane power (will). If you pass, you silence these voices and reassert your own self-control; you remove one doom from your space. If you fail, the voices read your thoughts and then subside."
        , pass Will 0 (here 1)
        )
      ,
        ( (2, Nothing)
        , "Three large stones form a doorway, and through it you see an infinite void, lit only by luminescent spheres like floating bubbles. Dismantling the doorway will take time, but it must be done. You may become delayed to remove two doom from your space and gain one remnant."
        , mayPay CostDelayed (Seq [here 2, remnant])
        )
      ]
  , anomaly
      9
      [
        ( (0, Just 0)
        , "Your sleep is troubled by powerful dreams of faraway places and a sinister voice that promises you power (will). If you pass, you awaken and know what to do; you remove one doom from any space in your neighborhood. If you fail, you wake with a strange scar; you suffer one damage and one horror."
        , Test Will 0 (nearby 1) (harm 1 1)
        )
      ,
        ( (1, Just 1)
        , "The sinister beings erecting the standing monoliths appear to be from another world, and are accepting payment in alien artifacts. You may spend one remnant to remove one doom from your space and one doom from another space in your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [here 1, RemoveDoomFrom OtherSpaceInYourNeighborhood (N 1)])
        )
      ,
        ( (2, Nothing)
        , "Arcane energy pours out of a hole in the earth. You try to harness this power (lore -1). If you pass, you reshape the ground to seal the rift; you remove two doom from your space and gain one remnant. If you fail, the power throws you like a ragdoll; you suffer one damage and one horror."
        , Test Lore (-1) (Seq [here 2, remnant]) (harm 1 1)
        )
      ]
  , anomaly
      10
      [
        ( (0, Just 0)
        , "A robed figure sits behind a chessboard (lore). If you pass, you best him with a clever gambit and he tells you what he knows of the Lurker at the Threshold; you remove one doom from any space in your neighborhood. If you fail, you feel your defeat foretells something awful; you suffer two horror."
        , Test Lore 0 (nearby 1) (horror 2)
        )
      ,
        ( (1, Just 1)
        , "A powerful voice seizes control of your mind, ordering you to perform seemingly random tasks (will). If you pass, you resist the voice; you remove one doom from your space. If you fail, you watch in silence as your hands unlock a door here, place a stone there; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (2, Nothing)
        , "An alien world bleeds into your own and large stones fly through the air! You try to undo the spell behind this intrusion (lore -2). If you pass, reality reasserts itself; you remove three doom from your space and gain one remnant. If you fail, the stones fly at you; you suffer two damage."
        , Test Lore (-2) (Seq [here 3, remnant]) (damage 2)
        )
      ]
  , anomaly
      11
      [
        ( (0, Just 0)
        , "A dog with milky-white eyes approaches, growling and showing its teeth. You stare it down (will). If you pass, at length the dog's eyes clear and it wanders off, wagging its tail; you remove one doom from any space in your neighborhood. If you fail, you blink and the dog bites you; you suffer two damage."
        , Test Will 0 (nearby 1) (damage 2)
        )
      ,
        ( (1, Just 1)
        , "A pale man wearing old-fashioned clothes lurks in the area. You think you recognize him (lore). If you pass, he falls to ash when you tell him he died centuries ago; you remove one doom from your space. If you fail, he calls you by name and promises your death; you suffer one horror."
        , Test Lore 0 (here 1) (horror 1)
        )
      ,
        ( (2, Nothing)
        , "The archway before you leads to an alien world, and your feet are dragging you toward it (will -1). If you pass, you manage to resist the pull long enough to destroy the arch; you remove two doom from your space and gain one remnant. If you fail, you step through; you suffer one damage and one horror."
        , Test Will (-1) (Seq [here 2, remnant]) (harm 1 1)
        )
      ]
  , anomaly
      12
      [
        ( (0, Just 0)
        , "There's a pattern to the odd occurrences in your neighborhood (lore). If you pass, you find the epicenter by carefully correlating eyewitness accounts; you remove one doom from any space in your neighborhood. If you fail, you fear the pattern is you; you suffer two horror."
        , Test Lore 0 (nearby 1) (horror 2)
        )
      ,
        ( (1, Just 1)
        , "The plants here move gently without any breeze, their sweet-smelling nectar pulling you closer (will). If you pass, you realize that the plants don't belong on Earth and destroy them; you remove one doom from your space. If you fail, you can't stop yourself from drinking their nectar; you suffer one horror."
        , Test Will 0 (here 1) (horror 1)
        )
      ,
        ( (2, Nothing)
        , "You float in a void, the real world dimly visible above you as if above the surface of the sea (will -2). If you pass, you remember yourself and push through to your world; you remove three doom from your space and gain one remnant. If you fail, you wonder why you cannot float forever; you suffer two horror."
        , Test Will (-2) (Seq [here 3, remnant]) (horror 2)
        )
      ]
  ]
