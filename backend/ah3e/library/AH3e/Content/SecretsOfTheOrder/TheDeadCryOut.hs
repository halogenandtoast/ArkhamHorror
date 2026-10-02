{- | The Dead Cry Out.

The Underworld is on the board from the start, joined to Arkham by nothing but a
derelict portal into Uptown and a hidden path standing in the corner where
Northside, Easttown and the Underworld meet.

Bystanders are the scenario: facedown ally cards the gugs hunt instead of the
investigators, laid out three at setup and one more every reckoning.
-}
module AH3e.Content.SecretsOfTheOrder.TheDeadCryOut (code, scenario, cards) where

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
code = "the-dead-cry-out"

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

scenario :: ScenarioDef
scenario =
  ScenarioDef
    { code = code
    , name = "The Dead Cry Out"
    , expansion = SecretsOfTheOrder
    , startingSpace = spaceIdFor "Train Station"
    , reckoningText =
        "Place one ally card facedown in the street nearest the unstable space; then spawn one monster in the unstable space."
    , reckoning = Custom "the-dead-cry-out-reckoning"
    , setupMap =
        buildMapOf
          [ nb "Northside"
          , nb "Easttown"
          , nb "Miskatonic University"
          , nb "The Underworld"
          , nb "French Hill"
          , nb "Uptown"
          , nb "Southside"
          ]
          [ StreetDef (nb "Northside") SideRight (nb "Easttown") Residential
          , StreetDef (nb "Northside") BottomLeft (nb "Miskatonic University") Bridge
          , StreetDef (nb "Easttown") BottomRight (nb "French Hill") Bridge
          , StreetDef (nb "Miskatonic University") BottomRight (nb "Uptown") Scenic
          , StreetDef (nb "Uptown") SideRight (nb "Southside") Residential
          , StreetDef (nb "French Hill") BottomLeft (nb "Southside") Scenic
          ]
          noPieces
            { -- the portal's own icon, which setup turns along with the tile; the
              -- rules' example crosses it as a damage border out into the Vale of Pnath
              thresholds =
                [ThresholdTile (nb "The Underworld") BottomLeft (nb "Uptown") DerelictPortal [HazardDamage]]
            , -- the hidden path stands in the corner those three tiles share, so an
              -- investigator on it may step into any of them but not into the street;
              -- each of its three borders carries an icon, and setup turns the tile
              corners =
                [ CornerTile
                    [nb "The Underworld", nb "Northside", nb "Easttown"]
                    HiddenPath
                    [HazardDamage, HazardHorror, HazardFocus]
                ]
            }
    , monsters =
        [ ("abyssal-servant", 1)
        , ("coursing-hound", 1)
        , ("flesh-eater", 2)
        , -- the three Haunting Dead, which are shrouded and so named on the sheet by
          -- the face they show while ready
          ("screaming-haunt", 1)
        , ("weeping-haunt", 1)
        , ("cacophonous-haunt", 1)
        , ("nightmarish-fiend", 1)
        , ("swift-byakhee", 1)
        , ("tunneling-dhole", 1)
        , ("vicious-glutton", 2)
        , -- every gug and ghast monster
          ("gluttonous-giant", 1)
        , ("menacing-bulk", 1)
        , ("bloody-titan", 1)
        , ("crazed-fiend", 1)
        , ("taloned-cannibal", 1)
        ]
    , startingMonsters =
        [ ("taloned-cannibal", spaceIdFor "Bayfriar Gardens")
        , ("vicious-glutton", spaceIdFor "South Church")
        ]
    , mythosCup =
        [ (SpreadDoomToken, 4)
        , (SpawnMonsterToken, 2)
        , (SpawnClueToken, 2)
        , (ReadHeadlineToken, 2)
        , (GateBurstToken, 1)
        , (ReckoningToken, 1)
        , (BlankToken, 2)
        ]
    , startingDoom =
        map
          spaceIdFor
          [ "Train Station"
          , "Police Station"
          , "Orne Library"
          , "Vale of Pnath"
          , "Duterte Funeral Home"
          , "Ye Olde Magick Shoppe"
          , "Historical Society"
          ]
    , startingMarkers = []
    , startingBystanders =
        map spaceIdFor ["Orne Library", "Silver Twilight Lodge", "St. Mary's Hospital"]
    , eventCards = [eventCode n | n <- [1 .. 28]]
    , -- the two gug priests the codex spawns, and the three Underworld cards that only
      -- join that deck once the hunt for the phylactery begins
      setAside = [CardCode ("archive-" <> tshow n) | n <- [145 .. 149 :: Int]]
    , codex = [1, 135, 136, 137]
    , anomalySet = Nothing
    , terrorSet = Nothing
    }

cards :: [CardDef]
cards = fromBox SecretsOfTheOrder events

eventCode :: Int -> CardCode
eventCode n = CardCode ("the-dead-cry-out-event-" <> pad n)

-- | Zero padded, so the card codes sort the way the cards are numbered.
pad :: Int -> Text
pad n = if n < 10 then "0" <> tshow n else tshow n

clue :: Effect
clue = GainE ClueFromNeighborhood

-- | "Remove one doom from any space", which only offers the spaces holding any.
anywhere :: Int -> Effect
anywhere n = RemoveDoomFrom AnySpace (N n)

-- | Miriam's shelves, where the clue waits on buying something.
shoppeOneHalf :: Effect
shoppeOneHalf = Custom "soto-spell-market:one-half"

event :: Int -> Text -> [Text] -> [(Text, Text, Effect)] -> CardDef
event n hood dooms encounters =
  CardDef
    (eventCode n)
    ("Event " <> tshow n <> "/28")
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
      ["Police Station", "Police Station"]
      [
        ( "Hibb's Roadhouse"
        , "The music is soothing. You or an ally may recover two sanity. You hear a scratching sound beneath the melody (observation). If you pass, you follow the noise and startle a horrid, winged beast from its roost; gain one clue from your neighborhood. If you fail, the sound grates inside your head; become FATIGUED."
        , Seq [sanity 2, Test Observation 0 clue fatigued]
        )
      ,
        ( "Police Station"
        , "Several officers scramble around the station (observation). If you pass, you realize one of the cell doors has been ripped from its hinges and something smeared with blood rests on the floor; gain one clue from your neighborhood and one common item. If you fail, an officer notices you and barks at you to get out."
        , pass Observation 0 (Seq [clue, commonItem])
        )
      ,
        ( "Velma's Diner"
        , "\"My best server went missing yesterday. I worry for her two boys,\" Velma says as she takes your order. You may spend $1 for you or an ally to recover two health. If you do, Velma continues, \"She had been talking about wanting to see the mountains, poor thing. Just needed a break and now this;\" gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ]
  , event
      2
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "You scan the room for Old Man Hibbard (observation). If you pass, you spot him holding court, proclaiming, \"It's all the missing folk. Can't expect 'em to have a good time if half of 'em are gone! But, I s'pose we can try;\" gain one clue from your neighborhood and you or an ally may recover two sanity."
        , pass Observation 0 (Seq [clue, sanity 2])
        )
      ,
        ( "Police Station"
        , "Missing person posters line the bulletin board. Gain one clue from your neighborhood. You take a closer look (observation). If you pass, you notice the missing are usually related by blood and also spot something left behind on a lobby chair; gain one common item. If you fail, you realize most of the missing look strikingly like you; suffer one horror."
        , Seq [clue, Test Observation 0 commonItem (horror 1)]
        )
      ,
        ( "Velma's Diner"
        , "\"Order up!\" the cook shouts. You or an ally may recover two health. People seem apprehensive (observation). If you pass, you notice one man tearing up in a booth while looking at several photos in his wallet, \"All gone, all of 'em,\" he mutters; gain one clue from your neighborhood. If you fail, you eat your meatloaf in glum silence."
        , Seq [health 2, pass Observation 0 clue]
        )
      ]
  , event
      3
      "Easttown"
      ["Hibb's Roadhouse"]
      [
        ( "Hibb's Roadhouse"
        , "The rafters groan above you and a viscous dribble of brownish saliva plops onto your shoulder, but when you look up, you see nothing. Gain one clue from your neighborhood. Seemingly unaware, a server cheerily asks you if you'd like a drink. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ,
        ( "Police Station"
        , "The staff sergeant mutters stern orders to her subordinates and you catch her eye (influence). If you pass, you explain how you might be able to help and she nods, handing you a piece of evidence to take a look at; gain one clue from your neighborhood and one common item. If you fail, she has you thrown out on the street; suffer two damage."
        , Test Influence 0 (Seq [clue, commonItem]) (damage 2)
        )
      ,
        ( "Velma's Diner"
        , "\"The man from down the road said it was huge!\" a patron exclaims from a nearby booth. Gain one clue from your neighborhood. \"It had these massive claws\8212no, not claws, talons! I swear it's what he said!\" Velma walks over, rolls her eyes, and asks if she can get you the special. You may spend $1 for you or an ally to recover three health."
        , Seq [clue, mayPay (SpendMoney 1) (health 3)]
        )
      ]
  , event
      4
      "French Hill"
      ["Bayfriar Gardens"]
      [
        ( "Bayfriar Gardens"
        , "A stranger scrambles past you and drops a rune-covered pendant. Gain one remnant. You look around (observation). If you pass, you see something massive stumble through the hedge maze; gain one clue from your neighborhood. If you fail, a deep grunt makes you reconsider; suffer one horror."
        , Seq [remnants 1, Test Observation 0 clue (horror 1)]
        )
      ,
        ( "Duterte Funeral Home"
        , "Samuel scratches his head as he looks at a new body that just arrived. \"Just don't know what coulda made those kinds of marks,\" Samuel murmurs. You may spend one remnant to explain what likely caused the damage. If you do, he mumbles, \"That's what Madeline's other guest said;\" gain one clue from your neighborhood and one ally."
        , mayPay (SpendRemnants 1) (Seq [clue, ally])
        )
      ,
        ( "Silver Twilight Lodge"
        , "A few Lodge initiates huddle together (observation). If you pass, you catch a glimpse of the amorphous, glimmering tear in reality they're staring at and notice one of the members drop something in their awe; gain one clue from your neighborhood and one curio. If you fail, the members' circle is too tight for you to see."
        , pass Observation 0 (Seq [clue, curioItem])
        )
      ]
  , event
      5
      "French Hill"
      ["Bayfriar Gardens", "Duterte Funeral Home"]
      [
        ( "Bayfriar Gardens"
        , "Something wet and bulbous slinks out of a crackling portal; gain one clue from your neighborhood. The creature makes awful, predatory noises (will). If you pass, you hold your breath until it leaves and find something left behind; gain one curio. If you fail, the teeth are innumerable; suffer two horror."
        , Seq [clue, Test Will 0 curioItem (horror 2)]
        )
      ,
        ( "Duterte Funeral Home"
        , "Madeline mutters, \"Milly Thompson said there was a hole in her fence and she saw hands coming out of it. Must be children playing tricks.\" Gain one clue from your neighborhood. Samuel suddenly rushes in, panting (observation). If you pass, you notice the paper he is clutching in his hand and read the warning on it; remove one doom from any space."
        , Seq [clue, pass Observation 0 (anywhere 1)]
        )
      ,
        ( "Silver Twilight Lodge"
        , "A member of the Lodge furiously studies a series of cryptic, rune-covered maps (influence). If you pass, you join him in silence and after a time he looks up at you, smiles, and shares several of his insights; gain one clue from your neighborhood and one spell. If you fail, you try to ask him what he's doing and he scoffs haughtily at you."
        , pass Influence 0 (Seq [clue, spell])
        )
      ]
  , event
      6
      "French Hill"
      ["Silver Twilight Lodge"]
      [
        ( "Bayfriar Gardens"
        , "You see a slick, many-legged creature scamper up the Bayfriar statue (will). If you pass, you circle the statue, and glimpse the thing as it runs off; gain one clue from your neighborhood and one remnant when you notice one of its legs snap off as it flees. If you fail, it escapes, leaving only thick slime behind."
        , pass Will 0 (Seq [clue, remnants 1])
        )
      ,
        ( "Duterte Funeral Home"
        , "Madeline Duterte quietly dusts the framed portraits that line the hall (observation). If you pass, you warn her just as a glimmering portal cuts a swath across a painting; gain one clue from your neighborhood and remove one doom from any space. If you fail, you realize too late that you are not alone and scramble to get away; become FATIGUED."
        , Test Observation 0 (Seq [clue, anywhere 1]) fatigued
        )
      ,
        ( "Silver Twilight Lodge"
        , "\"It's a breakthrough! Portals to another world!\" a member exclaims. Gain one clue from your neighborhood. She shows you her chaotic notes (lore). If you pass, you can make out some of her awful handwriting; gain one spell. If you fail, she laughs as you struggle to decipher the cryptic text; become CURSED."
        , Seq [clue, Test Lore 0 spell cursed]
        )
      ]
  , event
      7
      "French Hill"
      ["Silver Twilight Lodge"]
      [
        ( "Bayfriar Gardens"
        , "The ground here is sticky with congealed blood (observation). If you pass, you follow the trail of carnage to something wrapped in bloody scraps; gain one clue from your neighborhood and one curio. If you fail, the coppery smell of carnage turns your stomach."
        , pass Observation 0 (Seq [clue, curioItem])
        )
      ,
        ( "Duterte Funeral Home"
        , "A hysterical person rambles to Madeline about the awful thing that stalked and murdered their departed loved one. Gain one clue from your neighborhood. You may spend one remnant to show the stranger that you have avenged their companion. If you do, they thank you profusely; gain one ally."
        , Seq [clue, mayPay (SpendRemnants 1) ally]
        )
      ,
        ( "Silver Twilight Lodge"
        , "Members are told to select a relic to study; gain one curio. Several groups break off to attend lectures (observation). If you pass, the group you join discusses something called the Lurker; gain one clue from your neighborhood. If you fail, you pick a group that is particularly unhinged; suffer one horror."
        , Seq [curioItem, Test Observation 0 clue (horror 1)]
        )
      ]
  , event
      8
      "Miskatonic University"
      ["Science Building"]
      [
        ( "Observatory"
        , "A lone security guard is at the front desk, scribbling notes (observation). If you pass, you notice she looks very nervous and she whispers, \"I was looking through the telescope earlier, and something...something looked back;\" gain one clue from your neighborhood and remove one doom from any space."
        , pass Observation 0 (Seq [clue, anywhere 1])
        )
      ,
        ( "Orne Library"
        , "\"Many misunderstand the hierarchy of ancient beings,\" one of the librarians scoffs (lore). If you pass, you impress her with your understanding of several obscure references to the Outer Gods and she shares her theories and research related to the recent disappearances; gain one clue from your neighborhood and one spell."
        , pass Lore 0 (Seq [clue, spell])
        )
      ,
        ( "Science Building"
        , "A student wrings his hands outside of one of the labs. \"My final! I haven't got anything! Lucille has connected blood lines to bad luck. Luck?\" he exclaims. \"How can I compete with that?\" Gain one clue from your neighborhood. You may spend one remnant to offer the student something for his project to gain $3."
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ]
  , event
      9
      "Miskatonic University"
      ["Orne Library", "Science Building"]
      [
        ( "Observatory"
        , "\"Look at these crazy readings!\" one of the scientists urges (lore). If you pass, you recognize that the stars seem to be wildly out of place and note the pattern; gain one clue from your neighborhood and one remnant. If you fail, you spend the night listening to the scientist rant; become FATIGUED."
        , Test Lore 0 (Seq [clue, remnants 1]) fatigued
        )
      ,
        ( "Orne Library"
        , "You study several texts intently, looking for any scraps of information about creatures and gateways. Gain one spell. You hear something from deep in the stacks (observation). If you pass, you peer into the darkness of the old archives and see something massive and hulking disappear through a portal; gain one clue from your neighborhood."
        , Seq [spell, pass Observation 0 clue]
        )
      ,
        ( "Science Building"
        , "\"Help! Please, oh, no-no-no, help me!\" (observation). If you pass, you arrive just in time to see a thin wisp of a student bash a chair into a massive, taloned gug and you pull them out of the room as quickly as you can; gain one clue from your neighborhood and focus one skill of your choice, even if it exceeds your focus limit."
        , pass Observation 0 (Seq [clue, focusExceed])
        )
      ]
  , event
      10
      "Miskatonic University"
      ["Observatory"]
      [
        ( "Observatory"
        , "\"It's happening randomly, right? Wrong!\" grins one of the technicians. Gain one clue from your neighborhood. You look at their notes (lore). If you pass, you manage to understand the pattern, and the technician smiles and hands you the book they used to figure it out; gain one remnant."
        , Seq [clue, pass Lore 0 (remnants 1)]
        )
      ,
        ( "Orne Library"
        , "You look for information on the giant, four-armed monstrous beings that people have sighted (observation). If you pass, you find a text called Scribes of Kadath; gain one clue from your neighborhood and one spell. If you fail, your search is interrupted by the ripping sound of a portal opening two shelves down and a chilling, guttural growl; suffer one horror."
        , Test Observation 0 (Seq [clue, spell]) (horror 1)
        )
      ,
        ( "Science Building"
        , "You pass by a sign that reads, \"Cash for Research Material!\" You may spend one remnant to gain $3. If you do, the young woman who inspects your offering engages you in pleasant conversation about other things people have dropped off in recent days; gain one clue from your neighborhood."
        , mayPay (SpendRemnants 1) (Seq [money 3, clue])
        )
      ]
  , event
      11
      "Miskatonic University"
      ["Orne Library"]
      [
        ( "Observatory"
        , "A scientist takes a plaster cast of a massive, clawed footprint in the path outside. \"Maybe you can take this to someone who knows more about animals?\" Gain one remnant. You follow the prints (observation). If you pass, you discover residue of a portal opening nearby; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "Orne Library"
        , "The library's sign says, \"Closed due to missing staff.\" Gain one clue from your neighborhood. You find a book someone must have dropped (lore). If you pass, you realize it is a book of the occult; gain one spell. If you fail, the longer you read the more tired you feel; become FATIGUED."
        , Seq [clue, Test Lore 0 spell fatigued]
        )
      ,
        ( "Science Building"
        , "The crowded lecture hall is packed with eager students (observation). If you pass, you notice a young woman slip out and follow her, only to see her frantically clawing at the ground for purchase as she falls through a glimmering slit in the wall, leaving her bag behind; gain one clue from your neighborhood and $3."
        , pass Observation 0 (Seq [clue, money 3])
        )
      ]
  , event
      12
      "Northside"
      ["Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "Gain $3 after Doyle Jefferies reviews your work. He seems tense (observation). If you pass, you see several missing person signs on his desk and recognize them as staff of the Advertiser; gain one clue from your neighborhood. If you fail, he clicks his tongue and tells you he needs more; become FATIGUED."
        , Seq [money 3, Test Observation 0 clue fatigued]
        )
      ,
        ( "Curiositie Shoppe"
        , "\"Don't you worry, we rarely close,\" Oliver Thomas smiles. You may buy any number of curios from the display. If you buy something, Oliver chats with you about an interesting book he read about how criminals choose their targets, which feels oddly applicable; gain one clue from your neighborhood."
        , BuyFromDisplay (Just "Curio") FullPrice Nothing clue
        )
      ,
        ( "Train Station"
        , "Several patrons wait impatiently for the train (influence). If you pass, you strike up a conversation about the recent events plaguing Arkham; gain one clue from your neighborhood and one ally. If you fail, the crowd suddenly starts running as something giant lumbers into view; suffer one damage."
        , Test Influence 0 (Seq [clue, ally]) (damage 1)
        )
      ]
  , event
      13
      "Northside"
      ["Curiositie Shoppe"]
      [
        ( "Arkham Advertiser"
        , "\"Monsters? Entire families just vanishing? This is bad, even for Arkham,\" Minnie laments. Gain one clue from your neighborhood. \"But I can't just run this story, the people need proof!\" You may spend one remnant to gain $3. If you do, Minnie stares at you wide-eyed, \"This\8212this is phenomenal!\""
        , Seq [clue, mayPay (SpendRemnants 1) (money 3)]
        )
      ,
        ( "Curiositie Shoppe"
        , "\"I haven't seen some of my regulars lately. I do hope they are alright,\" Oliver Thomas, the proprietor, muses. Gain one clue from your neighborhood. He shakes his head and turns to you. \"But you are here, shall I show you our new stock?\" You may buy any number of curios from the display."
        , Seq [clue, buyAny "Curio"]
        )
      ,
        ( "Train Station"
        , "You watch as a passenger steps off of the train and falls directly through the ground into a rippling tear in reality (will). If you pass, you shake off your shock and rush over, only to find the person is completely gone and their bag is still laying on the ground; gain one clue from your neighborhood and one common item."
        , pass Will 0 (Seq [clue, commonItem])
        )
      ]
  , event
      14
      "Northside"
      ["Arkham Advertiser", "Train Station"]
      [
        ( "Arkham Advertiser"
        , "\"Hard evidence folks! It's the Advertiser's standard. Don't you ever forget it,\" Doyle Jefferies says as he runs a hand through his graying hair. You may spend one remnant to gain one clue from your neighborhood and $3. If you do, he shakes your hand and says, \"I hope the giants spare you, kid.\""
        , mayPay (SpendRemnants 1) (Seq [clue, money 3])
        )
      ,
        ( "Curiositie Shoppe"
        , "\"I am offering a free charm today to new customers, but for you, I will make an exception,\" Oliver Thomas smiles. Become DRIVEN. You look around the store (observation). If you pass, you notice several new charcoal sketches of a large, bipedal creature with four arms and a wicked, vertical mouth full of teeth; gain one clue from your neighborhood."
        , Seq [driven, pass Observation 0 clue]
        )
      ,
        ( "Train Station"
        , "The train station is deserted, save for a lone passenger. She sits on a bench mumbling something about everyone just disappearing before her eyes. Gain one clue from your neighborhood. You look around for signs of the other passengers (observation). If you pass, all you find is a forgotten bag; gain one common item."
        , Seq [clue, pass Observation 0 commonItem]
        )
      ]
  , event
      15
      "Northside"
      ["Train Station"]
      [
        ( "Arkham Advertiser"
        , "\"No, no, no, no, no! Where are my notes? I had addresses and direct quotes in there!\" Minnie Klein says in a panic (observation). If you pass, you manage to find her notebook and take a peek through before giving it back to a grateful Minnie; gain one clue from your neighborhood and become DRIVEN."
        , pass Observation 0 (Seq [clue, driven])
        )
      ,
        ( "Curiositie Shoppe"
        , "\"Something stalked past the shop last night. Pressed the cobbles down into a wicked impression, too,\" Oliver Thomas muses flatly (observation). If you pass, you find the behemoth footprints; gain one clue from your neighborhood. \"Don't forget, sale today,\" Thomas calls after you. You may buy one curio from the display for half price (rounded up)."
        , Seq [pass Observation 0 clue, buyOneHalf "Curio"]
        )
      ,
        ( "Train Station"
        , "A salesman offers you a sample of his wares if you pass out a few flyers. Gain one common item. You agree and set off (influence). If you pass, you make small talk with passengers who tell you of recent events; gain one clue from your neighborhood. If you fail, being a salesperson is a lot harder than you thought; become FATIGUED."
        , Seq [commonItem, Test Influence 0 clue fatigued]
        )
      ]
  , event
      16
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "A patron rushes up to you and asks how they can help; gain one ally. Several members of the society gather and converse (observation). If you pass, you overhear their discussion about signs of blood rituals; gain one clue from your neighborhood. If you fail, the gruesome discourse is unsettling; suffer one horror."
        , Seq [ally, Test Observation 0 clue (horror 1)]
        )
      ,
        ( "Ma's Boarding House"
        , "\"Saleswoman came through and sold me a whole half-cow and quarter-pig. I've got a stew a-goin', whaddya say?\" Ma asks with an eyebrow cocked. You may spend $1 for you or an ally to recover two health. If you do, Ma speaks as she dishes you up, \"Fewer boarders lately, what with the rumors;\" gain one clue from your neighborhood."
        , mayPay (SpendMoney 1) (Seq [health 2, clue])
        )
      ,
        ( "South Church"
        , "You listen to a Sunday school class; you or an ally may recover two sanity. Several children are not paying attention (observation). If you pass, you overhear a group murmuring about climbing the snowy mountains of Kadath; gain one clue from your neighborhood. If you fail, a child kicks you in a tender spot; suffer one damage."
        , Seq [sanity 2, Test Observation 0 clue (damage 1)]
        )
      ]
  , event
      17
      "Southside"
      ["Historical Society"]
      [
        ( "Historical Society"
        , "A few members of the society huddle around a table and work on a sketch of the being sighted across the city. Gain one clue from your neighborhood. You inquire about it (influence). If you pass, one of them smiles and says tersely, \"Inquiry is well and good, but take care of yourself, scholar;\" gain one curio."
        , Seq [clue, pass Influence 0 curioItem]
        )
      ,
        ( "Ma's Boarding House"
        , "Ma paces by the front window, clutching a missing persons flyer (observation). If you pass, you recognize the face of one of her long-term boarders; gain one clue from your neighborhood. Whether you pass or not, she waves away your concern; \"Oh, I'm sure they're fine. Did you need something to eat, hon?\" You or an ally may recover two health."
        , Seq [pass Observation 0 clue, health 2]
        )
      ,
        ( "South Church"
        , "An elderly man walks down the pews with a donation basket and stops to smile toothlessly at you. You may spend $1 to gain one clue from your neighborhood and for you or an ally to recover two sanity. If you do, the man nods at you and mumbles, \"It's good to see folks coming together with all the tragedy.\""
        , mayPay (SpendMoney 1) (Seq [clue, sanity 2])
        )
      ]
  , event
      18
      "Southside"
      ["Ma's Boarding House"]
      [
        ( "Historical Society"
        , "\"Seeing isn't the same as evidence, man! We need proof, not just hearsay,\" one of the members shouts. You may spend one remnant to offer the evidence they're looking for and gain one clue from your neighborhood. If you do, both members smile to thank you; become DRIVEN."
        , mayPay (SpendRemnants 1) (Seq [clue, driven])
        )
      ,
        ( "Ma's Boarding House"
        , "You sit down to a practical feast of lamb and potatoes. You or an ally may recover two health. Several other boarders gossip about seeing something moving around the back garden, but seem reluctant to tell you about it when you ask. You may spend $1 to gain one clue from your neighborhood."
        , Seq [health 2, mayPay (SpendMoney 1) clue]
        )
      ,
        ( "South Church"
        , "Father Michael delivers a heart-warming sermon about cherishing those we love, even after they are gone (observation). If you pass, you notice several pews are completely empty, as if entire families are missing; gain one clue from your neighborhood and you or an ally may recover two sanity. The sermon fills you with both hope and sadness."
        , pass Observation 0 (Seq [clue, sanity 2])
        )
      ]
  , event
      19
      "Southside"
      ["Ma's Boarding House", "South Church"]
      [
        ( "Historical Society"
        , "You find an old tome and a few artifacts scattered across a desk (observation). If you pass, you examine them, grab a useful object, and learn of a place called Kadath; gain one clue from your neighborhood and one curio. If you fail, the book is painfully confounding; become FATIGUED."
        , Test Observation 0 (Seq [clue, curioItem]) fatigued
        )
      ,
        ( "Ma's Boarding House"
        , "\"I hear they have four arms, and don't wear any clothes! And those teeth, going up the face instead of side-to-side!\" one of the patrons mutters half-hysterically. Gain one clue from your neighborhood. Ma walks in and shushes the panicked whispers and offers some warm buttered bread. You may spend $1 for you or an ally to recover two health."
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "South Church"
        , "You arrive at the church to find several pews destroyed, crushed by something truly massive. Gain one clue from your neighborhood. \"We are taking donations for restoring the sanctuary; could you spare a few dollars for a good cause?\" Father Michael asks. You may spend $1 for you or an ally to recover three sanity."
        , Seq [clue, mayPay (SpendMoney 1) (sanity 3)]
        )
      ]
  , event
      20
      "The Underworld"
      ["City of the Gugs"]
      [
        ( "City of the Gugs"
        , "The gugs seem particularly agitated as of late, clamoring up the Tower of Koth as if compelled. Gain one clue from your neighborhood. Several of the climbing giants drop things as they climb, but grabbing something might be hazardous. You may suffer two damage to gain one curio."
        , Seq [clue, mayPay (CostDamage 2) curioItem]
        )
      ,
        ( "Vale of Pnath"
        , "Skulls and rib cages ripple as massive, slithering bodies burrow under the endless sea of bone and flesh. You may suffer two horror to press on in your search for something\8212anything\8212you can use. If you do, you find evidence of some of the missing people from Arkham among the morbid waves; gain one clue from your neighborhood and $3."
        , mayPay (CostHorror 2) (Seq [clue, money 3])
        )
      ,
        ( "Vaults of Zin"
        , "Several ghasts with twisted, knotted flesh call out with horrible, chortling noises (observation). If you pass, they seem to be calling others to feast upon a rune-marked, bloodless gug; gain one clue from your neighborhood and one remnant. If you fail, you shiver as the creatures move with inhuman speed over a rocky precipice."
        , pass Observation 0 (Seq [clue, remnants 1])
        )
      ]
  , event
      21
      "The Underworld"
      ["City of the Gugs", "Vale of Pnath"]
      [
        ( "City of the Gugs"
        , "A gug tears into a Nightgaunt. Gain one remnant as you pick up a piece of the carnage. You try to discern why the gug is so angry (observation). If you pass, you realize the gug is keeping the winged beast from getting too close to the Tower of Koth; gain one clue from your neighborhood."
        , Seq [remnants 1, pass Observation 0 clue]
        )
      ,
        ( "Vale of Pnath"
        , "A Nightgaunt drops a still-screaming Arkham inhabitant into the pit. Gain one clue from your neighborhood. You search for where they landed (observation). If you pass, you locate the mangled body and try to find some identification; gain one common item. If you fail, the sight is too gruesome and leaves a stain on your soul; become CURSED."
        , Seq [clue, Test Observation 0 commonItem cursed]
        )
      ,
        ( "Vaults of Zin"
        , "The ghasts at the gaping, maw-like entrance of the cave communicate with each other using wet chewing noises and thrown objects (will). If you pass, you take deep breaths, trying to discern what they are and if they can help you; gain one clue from your neighborhood and one remnant. If you fail, it is just too much; suffer one horror."
        , Test Will 0 (Seq [clue, remnants 1]) (horror 1)
        )
      ]
  , event
      22
      "The Underworld"
      ["City of the Gugs", "Vale of Pnath"]
      [
        ( "City of the Gugs"
        , "Nearby, a dying gug wheezes wetly (will). If you pass, you listen and think you hear discernible words, \"Re...turn...to us...tree...no dreams...a...gain;\" gain one clue from your neighborhood and one remnant as you pick up a broken talon. If you fail, the sound mesmerizes you; become FATIGUED."
        , Test Will 0 (Seq [clue, remnants 1]) fatigued
        )
      ,
        ( "Vale of Pnath"
        , "You discover a fresh corpse in the endless valley of flesh and rot. Gain $3. You realize there are a half-dozen fresh bodies nearby. You may suffer one horror to take a close look and gain one clue from your neighborhood. If you do, your face drains of color as you realize the bloodless bodies are all from a local Arkham family."
        , Seq [money 3, mayPay (CostHorror 1) clue]
        )
      ,
        ( "Vaults of Zin"
        , "Several ghasts stand in a circle making sudden, jerking movements and scribbling images of gugs into the ashen ground. Gain one clue from your neighborhood. You watch them (observation). If you pass, you notice one bury something and you go back for it later; gain one remnant. If you fail, the chattering is numbing; become FATIGUED."
        , Seq [clue, Test Observation 0 (remnants 1) fatigued]
        )
      ]
  , event
      23
      "The Underworld"
      ["Vaults of Zin"]
      [
        ( "City of the Gugs"
        , "You find yourself near one of the numerous towering structures of the city where innumerable gugs stand, as if waiting for something. You may suffer one damage to get closer and gain one clue from your neighborhood and find one curio amongst the beasts."
        , mayPay (CostDamage 1) (Seq [clue, curioItem])
        )
      ,
        ( "Vale of Pnath"
        , "You walk along the rocky border of the bone-filled valley (observation). If you pass, you find the corpses of several gugs covered in strange, ritualistic markings and surrounded by odd artifacts; gain one clue from your neighborhood and one common item. If you fail, the stench of pungent, coppery rot rushes over you suddenly; suffer one horror."
        , Test Observation 0 (Seq [clue, commonItem]) (horror 1)
        )
      ,
        ( "Vaults of Zin"
        , "You kneel and uncover a rune-covered stone; gain one spell. As you stand, you come face-to-face with a twitching ghast (will). If you pass, you silently stare back at it and it gives you a crumpled drawing of a gug before leaving; gain one clue from your neighborhood. If you fail, you gasp and the creature lunges; suffer one damage."
        , Seq [spell, Test Will 0 clue (damage 1)]
        )
      ]
  , event
      24
      "The Underworld"
      ["City of the Gugs"]
      [
        ( "City of the Gugs"
        , "A gug steps through a flickering portal set in the side of a building, but the closing gateway shears off the tip of one of its talons. Gain one remnant. You may decide to inspect near where you saw the portal, despite the residual heat. If you do, you suffer one damage and gain one clue from your neighborhood."
        , Seq [remnants 1, mayPay (CostDamage 1) clue]
        )
      ,
        ( "Vale of Pnath"
        , "You stub your toe on something amidst the mounds of bodies. Gain one common item. The object seems out of place, so you look around (observation). If you pass, you see one of Oliver Thomas' shop assistants mangled and bloodless on the ground, clutching a bag of deliveries; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "Vaults of Zin"
        , "You search for a way out of the maze-like caverns of the Vaults (observation). If you pass, you come across a bizarre ritual room lit by reddish-brown light and covered in runes and pictographs of gugs worshiping a much larger gug; gain one clue from your neighborhood and one spell. If you fail, you stumble blindly in the dark, endlessly."
        , pass Observation 0 (Seq [clue, spell])
        )
      ]
  , event
      25
      "Uptown"
      ["Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "People gather and begin to chant (will). If you pass, you hold your breath as a massive, multi-armed gug appears and grabs one of them, who drops something; gain one clue from your neighborhood and one common item. If you fail, you run until you can barely breathe; become FATIGUED."
        , Test Will 0 (Seq [clue, commonItem]) fatigued
        )
      ,
        ( "St. Mary's Hospital"
        , "Nurse Sharon says she should have your medicine ready shortly (observation). If you pass, you catch sight of a man with large gashes in his side just as she returns; gain one clue from your neighborhood and you or an ally may recover two health. If you fail, you wait for nearly twenty minutes before asking after the nurse, only to learn she has left for the evening."
        , pass Observation 0 (Seq [clue, health 2])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam Beecher chats with you pleasantly about the recent disappearances being a boon for business; gain one clue from your neighborhood. Though her chatter seems a bit morbid, you did come here for a reason; reveal the top three spells in the deck. You may buy any number of them. Place the rest on the bottom of the deck."
        , Seq [clue, spells 3 Nothing FullPrice]
        )
      ]
  , event
      26
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "A meaty, taloned fist grabs you from the dark shadows (strength). If you pass, you wrench your body away along with one of the talons; gain one clue from your neighborhood and one remnant. If you fail, you feel a sharp pain like a hot knife carving into your flesh; suffer one damage."
        , Test Strength 0 (Seq [clue, remnants 1]) (damage 1)
        )
      ,
        ( "St. Mary's Hospital"
        , "\"Strange cuts and wounds from people, lately. I suspect gang violence,\" Dr. Mortimore muses. Gain one clue from your neighborhood. \"That aside, have you an ailment that needs attending?\" You may spend $1 for you or an ally to recover two health. If you don't, the doctor cocks an eyebrow and says sternly, \"Then don't waste my time.\""
        , Seq [clue, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "Miriam shows you a collection of new tomes. Reveal the top three spells of the deck. You may buy one of them for half price (rounded up). Place the rest on the bottom of the deck. If you buy something, Miriam throws in an esoteric book on resurrection-focused cults; gain one clue from your neighborhood."
        , shoppeOneHalf
        )
      ]
  , event
      27
      "Uptown"
      ["Hangman's Hill", "Hangman's Hill"]
      [
        ( "Hangman's Hill"
        , "Something is wedged into the bark of the hangman's tree. Gain one common item. You look around for an explanation (observation). If you pass, you find a trail of blood leading to the tree and see fingernail scratches, as if someone was holding onto the tree for dear life; gain one clue from your neighborhood."
        , Seq [commonItem, pass Observation 0 clue]
        )
      ,
        ( "St. Mary's Hospital"
        , "\"There we are, all patched up,\" Doctor Maheswaran proclaims. You or an ally may recover two health. You scan the lobby (observation). If you pass, you notice there are not very many people here, and a nurse somberly states, \"Most are missing, not hurt;\" gain one clue from your neighborhood. If you fail, the quiet of the hospital is unnerving; suffer one horror."
        , Seq [health 2, Test Observation 0 clue (horror 1)]
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "\"I'm looking for a particular book, would you mind keeping an eye out?\" Miriam asks (observation). If you pass, you page through the Book of the Far Mountains; gain one clue from your neighborhood and one spell. If you fail, your fingers graze a fleshy-looking book that pulsates against your touch; suffer one horror."
        , Test Observation 0 (Seq [clue, spell]) (horror 1)
        )
      ]
  , event
      28
      "Uptown"
      ["St. Mary's Hospital"]
      [
        ( "Hangman's Hill"
        , "\"They will rise!\" a cultist cries (observation). If you pass, you recognize the runes on their cloaks as marks from the Dreamlands and manage to grab some evidence of their meeting; gain one clue from your neighborhood and one common item. If you fail, the sermon is nonsensical and unsettling."
        , pass Observation 0 (Seq [clue, commonItem])
        )
      ,
        ( "St. Mary's Hospital"
        , "\"The doctors are quite busy today, but I can administer some basic aid for little scrapes like that, dear,\" the nurse says. You may spend $2 to gain one clue from your neighborhood and for you or an ally to recover three health. If you do, the nurse somberly chats about the strange sightings and disappearances in recent weeks."
        , mayPay (SpendMoney 2) (Seq [clue, health 3])
        )
      ,
        ( "Ye Olde Magick Shoppe"
        , "\"I have something for you here,\" Miriam gestures to a book. Gain one spell. You shop around (observation). If you pass, you notice several books about dreams have been purchased recently; gain one clue from your neighborhood. If you fail, you lean on a bookshelf too heavily and it topples over on you as Miriam shrieks; suffer two damage."
        , Seq [spell, Test Observation 0 clue (damage 2)]
        )
      ]
  ]
