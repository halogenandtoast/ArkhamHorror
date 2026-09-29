{- | Dead of Night's second encounter card for every neighborhood and street type.

These reprint the numbers 1 to 8 and shuffle in with the base set's, so their
codes carry a @-don@ before the number and their art sits in its own folder.
-}
module AH3e.Content.DeadOfNight.Encounters (cards) where

import AH3e.Content.Tiles
import AH3e.Content.Vocabulary
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

cards :: [CardDef]
cards =
  downtown
    <> easttown
    <> merchantDistrict
    <> miskatonicUniversity
    <> northside
    <> rivertown
    <> southside
    <> uptown
    <> streets

pad :: Int -> Text
pad = T.justifyRight 2 '0' . tshow

card :: NeighborhoodId -> Int -> [(Text, Text, Effect)] -> CardDef
card nid n encounters =
  CardDef
    { code = CardCode (coerce nid <> "-don-" <> pad n)
    , name = (tile nid).name <> " " <> tshow n <> "/8 (Dead of Night)"
    , expansion = DeadOfNight
    , copies = 1
    , kind =
        NeighborhoodCard
          nid
          (Map.fromList [(spaceIdFor place, Encounter txt eff) | (place, txt, eff) <- encounters])
    }

street :: Int -> [(StreetType, Text, Effect)] -> CardDef
street n encounters =
  CardDef
    { code = CardCode ("street-don-" <> pad n)
    , name = "Street " <> tshow n <> "/8 (Dead of Night)"
    , expansion = DeadOfNight
    , copies = 1
    , kind = StreetCard (Map.fromList [(st, Encounter txt eff) | (st, txt, eff) <- encounters])
    }

-- shorthand this set needs beyond Vocabulary's
wanted :: Effect
wanted = GainE (Condition "WANTED")

darkPact :: Effect
darkPact = GainE (Condition "DARK PACT")

allSanity :: Int -> Effect
allSanity n = RecoverSanity YouAndYourAllies (N n)

both :: Int -> Int -> Effect
both h s = RecoverBoth YouOrAlly (N h) (N s)

buyAnyThen :: Trait -> Effect -> Effect
buyAnyThen t = BuyFromDisplay (Just t) FullPrice Nothing

downtown :: [CardDef]
downtown =
  map
    (uncurry (card "downtown"))
    [
      ( 1
      ,
        [
          ( "Arkham Asylum"
          , "The engravings over the windows in this wing of the sanitarium are in an obscure language (lore). If you pass, you piece the words together into some kind of prayer that briefly allows you to see all the music of the universe; become BLESSED. If you fail, you know you've missed something important."
          , pass Lore 0 blessed
          )
        ,
          ( "Independence Square"
          , "A glum-looking man has laid out a blanket, on which rest many household items and oddities. \"My uncle left me these,\" he explains. \"But I've not a notion what to do with 'em, except perhaps pay my tuition next semester.\" You may buy any number of curios from the display."
          , buyAny "Curio"
          )
        ,
          ( "La Bella Luna"
          , "You take a seat at a poker table in the Clover Club, hidden beneath La Bella Luna. The stern-faced man raises the stakes in your game quite sharply, and stares straight into your eyes when he challenges you to call the bet (observation). If you pass, you recognize his tell when he adjusts his cuff-link; gain $3. If you fail, you fold and leave the table."
          , pass Observation 0 (money 3)
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Arkham Asylum"
          , "The doctor's voice is soothing and calm as she brings you out of your tranquil hypnotic state; you or an ally recovers two sanity. You ask her about her methods (observation). If you pass, she gives you a small mirrored pendulum and teaches you some basic techniques; gain the HYPNOTIST'S MIRROR."
          , Seq [sanity 2, pass Observation 0 (named "HYPNOTIST'S MIRROR")]
          )
        ,
          ( "Independence Square"
          , "A woman from Innsmouth has set up a table full of trinkets at the flea market. Most of the items carry an odd odor that, despite its foulness, makes you yearn for the ocean. At least a few things she's brought for sale have some use. You may buy one common item from the display for half price (rounded up)."
          , buyOneHalf "Common"
          )
        ,
          ( "La Bella Luna"
          , "On a mad impulse, you bet it all on black. Gain $3. Peter Clover pulls you into a back room for a \"friendly chat\" and you try to explain that you're not cheating (observation). If you pass, you smooth everything over. If you fail, his associates have a less-friendly chat with you; suffer two damage."
          , Seq [money 3, Test Observation 0 NoEffect (damage 2)]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Arkham Asylum"
          , "Just talking about your problems is already making you feel better. You or an ally recovers two sanity. Charles Badoe suggests that you might benefit from a course of group therapy, sharing your problems with others. You may spend $2 for you and each of your allies to recover two sanity."
          , Seq [sanity 2, mayPay (SpendMoney 2) (allSanity 2)]
          )
        ,
          ( "Independence Square"
          , "You can't see anyone, but the hairs on your arm stand on end and you know that you're the focus of attention (will). If you pass, you force yourself to keep calm and continue towards Founder's Rock, where you find an object lying in front of you as though it's an offering; gain one common item."
          , pass Will 0 commonItem
          )
        ,
          ( "La Bella Luna"
          , "You look at the other cards showing on the blackjack table (observation). If you pass, you realize the odds are heavily in your favor; you double down for one more card and gain $4. If you fail, you play it safe and settle out more-or-less even. Either way, you see the pit boss watching your table and decide to move on."
          , pass Observation 0 (money 4)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Arkham Asylum"
          , "You sit down for a session with Dr. Badoe and work through some of the issues behind your anxiety. You or an ally recovers two sanity. The doctor offers to write you a script for something to help even you out. You may spend $1 for you or an ally to recover two sanity."
          , Seq [sanity 2, mayPay (SpendMoney 1) (sanity 2)]
          )
        ,
          ( "Independence Square"
          , "You play a relaxing game of bocce on the green with a few of the old timers. You or an ally recovers two sanity. After the game, Mr. Hatle invites you to join a tournament (observation). If you pass, become the BOCCE CHAMPION. If you fail, you try your best, but another player knocks the pallino away from your ball on the last toss."
          , Seq [sanity 2, pass Observation 0 (named "BOCCE CHAMPION")]
          )
        ,
          ( "La Bella Luna"
          , "\"See that gentleman with the walrus-whiskers? He's what we call a whale. Terrible at poker but with money to burn.\" You try to get an invite to the whale's private game (influence). If you pass, you've never won easier money in your life; gain $3. If you fail, you break even at more-skilled tables."
          , pass Influence 0 (money 3)
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Arkham Asylum"
          , "You overhear Nurse Heather complaining that her mother can't afford her rent. You may spend $2 to contribute to the worthy cause. If you do, Nurse Heather quietly bumps you to the head of the line for treatment with Dr. Badoe; you or an ally recovers three sanity."
          , mayPay (SpendMoney 2) (sanity 3)
          )
        ,
          ( "Independence Square"
          , "You're sure if you spent some time picking through the stalls and tents at the flea market you'd find just what you're looking for. You may buy one curio from the display for half price (rounded up). It's gotten quite late by the time you wrap up your shopping, and you feel a sense of dread at the thought of being alone in the square."
          , buyOneHalf "Curio"
          )
        ,
          ( "La Bella Luna"
          , "You set down your napkin, comfortably full and happy, and reach for your bill, only to find that it's been taken care of (observation). If you pass, you see your benefactor slip out the front door, but they're gone before you can thank them; become BLESSED by the random kindness. If you fail, you look around in confusion and leave a nice tip."
          , pass Observation 0 blessed
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Arkham Asylum"
          , "The setting sun shines pleasantly on the asylum garden. You wait for a brief time, watching a few patients walk the grounds, before you are joined by your doctor. You take great comfort in your session before heading back into the night. You may spend $1 for you or an ally to recover two sanity."
          , mayPay (SpendMoney 1) (sanity 2)
          )
        ,
          ( "Independence Square"
          , "A man and a woman in worsted wool suits as thick as their Irish accents recruit you for \"a right simple job\" at a house overlooking the square (will). If you pass, you stand lookout and gain your share of the spoils; gain one common item. If you fail, you leg it and the mobsters promise revenge; become WANTED."
          , Test Will 0 commonItem wanted
          )
        ,
          ( "La Bella Luna"
          , "When you notice the man next to you at the bar is packing heat and shooting daggers at the office door, you discreetly inform one of O'Bannion's associates (influence). If you pass, they roust the man out of the club and name you an ally of the gang; gain ARMED BACKUP. If you fail, the gang enforcer chases you out."
          , pass Influence 0 (named "ARMED BACKUP")
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Arkham Asylum"
          , "\"Doctor Mintz has been working for sixteen hours and finally went to sleep. I'm not waking him up for you, no way, no how,\" says Nurse Heather. You may spend $2 to change her mind. If you do, Mintz is bouncing with energy. He offers you some of his \"pep pills\"; you or an ally recovers three sanity."
          , mayPay (SpendMoney 2) (sanity 3)
          )
        ,
          ( "Independence Square"
          , "When Anna Kaslow draws the last card in your tarot reading, her face falls. The meaning is clear: you are in incredible danger (will). If you pass, the grim portent steels your resolve for the trials ahead; you may focus two skills of your choice, even if it exceeds your focus limit. If you fail, you feel a quail of hopelessness shudder through your body."
          , pass Will 0 (Seq [focusExceed, focusExceed])
          )
        ,
          ( "La Bella Luna"
          , "The woman at your side won't give her name but promises that she's your lucky charm. Her eyes are the color of money and you find yourself oddly convinced she's right; discard $2. If you do, you hit the tables and start winning; roll one die and gain money equal to your roll."
          , mayPay (SpendMoney 2) (Custom "don-lucky-charm")
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Arkham Asylum"
          , "While you wait for your appointment with Dr. Mintz, Dr. Zulock hints that his colleague's methods can be rather unpleasant. You may spend $2 for a session with Zulock instead. If you do, the hypnotherapy is remarkable; focus up to two skills of your choice, even if it exceeds your focus limit."
          , mayPay (SpendMoney 2) (Seq [focusExceed, focusExceed])
          )
        ,
          ( "Independence Square"
          , "As you approach each streetlight, it flickers and dies, leaving you to race through the dark to the next one (will). If you pass, you press on until you're standing below the only light in sight, where something catches your eye; gain one curio. If you fail, you retreat through the closest doorway and wait out the dark."
          , pass Will 0 curioItem
          )
        ,
          ( "La Bella Luna"
          , "You find a handful of casino chips on the floor; gain $2. As you straighten up, you realize that a stack of the tokens is unattended on a table nearby (influence). If you pass, you palm a few more chips and walk away; gain $2. If you fail, one of Clover's bouncers sees your interest in the chips and chases you off; become WANTED."
          , Seq [money 2, Test Influence 0 (money 2) wanted]
          )
        ]
      )
    ]

easttown :: [CardDef]
easttown =
  map
    (uncurry (card "easttown"))
    [
      ( 1
      ,
        [
          ( "Hibb's Roadhouse"
          , "It's a quiet night at the Roadhouse, and Old Man Hibbard is happy for any customers. You may spend $2 for a bottle of whiskey and a plate of corned beef with cabbage. If you do, you enjoy a fine meal and a long comfortable evening; you are invigorated and become BLESSED."
          , mayPay (SpendMoney 2) blessed
          )
        ,
          ( "Police Station"
          , "\"Dingby! Dingby, you left the evidence lockup hanging wide open again!\" Sergeant Parker doesn't seem to have noticed you as he rushes to chastise the hapless deputy (observation). If you pass, you walk into the still-unlocked evidence room and have a careful look for anything that could be helpful; gain one common item."
          , pass Observation 0 commonItem
          )
        ,
          ( "Velma's Diner"
          , "The booth at Velma's is comfortable, and it feels good to get off your feet for a while. You or an ally may recover one health and one sanity. While you sit and listen to Ryan Dean try to impress the decidedly uninterested waitress, she offers to warm up your coffee. You may spend $1 for you or an ally to recover two health."
          , Seq [both 1 1, mayPay (SpendMoney 1) (health 2)]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Hibb's Roadhouse"
          , "A pair of strangers with out-of-town accents interrupt your drink with a few questions (observation). If you pass, you play it cool with the plain-clothes feds, impressing the Sheldon gang smugglers that supply this gin joint; gain HIRED MUSCLE. If you fail, they pin it on you; become WANTED."
          , Test Observation 0 (named "HIRED MUSCLE") wanted
          )
        ,
          ( "Police Station"
          , "You chat briefly with Deputy Galeas about the struggles you're facing. \"Lord above, I thought it was just me!\" He's happy to learn that others have witnessed the unusual activity his superiors seem keen on ignoring and offers to let you look through the items that have been abandoned at the station. Gain one common item."
          , commonItem
          )
        ,
          ( "Velma's Diner"
          , "Joey \"the Rat\" Vigil slides into the booth across from you and places a bundle wrapped in newsprint on the table. \"Hey, let's help each other out.\" You may buy one curio from the display for half price (rounded up). Velma rolls her eyes at Joey as she comes to take your order. You may spend $1 for you or an ally to recover two health."
          , Seq [buyOneHalf "Curio", mayPay (SpendMoney 1) (health 2)]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Hibb's Roadhouse"
          , "As the darkness whispers in the trees outside, there's something soothing about being in a place full of light and life. You or an ally recovers one sanity. Old Man Hibbard puts a bottle of Kentucky bourbon on the bar and shakes it at you. You may spend $2 for you or an ally to recover two sanity."
          , Seq [sanity 1, mayPay (SpendMoney 2) (sanity 2)]
          )
        ,
          ( "Police Station"
          , "The police sergeant's case has fallen apart, and she's at wit's end. You may give her one remnant to reinforce her key piece of evidence and help her get the collar. If you do, she successfully builds a case against a banker who allowed a group of cultists to take his daughter; become BLESSED."
          , mayPay (SpendRemnants 1) blessed
          )
        ,
          ( "Velma's Diner"
          , "The diner is packed full of happy patrons, and Velma bustles about making sure that everyone is taken care of. Despite the rush, the food is still top notch, and watching her create order out of chaos is inspiring. Focus one skill of your choice. You may spend $1 for you or an ally to recover three health."
          , Seq [focusAny, mayPay (SpendMoney 1) (health 3)]
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Hibb's Roadhouse"
          , "Old Man Hibbard is holding court by the fire, trading increasingly painful puns with a crowd of people about his age. You or an ally recovers two sanity. The bartender sighs as you approach. \"A little liquor makes the puns not so bad.\" You may spend $2 for you or an ally to recover two sanity."
          , Seq [sanity 2, mayPay (SpendMoney 2) (sanity 2)]
          )
        ,
          ( "Police Station"
          , "Sheriff Engle is puzzling over a wall full of photo evidence, and allows you to take a look (observation). If you pass, you notice a tall slender figure in the back of each picture; you've earned the sheriff's trust and GOOD STANDING in the community. If you fail, you waste the sheriff's time with an unsupported theory."
          , pass Observation 0 (named "GOOD STANDING")
          )
        ,
          ( "Velma's Diner"
          , "Velma's Diner is so crowded, loud, and busy that it takes you three tries to get Velma's attention. \"I'm sorry, dear, I thought I'd already served you.\" She pulls out her pad and prepares to take your order. You may spend $1 for you or an ally to recover two health."
          , mayPay (SpendMoney 1) (health 2)
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Hibb's Roadhouse"
          , "There's a broad woman with a driving cap pulled low over her eyes blocking the door. \"There's a cover charge tonight,\" she says. You may pay $1 to enter. If you do, you find that the tables and chairs have all been pushed back and the whole joint is jumping with jazz; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ,
          ( "Police Station"
          , "\"Dainty\" Donohue comes whistling down the steps and presses something into your hands. \"Hold on to this for me.\" Gain one common item. As Donohue leaves, Sheriff Engle asks you about your relationship with him (influence). If you fail, your answers were satisfying to neither Engle nor Donohue; become WANTED."
          , Seq [commonItem, Test Influence 0 NoEffect wanted]
          )
        ,
          ( "Velma's Diner"
          , "One of Velma's servers is handing out small baked goods from a pushcart on the street. You or an ally recovers two health. You may spend $1 to tip the woman running the snack cart. If you do, she gives you an extra muffin and a grateful smile; become BLESSED."
          , Seq [health 2, mayPay (SpendMoney 1) blessed]
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Hibb's Roadhouse"
          , "You have no idea how the brawl started and no idea why you got involved in it, but when it's over you and your new best friend for life are lying facedown in the dirt of the driveway outside. Take two damage and gain one ally."
          , Seq [damage 2, ally]
          )
        ,
          ( "Police Station"
          , "\"Detective Harden is dragging his heels on catching the burglars who robbed me,\" complains a woman in a pearl necklace. \"You've got time to work the case, don't you?\" You may become delayed to review the evidence. If you do, you determine that the butler did it and she offers you a reward; gain one common item with value four or greater."
          , mayPay CostDelayed (GainE (AnItemValued (Just "Common") (AtLeast 4)))
          )
        ,
          ( "Velma's Diner"
          , "Velma is just slicing into a fresh pie as you sit at the counter. Beaming with pride, she offers you a slice. You or an ally recovers two health. \"Have you ever had a better piece of pie?\" You attempt to assure her that it's better than Ma Mathison's (influence). If you pass, she packs one up for you to take with you; gain VELMA'S CHERRY PIE."
          , Seq [health 2, pass Influence 0 (named "VELMA'S CHERRY PIE")]
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Hibb's Roadhouse"
          , "You or an ally recovers three sanity. As you finish your meal, a man joins you without saying a word (observation). If you pass, you notice a pair of sharp-featured men eying him and quickly leave the stranger sitting alone. If you fail, you linger too long and they think you're together; become WANTED."
          , Seq [sanity 3, Test Observation 0 NoEffect wanted]
          )
        ,
          ( "Police Station"
          , "The desk sergeant rolls his eyes as you start to describe the things you've seen. You do your best to convince him that this isn't just some prank (influence). If you pass, he sends a patrolman to check it out; remove one doom from any space. If you fail, he chases you out the door, threatening to book you for wasting his time."
          , pass Influence 0 (RemoveDoomFrom AnySpace (N 1))
          )
        ,
          ( "Velma's Diner"
          , "The spicy-sweet smell of Velma's pie wafts over you as soon as the door opens. Your stomach rumbles with each step as you make your way to your seat. You can already tell you're going to be ordering more food than you expected. You may spend $2 for you or an ally to recover four health."
          , mayPay (SpendMoney 2) (health 4)
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Hibb's Roadhouse"
          , "It's a quiet night at Hibb's. A few groups of people cluster in corners of the creaky old converted barn, swapping stories over a drink or seven. Others drink alone. The bartender pours you a dram of pure, clear firewater. You may spend $1 for you or an ally to recover two sanity."
          , mayPay (SpendMoney 1) (sanity 2)
          )
        ,
          ( "Police Station"
          , "Your tip is so accurate that Deputy Dingby is convinced that you must have been an accomplice to the crime you just helped him solve (influence). If you pass, you convince him that you actually had a psychic vision and the amazed deputy gives you a bonus; gain one common item. If you fail, he holds you for questioning; become delayed."
          , Test Influence 0 commonItem delayed
          )
        ,
          ( "Velma's Diner"
          , "Traveling salesman Ryan Dean is hawking his latest wares right in front of Velma's door (observation). If you pass, you notice the wares are defective just before someone makes a major purchase; gain one ally. If you fail, you shake your head at Dean's persistence and go about your business."
          , pass Observation 0 ally
          )
        ]
      )
    ]

merchantDistrict :: [CardDef]
merchantDistrict =
  map
    (uncurry (card "merchant-district"))
    [
      ( 1
      ,
        [
          ( "River Docks"
          , "From the edge of the pier, you stare at the stars reflected in the water of the Miskatonic River (will). If you pass, you feel a great sense of peace at being a part of the massive and eternal universe; become BLESSED. If you fail, you shiver, realizing how small and insignificant you are."
          , pass Will 0 blessed
          )
        ,
          ( "Tick-Tock Club"
          , "A large number of well-dressed men and women are celebrating something they are carefully vague about. Liquor and food both flow freely and you are invited to partake. You or an ally recovers two health and two sanity. You try to limit yourself to only a few drinks (will). If you fail, become delayed."
          , Seq [both 2 2, Test Will 0 NoEffect delayed]
          )
        ,
          ( "Unvisited Isle"
          , "The remains of some kind of ritual have been left behind among the standing stones. Gain one remnant. As you turn to leave, you see the runes in the ground are glowing faintly (lore). If you pass, you safely disrupt the summoning runes. If you fail, your mistake calls out to something in the darkness; spawn one monster in your space."
          , Seq [remnants 1, Test Lore 0 NoEffect (SpawnMonsterIn YourSpace False)]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "River Docks"
          , "Joey \"the Rat\" has got an assortment of merchandise available. You may buy one common item from the display. He says he's got buyers lined up for \"unusual\" goods, and that his trusted partners get greater access to his reserve stock. You may spend one remnant to gain JOEY VIGIL'S SUPPLY."
          , Seq [buyOne "Common", mayPay (SpendRemnants 1) (named "JOEY VIGIL'S SUPPLY")]
          )
        ,
          ( "Tick-Tock Club"
          , "The comfortable interior of the Tick-Tock Club provides a much needed respite from the stress that awaits you back out on the streets. You or an ally recovers two sanity. The waitress brings you a menu after your drink, and invites you to stay longer to enjoy a full meal. You may spend $1 for you or an ally to recover two health."
          , Seq [sanity 2, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "Unvisited Isle"
          , "Clouds hide the moon as a wind blows off the river, blotting out all light (will). If you pass, you navigate the hoary pine and find something snagged against the roots; gain one curio. If you fail, the sudden and utter darkness sends you screaming through the trees; suffer two horror."
          , Test Will 0 curioItem (horror 2)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "River Docks"
          , "The boat captain with the Russian accent refuses to tell you what his cargo is, but he's offering good pay to help unload it. \"The longshoremen won't touch it,\" he complains. You may become delayed to gain $5. As you work, you marvel at the snow and frost that cling to his deck even in the warmth."
          , mayPay CostDelayed (money 5)
          )
        ,
          ( "Tick-Tock Club"
          , "The band fills the club's lounge with great music and a cheerful atmosphere. You or an ally recovers two sanity. When the band takes a break, you may spend $2 to send them a round of drinks. If you do, they play your favorite song to start their next set; become BLESSED."
          , Seq [sanity 2, mayPay (SpendMoney 2) blessed]
          )
        ,
          ( "Unvisited Isle"
          , "Some of Sadie Sheldon's boys are using local superstitions about the island to keep unwanted eyes off of their smuggled goods. You may take one curio from their hidden cargo and head back the way you came, across the open ground. If you do, they spot you as you hustle back to your rowboat; become WANTED."
          , May "Take one curio from the hidden cargo" (Seq [curioItem, wanted])
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "River Docks"
          , "The sailor resting on the dock shares stories of his many adventures, explaining how he got each scar. \"And I have a little something for anyone who has a story to beat mine,\" he laughs. You may spend one remnant to gain one common item."
          , mayPay (SpendRemnants 1) commonItem
          )
        ,
          ( "Tick-Tock Club"
          , "You're alone at the bar when \"Dainty\" Donohue sidles up behind you and attempts to work you over (strength). If you pass, you easily overpower the small man and take his flashy custom pistols instead; gain DONOHUE'S NEW .45s. If you fail, he scurries away and flees before he can lose another pair of guns; become WANTED."
          , Test Strength 0 (named "DONOHUE'S NEW .45S") wanted
          )
        ,
          ( "Unvisited Isle"
          , "Surely the detritus scattered among the standing stones isn't here by mere chance. You step into the ring of carved menhirs and study the fragments (lore). If you pass, you recognize a handful of ritual candles and other leftover reagents; gain one remnant. If you fail, you don't recognize any pattern to the chaotic swirl of debris."
          , pass Lore 0 (remnants 1)
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "River Docks"
          , "The tall stranger asks you to help him acquire some unusual materials, but refuses to answer any of your questions. You may spend one remnant to get him what he wants. If you do, you try to ignore the leathery scales on his hand when he compensates you well for your efforts; gain $4."
          , mayPay (SpendRemnants 1) (money 4)
          )
        ,
          ( "Tick-Tock Club"
          , "The bartender opens a bottle of fine brandy for the man with the tarnished trumpet, but it is wordlessly rejected. The barman, clearly unnerved, goes to return the bottle to the top shelf, but notices that it has your attention. Since it's open, he offers you a glass to go with your meal. You may spend $2 for you or an ally to recover two health and two sanity."
          , mayPay (SpendMoney 2) (both 2 2)
          )
        ,
          ( "Unvisited Isle"
          , "You're not certain how you got here, or why you're naked, but the three women surrounding you feel almost familiar. The young one paints your skin with something warm, the old one chants, and the one that reminds you of your mother offers you a drink from a clay ewer. You suffer two horror and become BLESSED."
          , Seq [horror 2, blessed]
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "River Docks"
          , "Joey \"the Rat\" asks you for a bit of assistance unloading his truck and pays you well for your discretion. Gain $2. He offers you another deal if you want it; you may spend one remnant for another $2. If you do not, you run into a police patrol on your way out; become WANTED."
          , Seq [money 2, MayPay (SpendRemnants 1) (money 2) wanted]
          )
        ,
          ( "Tick-Tock Club"
          , "No one else notices the narrow figure gliding through the room, adjusting the clocks. \"Terrible when things are out of alignment,\" they say in a multitonal voice, nudging spectacles like gemstones up a long nose. \"I can set you right, too, for a price.\" You may spend $2 to focus two skills of your choice, even if it exceeds your focus limit."
          , mayPay (SpendMoney 2) (Seq [focusExceed, focusExceed])
          )
        ,
          ( "Unvisited Isle"
          , "\"Ah, a little help?\" The handsome stranger floats between a pair of standing stones, bound in some kind of mystic trap. You attempt to free him (lore). If you pass, he introduces himself and offers to help you for a while; LEO DE LUCA joins you. If you fail, he yelps in pain and tells you to leave off; he'll get himself out of this fix."
          , pass Lore 0 (named "LEO DE LUCA")
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "River Docks"
          , "A sailor with a fuzzy wool cap accosts you. \"I'm looking to sell,\" he explains as he lets you peek into his bag. \"But I'm not interested in American money. More of a swap, you might say. What do you think?\" You may spend one remnant to gain one common item."
          , mayPay (SpendRemnants 1) commonItem
          )
        ,
          ( "Tick-Tock Club"
          , "The peaceful atmosphere inside the club offers a welcome respite from the perils of the increasingly dangerous streets. You order a cocktail and something to eat and watch the other patrons dance. You may spend $1 for you or an ally to recover two health and two sanity."
          , mayPay (SpendMoney 1) (both 2 2)
          )
        ,
          ( "Unvisited Isle"
          , "A sudden bank of fog rolls over you as you head back toward your rowboat. You do your best to keep your head, even as you hear movement from something unseen in the mist (will). If you pass, you come across the remains of some kind of creature when the fog clears as suddenly as it came; gain one remnant."
          , pass Will 0 (remnants 1)
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "River Docks"
          , "The man on the dock claims to be a visiting German biologist, but you've never seen a biologist pay good hard cash for \"samples.\" Still, he's willing to pay for anything unusual you might have. You may spend one remnant to gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ,
          ( "Tick-Tock Club"
          , "You give the doorman the password and duck in out of the rain. The walls of the club are covered in dozens, if not hundreds of clocks, but they only make it easier to lose track of time in the comfortable lounge. You may spend $2 for you or an ally to recover two health and two sanity."
          , mayPay (SpendMoney 2) (both 2 2)
          )
        ,
          ( "Unvisited Isle"
          , "The tall stranger offers you his assistance if you'll sign the contract (lore). If you pass, you note a few problematic clauses before proposing an amended agreement and the satisfied stranger offers you a gift; gain one curio. If you fail, you miss the many strings attached to the stranger's offer; gain a DARK PACT with nothing to show for it."
          , Test Lore 0 curioItem darkPact
          )
        ]
      )
    ]

miskatonicUniversity :: [CardDef]
miskatonicUniversity =
  map
    (uncurry (card "miskatonic-university"))
    [
      ( 1
      ,
        [
          ( "Observatory"
          , "In a rare moment of quiet atop the observatory, you breathe in the cool night air and meditate upon the wonder of the heavens above you (observation). If you pass, you marvel as a pure white light streaks across the sky; become BLESSED. If you fail, you cut your respite short, determined to succeed."
          , pass Observation 0 blessed
          )
        ,
          ( "Orne Library"
          , "Alone in the Ruggles Rare Books Room, you push back from the leather-bound tome, convinced that the creatures embellished in the margins are moving when you aren't looking at them (will). If you pass, you steel your mind and allow the book to impart its power upon you; gain one spell. If you fail, the beasts advance menacingly; suffer one horror."
          , Test Will 0 spell (horror 1)
          )
        ,
          ( "Science Building"
          , "While carrying one of your more interesting findings from your adventures in hand, you stumble into an argument about the validity of alchemy as a scientific discipline. \"What do you think, hmm?\" asks one professor. \"Never-mind that,\" says the other. \"What have you there?\" You may spend one remnant to gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Observatory"
          , "A collection of books on astronomy and astrology are piled on one of the research tables (lore). If you pass, you peruse the researcher's notes and identify a significant date that appears in multiple calculations; tucked in the page for the appropriate star chart is a well-worn tarot card; gain THE STAR."
          , pass Lore 0 (named "THE STAR")
          )
        ,
          ( "Orne Library"
          , "The catalog for the restricted section of the library is far more byzantine than you expected. You try your best to make sense of the archaic filing system (lore). If you pass, you find the reference volume you were looking for and, next to it, an additional book that doesn't seem to be in the library's records at all; you may research one clue and gain one tome."
          , pass Lore 0 (Seq [Custom "don-research-one", tomeItem])
          )
        ,
          ( "Science Building"
          , "Professor Liebermann rubs the bald patch between his tufts of grey hair. \"I am almost certain that this device will magnify your inherent resonance,\" he says. \"But it is missing a crucial catalyst! Have you anything suitable?\" You may spend one remnant to gain $2 and focus one skill of your choice."
          , mayPay (SpendRemnants 1) (Seq [money 2, focusAny])
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Observatory"
          , "A small, jagged stone cracks against the steel grating of the observation deck, revealing an odd metal fetish statue; gain one remnant (observation). If you pass, you shelter against the wall as a volley of additional stones pelt down around you. If you fail, the stone rain catches you in the open; suffer one damage."
          , Seq [remnants 1, Test Observation 0 NoEffect (damage 1)]
          )
        ,
          ( "Orne Library"
          , "You don't recall seeing that door before. It's made of heavy, dark wood but opens easily at your touch. Within is a single book lying on a reading stand. Written on a placard above the book is a simple message: \"Some knowledge is forbidden for a reason.\" You may gain a DARK PACT. If you do, gain one spell and become BLESSED."
          , mayPay (CostCondition "DARK PACT") (Seq [spell, blessed])
          )
        ,
          ( "Science Building"
          , "The usual hum of machinery and bubbling of strange liquids is put on hold tonight for some sort of gala or soiree. Undergraduates brave the gauntlet of hostile conversation for a chance at free food and discreet liquor, and you could do the same. You may become delayed for you or an ally to recover two health and two sanity."
          , mayPay CostDelayed (both 2 2)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Observatory"
          , "Professor Tremaine's latest observations have been written on the chalkboard for anyone to see (lore). If you pass, you determine that the passage of the planets will soon place them in a favorable alignment; remove one doom from any space. If you fail, you find nothing of interest in her notes."
          , pass Lore 0 (RemoveDoomFrom AnySpace (N 1))
          )
        ,
          ( "Orne Library"
          , "\"What book did you say you wanted?\" asks Henry Armitage, the head librarian. \"What a curious title; I'm fond of it myself.\" Gain one spell and test lore. If you pass, Armitage is impressed with your knowledgeable erudition and offers you a job; become a LIBRARY DOCENT."
          , Seq [spell, pass Lore 0 (named "LIBRARY DOCENT")]
          )
        ,
          ( "Science Building"
          , "You agree to be a part of a research project dedicated to using radio waves to scan for projected subliminal thought. Despite your skepticism, you are shocked to hear your own voice crackle through the speakers. When that voice starts screaming for help, the grad student abruptly ends the experiment. Suffer one horror and gain $3."
          , Seq [horror 1, money 3]
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Observatory"
          , "A group of students have pinned a cluster of photographs and tables of data to a large board in the lecture hall (lore). If you pass, you notice a correlation between several groups of stars and some of the odd events you've been tracking in Arkham; remove up to one doom each from any two spaces."
          , pass Lore 0 (RemoveDoomFrom (DifferentSpaces 2 []) (N 1))
          )
        ,
          ( "Orne Library"
          , "The thesis in your hands describes a method of auto-hypnosis that will \"allow you to unlock the power hidden within your ancestral memory\" (lore). For each success you roll, reveal one spell from the deck; if you reveal one or more cards, gain one of those revealed spells and place the others on the bottom of the deck."
          , Test Lore 0 (Custom "don-ancestral-memory") NoEffect
          )
        ,
          ( "Science Building"
          , "Through the one-way glass, you can see a student strapped to a chair. When you flip the switch, the student screams in pain. \"Please flip the switch again,\" says the woman with the clipboard (will). If you pass, you refuse to be a part of this warped experiment any longer; become BLESSED."
          , pass Will 0 blessed
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Observatory"
          , "While you wait for your turn at the telescope, you enjoy the brilliant sparkle of distant stars. You may focus one skill of your choice, even if it exceeds your focus limit. Finally, your turn comes (observation). If you pass, you manage to capture an image of something silhouetted against the moon; gain one remnant."
          , Seq [focusExceed, pass Observation 0 (remnants 1)]
          )
        ,
          ( "Orne Library"
          , "As you read the words aloud, the text on the page begins to coruscate with violet fire. Confirming that it's cool to the touch, you press on (lore). If you pass, the invocation imprints itself in your mind; gain one spell. If you fail, a missed syllable causes the purple flame to scorch your memories away; suffer two horror."
          , Test Lore 0 spell (horror 2)
          )
        ,
          ( "Science Building"
          , "\"Hey, you look like you know what's going on!\" The student explains that he's been noticing odd things on campus for weeks, and he wants to help solve whatever's happening. You may spend one remnant to show him what he's up against. If you do, PETER SYLVESTRE joins you."
          , mayPay (SpendRemnants 1) (named "PETER SYLVESTRE")
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Observatory"
          , "Within moments, you see five shooting stars streaking through the sky (observation). If you pass, you realize that they are all converging at a point above the city; remove one doom from any space. If you fail, you vividly imagine one of those lights smashing into the city below you; suffer one horror."
          , Test Observation 0 (RemoveDoomFrom AnySpace (N 1)) (horror 1)
          )
        ,
          ( "Orne Library"
          , "Your reading starts with Murray's The Witch-Cult in Western Europe and moves from there into von Junzt's Unaussprechlichen Kulten, and before you know it, the library has closed around you and you are alone in the dark. Distant thumping and what might be a scream send you out the door. Suffer two horror and gain two spells."
          , Seq [horror 2, spell, spell]
          )
        ,
          ( "Science Building"
          , "You get caught up in a vigorous debate about whether or not to proceed with the radioisotope spectralyzer test (influence). If you pass, the test is a complete success and Professor Liebermann slips you a reward; gain $3. If you fail, all you manage is to make new enemies in the scientific community; become WANTED."
          , Test Influence 0 (money 3) wanted
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Observatory"
          , "Professor Tremaine is struggling to make sense of her observations, plotting lunar and solar eclipses back through history (lore). If you pass, she's grateful for your help and offers you a lump of meteoric iron \"for luck\"; gain one remnant."
          , pass Lore 0 (remnants 1)
          )
        ,
          ( "Orne Library"
          , "The collection before you includes volumes collected from dozens of occult scholars and arcanists. Draw two spells from the deck; gain one and put the other at the bottom of the deck. As you open another tome, you see the specter of its previous owner (will). If you fail, the apparition locks its mad eyes with yours; suffer two horror."
          , Seq [spells 2 (Just 1) (FlatPrice 0), Test Will 0 NoEffect (horror 2)]
          )
        ,
          ( "Science Building"
          , "A young graduate student lurks outside, kicking rocks with her hands jammed deep in her pockets. \"My thesis proposal got rejected,\" she explains. \"I have nothing to write about.\" You may spend one remnant to inspire her thesis and gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ]
      )
    ]

northside :: [CardDef]
northside =
  map
    (uncurry (card "northside"))
    [
      ( 1
      ,
        [
          ( "Arkham Advertiser"
          , "\"Whose bloody idea was it to run a poetry contest?\" complains editor Doyle Jeffries. \"I can't read this tripe. You there, you do it\" (will)! If you pass, you read through the submissions and find some gems that are genuinely inspiring; become BLESSED. If you fail, you pick a winner at random."
          , pass Will 0 blessed
          )
        ,
          ( "Curiositie Shoppe"
          , "Oliver Thomas's cat jumps into your arms, purring happily as you pick through shelf after shelf of forgotten keepsakes and unusual statues. You may buy any number of curios from the display. When you put him down, The Baron looks at you in shock, appalled that you would ever abandon him. Then he wanders off without a second glance."
          , buyAny "Curio"
          )
        ,
          ( "Train Station"
          , "A piece of luggage topples off of the old porter's cart and pops open, spilling personal effects all over the platform. You help your harried fellow traveler gather their belongings and attempt to calm them down (influence). If you pass, you hit it off; gain one ally. If you fail, the paranoid traveler is certain that you stole something; become WANTED."
          , Test Influence 0 ally wanted
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Arkham Advertiser"
          , "Minnie Klein offers to introduce you to one of her informants, but only if you can convince her that you're a valuable resource as well. You may give her one remnant to prove to her that you're a reliable contact. If you do, she arranges a meeting with a sympathetic police deputy; gain a TRUSTED SOURCE."
          , mayPay (SpendRemnants 1) (named "TRUSTED SOURCE")
          )
        ,
          ( "Curiositie Shoppe"
          , "The Baron, a large cat with an imperious stare, follows you through the store, watching intently as you inspect the merchandise. You could almost swear he's judging your purchases, but you cannot determine his criteria. You may buy any number of curios from the display."
          , buyAny "Curio"
          )
        ,
          ( "Train Station"
          , "You arrive at the train station, happy to collect an old friend, newly arrived in town. While you wait, the clock on the platform ticks away with a soothing regularity (will). If you succeed, you stay alert and watch the train roll in. If you fail, you awake hours later to your friend's gentle shake; become delayed. Either way, gain one ally."
          , Seq [Test Will 0 NoEffect delayed, ally]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Arkham Advertiser"
          , "Doyle Jeffries, the editor of The Arkham Advertiser, tells you that he's looking for material to research an expose he's working on. You may give him one remnant to support his story. If you do, he tells you where you can find evidence for your own research; spawn one clue."
          , mayPay (SpendRemnants 1) SpawnOneClue
          )
        ,
          ( "Curiositie Shoppe"
          , "Motes of dust drift delicately through colored light streaming from the stained glass over the shop's door (observation). If you pass, the dancing light leads you to a well-worn penny stamped with the year you were born; become BLESSED. If you fail, you cough and wave the dust out of your face."
          , pass Observation 0 blessed
          )
        ,
          ( "Train Station"
          , "The bare branches of the birch trees clatter against each other in the wind, the irregular rhythm lulling you into a dull-eyed trance (will). If you pass, you shake your head clear when an old friend taps you on the shoulder; gain one ally. If you fail, the sudden touch startles you, and you strike the stranger in your panic."
          , pass Will 0 ally
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Arkham Advertiser"
          , "Doyle Jeffries hires you to proofread a feature for tomorrow's paper. Gain $2. While you work, you hear two of the staff writers arguing about whether or not they can run a story with insufficient proof. You may spend one remnant to help support the piece; if you do, the grateful reporter pays you $2."
          , Seq [money 2, mayPay (SpendRemnants 1) (money 2)]
          )
        ,
          ( "Curiositie Shoppe"
          , "Oliver Thomas offers to wrap your purchases up for you. You may buy any number of curios from the display. If you buy anything, you find an extra parcel with your things, wrapped in brown paper and tied with twine; gain the PUZZLE BOX. If you try to return the extra item, Oliver doesn't recognize the box and lets you keep it."
          , buyAnyThen "Curio" (named "PUZZLE BOX")
          )
        ,
          ( "Train Station"
          , "The handsome salesman has a suitcase full of goods for sale on the train platform. With a wink and a smile, Ryan Dean offers you a bargain. You may buy one common item for half price (rounded up). If you don't buy anything, another traveler agrees that the salesman doesn't seem to be quite on the level; gain one ally."
          , BuyFromDisplay (Just "Common") HalfPrice (Just 1) NoEffect
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Arkham Advertiser"
          , "The new hire wants to start running an advice column, but her editor insists she finish a \"real\" story first. You may spend one remnant to help her research an article to keep her boss happy. If you do, she pays you for your time and offers some free advice; gain $3 and focus one skill of your choice."
          , mayPay (SpendRemnants 1) (Seq [money 3, focusAny])
          )
        ,
          ( "Curiositie Shoppe"
          , "You peruse the shop shelves, pausing to study a small painted cameo of a stern-looking woman. The engraving on the back of the frame looks like more than a dedication (lore). If you pass, gain one spell. If you fail, you can feel the woman's disapproval; suffer one horror. You may continue your shopping and buy any number of curios from the display."
          , Seq [Test Lore 0 spell (horror 1), buyAny "Curio"]
          )
        ,
          ( "Train Station"
          , "Bill Washington sits by himself, repairing the broken wheel on his luggage cart. You join him for a chat while he works (influence). If you pass, the time passes quickly and the old porter's rich laughter nourishes your spirit; become BLESSED. If you fail, he finds your awkward attempt at small talk distracting and waves you away."
          , pass Influence 0 blessed
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Arkham Advertiser"
          , "You submit a story to the editor implicating a few well-heeled families in the Sheldon gang's smuggling operation; gain $3. When Sadie's mooks corner you, you insist that you could never reveal a source (influence). If you fail, they are unmoved by your principles and promise to return; become WANTED."
          , Seq [money 3, Test Influence 0 NoEffect wanted]
          )
        ,
          ( "Curiositie Shoppe"
          , "Oliver Thomas offers you a discount on a piece that's been on the shelf for a while. You may buy one curio from the display for half price (rounded up). On your way out, a man with a nasty, puckered scar over his eye demands to know what you bought (influence). If you fail, he doesn't believe your feeble misdirection; become WANTED."
          , Seq [buyOneHalf "Curio", Test Influence 0 NoEffect wanted]
          )
        ,
          ( "Train Station"
          , "You spot a bit of paper on a bench, weighted down against the wind with a handful of stones, and on a closer look, find that it's a tarot card. Gain THE WORLD. As you glance about to see who left this here, you notice the \"stones\" are really vertebrae (will). If you fail, suffer one horror."
          , Seq [named "THE WORLD", Test Will 0 NoEffect (horror 1)]
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Arkham Advertiser"
          , "A staff writer is having trouble with copy for a puff piece about the new bocce ball court downtown. You can provide evidence for a real story by either giving them one remnant or doing your best to describe your adventures (observation). If you pass or spend the remnant, they write your story instead; gain $3."
          , orPay Observation "Spend one remnant" (SpendRemnants 1) (money 3)
          )
        ,
          ( "Curiositie Shoppe"
          , "The shop is dark, and covered in so much dust you'd swear no one has been here in weeks. When you ring the bell on the counter, you hear whispered voices behind you (will). If you pass, you turn to look and find a parcel on the floor with your name on it in gold script; gain one curio. If you fail, you turn and leave without a word."
          , pass Will 0 curioItem
          )
        ,
          ( "Train Station"
          , "The stranger is newly arrived in town, and pretty rattled by the stories told on the train about Arkham's past (influence). If you pass, you confirm that there's a lot of truth to the rumor, but assure them that you're working to preserve the future; gain one ally. If you fail, they buy the next ticket out of town."
          , pass Influence 0 ally
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Arkham Advertiser"
          , "\"I can't run this dreck!\" There's a flurry of papers as Doyle Jeffries, the editor, throws out tomorrow's front page story. He calls to you, \"Are you as useless as my staff?\" You may spend one clue or one remnant to give him the basis for a story that belongs above the fold. If you do, gain $4."
          , Choose
              [ ("Spend one clue", Pay (SpendClues 1) (money 4))
              , ("Spend one remnant", Pay (SpendRemnants 1) (money 4))
              , ("Decline", NoEffect)
              ]
          )
        ,
          ( "Curiositie Shoppe"
          , "The old wardrobe won't open, but you and The Baron, the owner's cat, can hear something knocking on the inside. \"I just picked that up at an auction in Maine,\" says Oliver Thomas as you try to pull it open (strength). If you pass, you feel a cold wind blow past you and spot a single object lying forlornly on the floor of the cabinet; gain one curio."
          , pass Strength 0 curioItem
          )
        ,
          ( "Train Station"
          , "Bill Washington, the old porter, brings you a weathered suitcase with your name on the luggage tag and tells you it was left on the train from Kingsport. You don't recognize it, and you certainly don't know the combination for the lock (will). If you pass, you try your mother's birthday and it opens right up; gain one common item."
          , pass Will 0 commonItem
          )
        ]
      )
    ]

rivertown :: [CardDef]
rivertown =
  map
    (uncurry (card "rivertown"))
    [
      ( 1
      ,
        [
          ( "Black Cave"
          , "You've heard rumors about an old witch who used to live in this cave, and you think you may have stumbled upon one of her ritual sites (lore). If you pass, you recognize the pentacle on the floor as a symbol of protection and sit for a while listening to the spirits around you; become BLESSED."
          , pass Lore 0 blessed
          )
        ,
          ( "General Store"
          , "You stand for a while with the shopkeep, watching the rain patter against the front window. You or an ally recovers one sanity. When a grocery truck lumbers by, the rumbling shakes you from your shared reverie and Davy Schoffner turns to you: \"What can I help you with today?\" You may buy any number of common items from the display."
          , Seq [sanity 1, buyAny "Common"]
          )
        ,
          ( "Graveyard"
          , "You can hear movement all around you in the twilight; feral snarls and the hard click of claws on weathered headstones echo as you search the newly open grave (will). If you pass, you stay calm and complete your investigation; gain $3. If you fail, you break for the road, with the ghouls snapping at your heels; suffer two horror."
          , Test Will 0 (money 3) (horror 2)
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Black Cave"
          , "You stumble across a group of armed men hauling a pair of crates out of the cave. Either give them a remnant to show you've got something to trade or start talking (influence). If you pass or spend a remnant, they'll do business with you; gain the SMUGGLER CONTACTS. If you fail, suffer two damage."
          , Choose
              [ ("Test influence", Test Influence 0 (named "SMUGGLER CONTACTS") (damage 2))
              , ("Spend one remnant", Pay (SpendRemnants 1) (named "SMUGGLER CONTACTS"))
              ]
          )
        ,
          ( "General Store"
          , "Due to an error in his bookkeeping, Schoffner's stockroom is full to bursting. You may discard and replace up to two items from the display. The shopkeep has marked a few things down with a discount to move the overstock. You may buy one common item from the display for half price (rounded up)."
          , Seq [Custom "don-cycle-display-2", buyOneHalf "Common"]
          )
        ,
          ( "Graveyard"
          , "About a dozen bodies are scattered around what's left of a ritual site among the mausoleums. Gain one remnant. You may suffer one horror to search through the carnage more carefully. If you do, you confirm that the cultists must have lost control over whatever they tried to summon; gain an additional remnant."
          , Seq [remnants 1, mayPay (CostHorror 1) (remnants 1)]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Black Cave"
          , "Someone is singing in the back of the cave, their voice echoing softly off of the damp walls. You do your best to listen to lyrics in a language you've never heard before (lore). If you pass, they make an odd kind of sense; gain one spell. If you fail, a shiver runs down your spine as you leave."
          , pass Lore 0 spell
          )
        ,
          ( "General Store"
          , "There's a pretty young woman dressed in natty, patched trousers and a woolen vest out front of Schoffner's store, busking with an old and worn violin. You may put $1 in her open instrument case. If you do, she smiles warmly and her skilled playing reveals her classically-trained technique; become BLESSED."
          , mayPay (SpendMoney 1) blessed
          )
        ,
          ( "Graveyard"
          , "The door to the musty mausoleum has been roughly prised open and hangs from its hinges like a broken puppet (will). If you pass, you work up the nerve to enter the dark stone burial vault, where you find footprints leading away from empty crypts and the scattered remains of some arcane ritual; gain one remnant."
          , pass Will 0 (remnants 1)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Black Cave"
          , "The skeleton seems far older than the oilskin-wrapped object clutched in its bony fingers (will). If you pass, you carefully take the bundle from its hands and unwrap it; gain one curio. If you fail, the skull's gaping eyes pierce right through you; suffer one horror."
          , Test Will 0 curioItem (horror 1)
          )
        ,
          ( "General Store"
          , "You may buy any number of common items from the display. If you buy anything, Davy Schoffner writes your name in the ledger and allows you to open an account, saying \"If you need anything else, you can just call me up and I'll send it over with Nathan;\" gain the SCHOFFNER'S CATALOGUE."
          , buyAnyThen "Common" (named "SCHOFFNER'S CATALOGUE")
          )
        ,
          ( "Graveyard"
          , "Leonard Coburn, the groundskeeper, is desperate for help, and hires you to dig a grave. Gain $3. The ground is hard, and the work is slow going (strength). If you fail, your work takes you into the night, and you are caught in the middle of an illicit deal between the O'Bannions and Johnny Valone; become WANTED."
          , Seq [money 3, Test Strength 0 NoEffect wanted]
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Black Cave"
          , "The matron smiles warmly as she passes you a cup of tea brewed from herbs and cave moss. She bids you to drink (will). If you pass, the fragrant tea conjures a ghostly vision of an ancient coven, happy to guide you through the craft; gain one spell. If you fail, you flee the cave; suffer one horror."
          , Test Will 0 spell (horror 1)
          )
        ,
          ( "General Store"
          , "You may buy any number of common items from the display. If you buy anything, Davy Schoffner offers you a cup of coffee while Nathan, the delivery boy, packs up your purchases; you or an ally recovers one sanity for each item you purchased."
          , buyAnyThen "Common" (sanity 1)
          )
        ,
          ( "Graveyard"
          , "The ground has grown uneven with the passing of time, and a large marble grave marker has fallen over onto the path. Careful to lift with your knees, you set the toppled headstone upright again (strength). If you pass, a sense of peace washes over you; become BLESSED. If you fail, the stone slips from your grasp, falls, and cracks on the ground."
          , pass Strength 0 blessed
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Black Cave"
          , "Students from the university study the carvings on the walls, and their idle chatter begins to sound more and more like alien gibberish (will). If you pass, you adjust to the echoing noise and befriend the professor; gain one curio. If you fail, they report your screaming to the police; become WANTED."
          , Test Will 0 curioItem wanted
          )
        ,
          ( "General Store"
          , "Davy Schoffner's ladder wobbles a bit as he cleans the shop's sign and touches up the paint. You offer to hold the ladder while he works (influence). If you pass, he's grateful for the assistance and complains that Nathan has skived off work for the day; gain one common item as payment for your help."
          , pass Influence 0 commonItem
          )
        ,
          ( "Graveyard"
          , "A shovel, its handle snapped cleanly in half, is the only evidence of activity near this fresh grave. You breathe in the rich, warm smell of the fresh earth, and wonder who is to be interred (will). If you pass, gain a handful of GRAVE DIRT. If you fail, the abandoned site allows your imagination to run wild; suffer one horror."
          , Test Will 0 (named "GRAVE DIRT") (horror 1)
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Black Cave"
          , "You glimpse the night sky through a small stone chimney at the back of the cave. Gain one spell as a shooting star streaks by and a rush of energy surges into your mind (lore). If you pass, you channel it into the ley line beneath you; focus one skill of your choice, even if it exceeds your focus limit."
          , Seq [spell, pass Lore 0 focusExceed]
          )
        ,
          ( "General Store"
          , "Old Man Hatle is holding court, as usual, over the checkers table by the potbellied stove. Davy Schoffner smiles and shakes his head doubtfully at the old man's story about catching the women from the quilting circle at the church in the middle of some kind of seance. You may buy any number of common items from the display."
          , buyAny "Common"
          )
        ,
          ( "Graveyard"
          , "As you listen to the rain fall among the crypts, you don't sense your attacker until his arm is already around your neck (strength). If you pass, you fight the man off, snatching the beads from his neck before he retreats into the trees; gain one remnant. If you fail, you struggle to get free and the man slams you into a headstone; suffer one damage."
          , Test Strength 0 (remnants 1) (damage 1)
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Black Cave"
          , "The old woman is gathering bits of cave moss into a wicker basket with her knitting supplies. Either test lore to tell her what you know about the history of the cave or spend one remnant to show her the trials you've faced. If you pass or spend the remnant, she offers a gift from her basket; gain one curio."
          , orPay Lore "Spend one remnant" (SpendRemnants 1) curioItem
          )
        ,
          ( "General Store"
          , "The shop is warm and comfortable thanks to the pot-bellied stove in the back. You may buy any number of common items from the display. If you buy anything, Davy Schoffner pores over the ledger as he writes your receipt and concludes that Nathan, the delivery boy, must have priced something incorrectly; gain $1."
          , buyAnyThen "Common" (money 1)
          )
        ,
          ( "Graveyard"
          , "Something has dug up one of the graves and torn the body apart. You may test will to follow the creature to its lair or become delayed to set a trap for the beast's return. If you pass or become delayed, you corner the crazed ghoul and put it down, collecting the remains of its victims for reinterment; gain $3 and one remnant."
          , orPay Will "Become delayed" CostDelayed (Seq [money 3, remnants 1])
          )
        ]
      )
    ]

southside :: [CardDef]
southside =
  map
    (uncurry (card "southside"))
    [
      ( 1
      ,
        [
          ( "Historical Society"
          , "The assistant curator is desperate for another piece to round out the collection for his exhibit on local history, but doesn't have the funds to acquire a new artifact. You may give him one remnant. If you do, you receive nothing but his gratitude; become BLESSED."
          , mayPay (SpendRemnants 1) blessed
          )
        ,
          ( "Ma's Boarding House"
          , "You stop by to get a share of Ma Mathison's hearty breakfast and chew the fat with her lodgers. You or an ally recovers two health. After the meal, a truck driver from Innsmouth brushes off the brim of her cap and offers you a ride, provided you can pay your way and don't mind the smell of fish. You may spend $1 to move up to two spaces."
          , Seq [health 2, mayPay (SpendMoney 1) (MoveUpTo 2)]
          )
        ,
          ( "South Church"
          , "The church sanctuary is tranquil, with only the sound of the rain on the roof. You or an ally recovers two sanity. You hear the door close softly behind you, and see a young boy, dripping wet, hungry, and alone. You may give him $1 and a few kind words to make his life easier. If you do, you or an ally recovers two sanity."
          , Seq [sanity 2, mayPay (SpendMoney 1) (sanity 2)]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Historical Society"
          , "The Society is planning a new exhibit, and Mr. Peabody is eager to acquire new specimens. You may donate a remnant to help the curator improve the new installation. If you do, the grateful man informs you that he's always interested in any goods you'd like to bring him; become a VALUED DONOR."
          , mayPay (SpendRemnants 1) (named "VALUED DONOR")
          )
        ,
          ( "Ma's Boarding House"
          , "The boarding house always has a room open for you to stop by for a short rest when you need it. You or an ally recovers one health. After a nap and a hot bath, you find Ma Mathison plating up thick slices of hearty meatloaf with roasted vegetables and crusty bread. You may spend $1 for you or an ally to recover two health."
          , Seq [health 1, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "South Church"
          , "Father Michael is always ready to provide a friendly ear, words of comfort, and wise counsel, even to those who don't share his faith. All he asks is a small donation so that the church may continue to provide services for Arkham's disadvantaged. You may spend $1 for you or an ally to recover three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Historical Society"
          , "The other patron is scouring the exhibits looking for something very specific. When you ask, they describe an event similar to things you've encountered yourself. You may give them one remnant to show that you know what they're going through. If you do, gain one ally."
          , mayPay (SpendRemnants 1) ally
          )
        ,
          ( "Ma's Boarding House"
          , "The rain lets up just as you turn off the street toward the boarding house, and you find yourself bathed in clear moonlight. Become BLESSED. Whistling as you press through the front door, you are delighted to find Ma serving up your favorite meal. You may spend $1 for you or an ally to recover two health."
          , Seq [blessed, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "South Church"
          , "The ladies' quilting circle invites you to have a cup of sweet-smelling herbal tea. You or an ally recovers two sanity. As you sit with them, you find great comfort in the sounds of their gentle work and comfortable chatter, and are tempted to stay for a while. You may become delayed for you or an ally to recover two additional sanity."
          , Seq [sanity 2, mayPay CostDelayed (sanity 2)]
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Historical Society"
          , "The woman visiting from the Universidad Nacional de Mexico gives a captivating speech about early religions and cults. After the seminar, you may spend one remnant to prompt further discussion with the guest lecturer. If you do, another attendant is impressed with your knowledge; gain one ally."
          , mayPay (SpendRemnants 1) ally
          )
        ,
          ( "Ma's Boarding House"
          , "Ma serves you a generous slice of exquisite pie. You or an ally recovers two health. \"I've got another one cooling on the sill. It's yours if you spend some time cleaning up the brush out back.\" You may become delayed to help her with the yard work. If you do, she packs the pie into a wicker basket for you; gain MA'S APPLE PIE."
          , Seq [health 2, mayPay CostDelayed (named "MA'S APPLE PIE")]
          )
        ,
          ( "South Church"
          , "Water drips through the shingles of the church's leaky roof, disturbing the otherwise silent peace of the sanctuary. You may donate $1 to help refurbish the aging church. If you do, you feel confident that you've helped make a difference; focus two skills of your choice, even if it exceeds your focus limit."
          , mayPay (SpendMoney 1) (Seq [focusExceed, focusExceed])
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Historical Society"
          , "Try as you might, you simply don't notice or remember the woman's face. When she speaks, in a voice simultaneously soothing and unsettling, she offers you a gift in exchange for a favor. You may gain a DARK PACT to gain two curios, but you can't recall later what she asked of you."
          , mayPay (CostCondition "DARK PACT") (Seq [curioItem, curioItem])
          )
        ,
          ( "Ma's Boarding House"
          , "Passing the night by Ma's fireplace, you discuss recent events with the other boarders, doing your best to describe what you've witnessed without sparking panic (influence). If you pass, one of the other guests promises to help you; gain one ally. If you fail, the rattled guests retire to their rooms and lock the doors."
          , pass Influence 0 ally
          )
        ,
          ( "South Church"
          , "A box of quilting supplies has been ruined by water seeping in through the walls of the church basement. You may spend $2 to help the quilting circle replace their lost material. If you do, they invite you to help them finish their latest project, featuring a five-pointed star on a large blue blanket; become BLESSED."
          , mayPay (SpendMoney 2) blessed
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Historical Society"
          , "Ruby Standish thrusts a small item into your hands before she runs off into the night. Gain one curio. Moments later, the security guard is demanding answers with a flashlight in your face (influence). If you fail, he isn't satisfied when you explain that you don't know where she went; become WANTED."
          , Seq [curioItem, Test Influence 0 NoEffect wanted]
          )
        ,
          ( "Ma's Boarding House"
          , "Sheriff Engle is tucking into a large slice of apple pie, holding court with Ma and the other guests. After a short wait, you manage to attract Ma's attention and pay to rent a room. You may spend $1 for you or an ally to recover three health. On your way upstairs, the room shares a laugh when the Sheriff describes \"some crackpot theory about cults.\""
          , mayPay (SpendMoney 1) (health 3)
          )
        ,
          ( "South Church"
          , "The bird hops along the railing, following as you walk the perimeter of the churchyard. When you speak to it, it cocks its head quizzically and calls to you, squawking rhythmically in response. You may extend your arm to the bird in greeting. If you do, it lands on your gloved hand; the FRIENDLY RAVEN joins you."
          , May "Extend your arm to the bird" (named "FRIENDLY RAVEN")
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Historical Society"
          , "Another patron paces around the gallery, stopping now and then to scrutinize the symbols etched on several of the artifacts. \"I swear I've seen that design before,\" they explain to you, \"and it always leads to trouble\" (influence). If you pass, you convince them that you can handle that kind of trouble; gain one ally."
          , pass Influence 0 ally
          )
        ,
          ( "Ma's Boarding House"
          , "The truck driver explains that he can't return to Dunwich until his truck gets a new carburetor, but admits that he's in no rush to leave \"the big city.\" You may spend $1 for you or an ally to recover two health while you talk to the man over dinner and pie about his hometown."
          , mayPay (SpendMoney 1) (health 2)
          )
        ,
          ( "South Church"
          , "Despite the building's age and poor state of repair, the art and architecture of the old Catholic church is still inspiring. You may focus one skill of your choice. On your way out, you pass the church's donation box, promising to use any proceeds to help the poor and needy. You may spend $1 for you or an ally to recover two sanity."
          , Seq [focusAny, mayPay (SpendMoney 1) (sanity 2)]
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Historical Society"
          , "Mr. Peabody, the curator of the society's museum, eyes you skeptically while you explain that the materials in his collection could be instrumental to stopping something terrible. You may spend one remnant to convince him that the threat to Arkham is very real. If you do, gain one curio."
          , mayPay (SpendRemnants 1) curioItem
          )
        ,
          ( "Ma's Boarding House"
          , "Ma Mathison is serving up a heaping portion of beef stroganoff, made from an old family recipe. You may spend $1 for you or an ally to recover two health. If you do, the tangy sauce reminds you of all the best meals of your childhood; you may focus one skill of your choice, even if it exceeds your limit."
          , mayPay (SpendMoney 1) (Seq [health 2, focusExceed])
          )
        ,
          ( "South Church"
          , "You duck out of the rain through the wide open front doors of the church, and sit for a while listening to the singer practicing unseen in the choir loft (will). If you pass, the music affirms your hope for the future; you or an ally recovers four sanity. If you fail, you look up at the empty loft above you and wonder at the voice."
          , pass Will 0 (sanity 4)
          )
        ]
      )
    ]

uptown :: [CardDef]
uptown =
  map
    (uncurry (card "uptown"))
    [
      ( 1
      ,
        [
          ( "Hangman's Hill"
          , "Thousands of crows perch in the trees overhead, watching you intently (will). If you pass, you take comfort in the vibrant natural life around you; become BLESSED. If you fail, the cawing gets under your skin and you scamper away, leaving the murder in the trees behind you."
          , pass Will 0 blessed
          )
        ,
          ( "St. Mary's Hospital"
          , "Despite her fatigue from a long shift, Doctor Maheswaren works with professional poise while you share your findings and ask her about the sudden influx of patients. Spawn one clue. You may spend $1 for you or an ally to recover three health. She agrees that she'll get back to work if you'll do the same."
          , Seq [SpawnOneClue, mayPay (SpendMoney 1) (health 3)]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "The proprietor drums her fingers impatiently while you browse, eager to close for the night. You realize suddenly that you've been in the store for hours and sheepishly bring your selections up to the counter. Become delayed and reveal the top four spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
          , Seq [delayed, spells 4 Nothing FullPrice]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Hangman's Hill"
          , "Your wonder at the small black flowers quickly turns to fear when the plants reach out to you, their grasping leaves enveloping your arm (strength). If you pass, you tear free and keep a handful of the sweet-smelling flowers; gain the WITCHWEED. If you fail, you are pinned under the foliage; become delayed."
          , Test Strength 0 (named "WITCHWEED") delayed
          )
        ,
          ( "St. Mary's Hospital"
          , "Nurse Sharon tells you she's got some space in her schedule to clean and dress your minor injuries. You or an ally recovers two health. Apologizing that she can't be of more help right now, she encourages you to check in at the desk if you need further care. You may spend $1 for you or an ally to recover two additional health."
          , Seq [health 2, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "Miriam Beecher follows you while you browse, happily summarizing the seminar she recently attended about ley lines and theoretical geometry. You may focus lore. Reveal the top three spells in the deck. You may buy one of them for half price (rounded down). Put the rest on the bottom of the deck."
          , Seq [Focus (Just Lore) False, spells 3 (Just 1) HalfPrice]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Hangman's Hill"
          , "There's an unattended object lying in the dirt path of the old chapel; gain one common item. As you lift the item out of the loamy earth, you realize it is sticky and warm (will). If you pass, you calmly clean it off. If you fail, you brush a hand over your face, leaving a long smear of blood; suffer one horror."
          , Seq [commonItem, Test Will 0 NoEffect (horror 1)]
          )
        ,
          ( "St. Mary's Hospital"
          , "Exhausted from a long volunteer shift, you collapse into the cot in the lounge. You dream vividly of the old man the doctors couldn't save (will). If you pass, you find a new comfort in the end of his suffering; become BLESSED. If you fail, he blames you and the rest of Arkham for failing him; suffer one horror."
          , Test Will 0 blessed (horror 1)
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "The shelves that crowd in around you, loaded with books, diagrams, and all kinds of mystic paraphernalia, are simultaneously comforting and suffocating. Reveal the top three spells in the deck. You may buy any number of them. Put one of the remaining cards on top of the deck and the rest on the bottom of the deck."
          , spells 3 Nothing FullPrice
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Hangman's Hill"
          , "You see movement in the old chapel, and hear rasping voices in hushed conversation. You creep up to peek into the long broken window, trying to breathe smoothly and evenly (will). If you pass, you master your nerves and creep forward, but see only a tattered scarf draped over a pew; gain one remnant."
          , pass Will 0 (remnants 1)
          )
        ,
          ( "St. Mary's Hospital"
          , "Nurse Chapman is only too happy to get you back on your feet. \"I've seen what this town can do to people, and I've got to help how I can.\" You or an ally recovers two health. If you still have one or more damage, she quickly packs a kit and follows you out, promising to help you protect the citizens of Arkham; MAEVE CHAPMAN joins you."
          , Seq [health 2, Custom "don-maeve-chapman"]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "The ornate sundial in the yard of Miriam Beecher's shop must be misaligned; as you listen to the clock tower chime on the distant university campus, the shadow cast by the arm of the dial sweeps wildly across its face. As you gape at this inexplicable movement, you realize that you know more than you used to. Gain one spell."
          , spell
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Hangman's Hill"
          , "A long scrap of fabric is snagged on the thorny vines, embroidered with dozens of arcane symbols. Gain one remnant. Deeper in the vines you see something wrapped in the rest of the robe (strength). If you pass, you push through the vines to reach the bundle; gain one common item."
          , Seq [remnants 1, pass Strength 0 commonItem]
          )
        ,
          ( "St. Mary's Hospital"
          , "You may spend $1 for you or an ally to recover two health. Doctor Mortimore distractedly performs the exam, while keeping an eye on the orderly in the hallway. He admits when you press him that he suspects that the man has been stealing medication from the supply closet, but hasn't been caught in the act yet."
          , mayPay (SpendMoney 1) (health 2)
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "Clear moonlight streams through the small ocular window at the front of the shop as Miriam Beecher shows you an assortment of tomes. Reveal the top three spells of the spell deck. You may buy any number of them; put the rest on the bottom of the deck. If you buy any, the light shines on your face as you learn the rite; become BLESSED."
          , Seq [spells 3 Nothing FullPrice, blessed]
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Hangman's Hill"
          , "The witchweed rustles around you, moved by a breeze you cannot feel. Following the movement of the small black blossoms, you find the mutilated body of a dog-faced humanoid with bizarre runes carved into its flesh. Gain two remnants and discard one focus as you find all the flowers facing you."
          , Seq [remnants 2, DiscardAFocus]
          )
        ,
          ( "St. Mary's Hospital"
          , "The surgeon, visiting from a hospital in Buenos Aires, leads you back into an examination room. She is remarkably adroit, and thanks you with a blank smile when you compliment her English, completely nonplussed. You may spend $1 for you or an ally to recover three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "When you trace your fingers over the black leather cover, you hear a woman's voice in your head (will). If you pass, you learn that the book belonged to a witch, murdered by cultists long ago; gain the BLACK GRIMOIRE. If you fail, the spirit tethered to the book overwhelms your mind and you awaken in the street; become delayed."
          , Test Will 0 (named "BLACK GRIMOIRE") delayed
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Hangman's Hill"
          , "The shipping crates were hastily covered with brush and long black vines of witchweed. There'd be no reason to disguise a crate of legitimate goods; you quickly pry the top off of the nearest crate (strength). If you pass, gain one common item. If you fail, the smugglers catch you in the act; become WANTED."
          , Test Strength 0 commonItem wanted
          )
        ,
          ( "St. Mary's Hospital"
          , "The nurse barely speaks to you as she dresses your injuries in a brisk, all-business manner. You or an ally recovers one health. As she ushers you out the door, you finally manage to get her undivided attention, moments before she sighs and hands you off to another nurse. You may spend $1 for you or an ally to recover three additional health."
          , Seq [health 1, mayPay (SpendMoney 1) (health 3)]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "Miriam Beecher tells you that she found the book at an estate sale down south, but it seems to be written in code (lore). If you pass, you find the key to the cypher and confirm that the book holds magical secrets; draw the top two spells in the deck, gain one of them and put the other on the bottom of the deck."
          , pass Lore 0 (spells 2 (Just 1) (FlatPrice 0))
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Hangman's Hill"
          , "The ghostly form of a young woman paces the mouldering church, but stops when she locks eyes with you (will). If you pass, you aren't afraid when she leads you to a hidden cache under a pew; gain one common item. If you fail, she echoes your scream and hurls a heavy bench at you; suffer two damage."
          , Test Will 0 commonItem (damage 2)
          )
        ,
          ( "St. Mary's Hospital"
          , "The surgery Doctor Mortimore recommends is expensive, but he swears it will get you right as rain and back on your feet. You may spend $2 for you or an ally to recover four health. As you recover from the anesthetic, you think you see small, winged creatures flitting all around the hospital, glowing like brightly-colored light bulbs."
          , mayPay (SpendMoney 2) (health 4)
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "The proprietor is only too happy to show you a large collection of books recently acquired from an estate sale in Providence. \"The owner went missing a few years ago, and the bank finally released his belongings for auction!\" Reveal the top four spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
          , spells 4 Nothing FullPrice
          )
        ]
      )
    ]

streets :: [CardDef]
streets =
  map
    (uncurry street)
    [
      ( 1
      ,
        [
          ( Residential
          , "The old man waves and invites you to join him on the porch and listen to the afternoon's New England League ballgame on his radio. You offer a bet favoring the Kingsport Crowns over the Lewiston Twins (influence). If you pass, you gain $2 when the Twins choke in the top of the seventh inning."
          , pass Influence 0 (money 2)
          )
        ,
          ( Bridge
          , "The Egyptian man in the fine suit offers you shelter from the sudden downpour underneath his umbrella. As you walk the length of the bridge together he shows a quick wit and a warm smile and reveals himself to be an antiquities dealer. You may buy one curio from the display as the rain clears and you part ways with a friendly handshake."
          , buyOne "Curio"
          )
        ,
          ( Scenic
          , "The sounds of a struggle from the underbrush alert you before you see a flash of patchy, ashen fur and a robed sleeve (observation). If you pass, you conceal yourself until the danger has passed and investigate the scene, finding a torn and blood-stained scrap of fabric, embroidered with occult symbols; gain one remnant."
          , pass Observation 0 (remnants 1)
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( Residential
          , "The truck rounds the corner with a screech and clips a parked auto, losing a headlight before it screams off into the night (observation). If you pass, you spot a small package that must have fallen off of the truck when it hit; gain one common item."
          , pass Observation 0 commonItem
          )
        ,
          ( Bridge
          , "Abner Weems sits on the side of the bridge, staring at the water below. In a rare moment of lucidity, the man offers his flask and invites you to join him. You or an ally recovers one health and one sanity while you sit with the old man and reminisce about past friends and lost loves."
          , both 1 1
          )
        ,
          ( Scenic
          , "You find a bundle of dried flowers tied to a birch tree with a faded length of black ribbon (lore). If you pass, your knowledge of floriography identifies that someone used the bouquet to offer a loved one up as some kind of sacrifice; gain a remnant. If you fail, you smile at a gesture you misidentify as one of affection."
          , pass Lore 0 (remnants 1)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( Residential
          , "Nathan waves to you from the Schoffner's delivery truck, and offers to hand off anything you'd like him to take to your colleagues as he drives his delivery route. You may trade with an investigator in any space as though you had performed the trade action in that space."
          , Custom "don-remote-trade"
          )
        ,
          ( Bridge
          , "You spot something bobbing in the water under the bridge, and as you get closer you can make out a humanoid shape in the murky water (will). If you pass, you take a deep breath and climb down to find a corpse with a large, pale-violet flower jammed into its black and bloated mouth; gain one remnant."
          , pass Will 0 (remnants 1)
          )
        ,
          ( Scenic
          , "You round a corner in your familiar shortcut, only to find an unexpected threat blocking the path. Spawn a monster in your space as you search desperately for an opening to escape (observation). If you pass, you may disengage all monsters and move up to two spaces."
          , Seq [SpawnMonsterIn YourSpace False, pass Observation 0 (Custom "don-slip-away")]
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( Residential
          , "Your pace quickens when you hear small, hurried footsteps on the cobblestones behind you. You risk a look back under the light of an electric street lamp (will). If you pass, you see the silhouette of a porcelain doll following you down the street. You break it with one good kick and gain one remnant."
          , pass Will 0 (remnants 1)
          )
        ,
          ( Bridge
          , "The sailor in the knit cap glowers at you as you pass him, before warning you about traveling through the city alone. You may become delayed to ask him if he's seen anything. If you do, he tells you that his partner never came back from shore leave, and he suspects something waylaid the man; spawn one clue."
          , mayPay CostDelayed SpawnOneClue
          )
        ,
          ( Scenic
          , "You spot a pair of sweet potato pies cooling on a windowsill (will). If you pass, you ring the bell, offer to do some odd jobs around the farm, and chop some firewood; you receive a pie for your efforts and you or an ally recovers two health. If you fail, you swipe the pies from the window; become WANTED."
          , Test Will 0 (health 2) wanted
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( Residential
          , "The stranger runs down the street, looking over their shoulder like they're being chased. You attempt to lead them to safety (influence). If you pass, you both hunker down in an alley and watch a pair of robed figures pass you on the street; gain one ally. If you fail, the stranger flees from you as well."
          , pass Influence 0 ally
          )
        ,
          ( Bridge
          , "A gravelly voice from the culvert under the bridge offers you power, if only you'll perform a small, almost-inconsequential favor. You may gain a DARK PACT to assist the being in the darkness and gain two spells. If you do not, you hurry away, looking cautiously over your shoulder."
          , mayPay (CostCondition "DARK PACT") (Seq [spell, spell])
          )
        ,
          ( Scenic
          , "The gravel road is lined with brambles, heavy with thousands of wild berries. You take a few minutes to gather a handful of perfectly ripe blackberries into your handkerchief. You or an ally recovers one health."
          , health 1
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( Residential
          , "Two young parents are selling many of their possessions to make their move up to Maine a little easier. You may buy one common item from the display. The man reveals that he's taken a public works job in a small town, where they're hoping to find a better life for their new daughter."
          , buyOne "Common"
          )
        ,
          ( Bridge
          , "The wind whips across the bridge, threatening to sweep you off your feet. Concluding that it would be dangerous to stay here in the open any longer than necessary, you hustle to the other side, out of the gale. You may move one space."
          , MoveUpTo 1
          )
        ,
          ( Scenic
          , "The battered old produce truck is stuck in the mud on a winding side road, while the driver tries in vain to push it free. \"I thought this would be a shortcut, and now I'm going to be late!\" You roll up your sleeves and offer to help (strength). If you pass, the truck finally rolls free, and the driver rewards you for your efforts; gain $2."
          , pass Strength 0 (money 2)
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( Residential
          , "You hear a gentle bell ringing from around the corner, and advance to find a white truck selling ice cream cones from the open side door. The man in the white paper hat waves when he sees you, a broad smile on his face. You may spend $1 for you or an ally to recover two health."
          , mayPay (SpendMoney 1) (health 2)
          )
        ,
          ( Bridge
          , "From the top of the bridge, you watch a few young children splash in the water below. At first, you carefully watch for shadows in the water around them, but your tension gradually melts away as you watch the innocent play below you and listen to the sound of their happy laughter. You or an ally recovers one sanity."
          , sanity 1
          )
        ,
          ( Scenic
          , "The old woman waves you off when you try to help her get her truck free from the mud. Not wanting to appear ungrateful, she explains that she's carrying her late brother's belongings into town to sell, but she's not expecting much for his junk (influence). If you pass, you spot something you like and she's eager to be rid of it; gain one common item."
          , pass Influence 0 commonItem
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( Residential
          , "You may spend one remnant to show the man with the thick Russian accent what you're dealing with. If you do, he shares an old folktale he learned as a child, and the grim story reminds you that people have always persevered in the face of hardship; you or an ally recovers one sanity."
          , mayPay (SpendRemnants 1) (sanity 1)
          )
        ,
          ( Bridge
          , "A young man leans against the railing of the bridge, greeting passersby. When he sees your eyes, he pauses briefly and professes the gift of second sight. You may spend $1 for a prophetic reading. If you do, a brief glimpse of the future prepares you for the trials ahead; you may focus one skill of your choice, even if it exceeds your focus limit."
          , mayPay (SpendMoney 1) focusExceed
          )
        ,
          ( Scenic
          , "You find a crate of fireworks abandoned along the side of the road. If you can find something in this damp box that hasn't been ruined by the recent rain, it could prove a useful distraction. You may become delayed to exhaust one monster in any space. (It disengages any investigators when it exhausts.)"
          , mayPay CostDelayed (Custom "don-fireworks")
          )
        ]
      )
    ]
