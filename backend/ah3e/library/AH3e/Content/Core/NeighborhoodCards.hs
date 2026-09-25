module AH3e.Content.Core.NeighborhoodCards (cards) where

import AH3e.Content.Tiles
import AH3e.Content.Vocabulary
import AH3e.Prelude
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

card :: NeighborhoodId -> Int -> [(Text, Text, Effect)] -> CardDef
card nid n encounters =
  CardDef
    { code = CardCode (coerce nid <> "-" <> T.justifyRight 2 '0' (tshow n))
    , name = (tile nid).name <> " " <> tshow n <> "/8"
    , expansion = CoreSet
    , copies = 1
    , kind =
        NeighborhoodCard
          nid
          (Map.fromList [(spaceIdFor place, Encounter txt eff) | (place, txt, eff) <- encounters])
    }

downtown :: [CardDef]
downtown =
  map
    (uncurry (card "downtown"))
    [
      ( 1
      ,
        [
          ( "Arkham Asylum"
          , "You check yourself into the asylum. You recover two sanity. You befriend one of the other patients, who mumbles about cursed oaths and powers beyond the mortal. You decide to sneak him out (observation). If you pass, DANIEL CHESTERFIELD joins you. If you fail, you are caught and forced to leave."
          , Seq [mySanity 2, pass Observation 0 (named "DANIEL CHESTERFIELD")]
          )
        ,
          ( "Independence Square"
          , "\"You must allow me to read your future!\" insists the fortune teller Anna Kaslow. She flips several tarot cards as you sit. \"The darkness that falls over Arkham is watching you closely. Be ready to act. Seize any opportunity.\" She gives you a card to keep as a talisman. You gain the ACE OF RODS."
          , named "ACE OF RODS"
          )
        ,
          ( "La Bella Luna"
          , "The Clover Club beneath La Bella Luna is the best place to gamble away your hard-earned cash. \"Pass-line bet for our friend here,\" says the croupier. \"Roll 'em!\" Roll two dice. On a 7 or 11, you double your bet; you gain $5. On a 2, 3, or 12, you lose the bet and tip out the croupier; you discard $3."
          , Custom "clover-club-craps"
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Arkham Asylum"
          , "The formidable Nurse Heather is not convinced that you require psychiatric treatment. You may spend $1 to help her past her misgivings. If you do, she not only checks you in for treatment but offers one of her vigorous, nearly painful massages; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ,
          ( "Independence Square"
          , "You follow the sounds of music and children laughing to the bandstand, but no one is there (will). If you pass, you feel as though you were meant to find this; you gain one curio item. If you fail, you find only a small rusted toy cornet of the type you played with as a child; you suffer one horror."
          , Test Will 0 curioItem (horror 1)
          )
        ,
          ( "La Bella Luna"
          , "You don't know what you did to insult the guy, but the next thing you know you're getting a wine bottle cracked across your phiz. \"Hey!\" someone shouts. \"How about you pick on someone your own size!\" Soon you both land face down in the street. You suffer two damage and gain one ally."
          , Seq [damage 2, ally]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Arkham Asylum"
          , "\"Sure, you could waste your time talking to the doctors every week,\" suggests the orderly. \"Or maybe just take a snootful of this.\" After checking that no doctors are around to observe, he holds out a small square of folded paper. You may spend $1 for you or an ally to recover three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ,
          ( "Independence Square"
          , "A flea market has sprung up, filling the square with happy voices and colorful wares. You browse the stalls in something of a daze, hoping to take your mind off the terrible things you have seen. You may buy any number of common items from the display."
          , buyAny "Common"
          )
        ,
          ( "La Bella Luna"
          , "The goon doesn't want to let you into the Clover Club beneath the restaurant. You tell him you're here to see Naomi O'Bannion (influence). If you pass, a few games at the card tables let you turn a profit; you gain $3. If you fail, Naomi must not be accepting visitors right now; you suffer one damage."
          , Test Influence 0 (money 3) (damage 1)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Arkham Asylum"
          , "\"You must be one of the candidates for the trial,\" says Nurse Heather. You may spend $1 to pay the fee. If you do, Doctor Mintz appears at your side and injects you with something before you can object; you focus two skills of your choice, even if it exceeds your focus limit."
          , mayPay (SpendMoney 1) (Seq [focusExceed, focusExceed])
          )
        ,
          ( "Independence Square"
          , "A rag-and-bone man pushes his cart across the square (will). If you pass, you chat for a moment and he gives you a token of appreciation; you gain one common item. If you fail, when you turn to speak with him there is nothing but a pile of stinking rags and rotting driftwood; you suffer one horror."
          , Test Will 0 commonItem (horror 1)
          )
        ,
          ( "La Bella Luna"
          , "Peter Clover, the proprietor of the Clover Club beneath the restaurant, lays a meaty hand on your shoulder. \"They say you're pretty good with the numbers\" (observation). If you pass, you're paid well for cooking the books; you gain $3. If you fail, Peter isn't pleased; you suffer two damage."
          , Test Observation 0 (money 3) (damage 2)
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Arkham Asylum"
          , "You speak briefly with Charles Badoe, the chief psychiatrist. You or an ally recovers two sanity. Either test (influence) to convince him to devote more time or spend $1 to pay his fee. If you pass or spend the money, Charles devotes his full attention to your concerns; you or an ally recovers two sanity."
          , Seq [sanity 2, orPay Influence "Spend $1" (SpendMoney 1) (sanity 2)]
          )
        ,
          ( "Independence Square"
          , "A band of Romani nomads have settled in the square for the moment. Some sing and dance, while others offer jewelry and other trinkets for sale. You may buy one curio item from the display for half price (rounded up)."
          , buyOneHalf "Curio"
          )
        ,
          ( "La Bella Luna"
          , "A panicked stranger forces a wad of cash into your hand. You gain $3. This benefactor tells you, \"You gotta get me outta here,\" and points to the club's imposing bouncer (observation). If you pass, you stealthily escort the stranger to safety. If you fail, you both get caught; you suffer two damage."
          , Seq [money 3, Test Observation 0 NoEffect (damage 2)]
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Arkham Asylum"
          , "An epidemic of trauma and madness has swept the city, and the asylum is overwhelmed by desperate souls seeking help. You may spend $1 to bribe a staff member. If you do, you are given priority and prescribed a highly effective medication; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ,
          ( "Independence Square"
          , "Anna Kaslow, the old soothsayer, is offering tarot readings. You may spend $1 to have her tell your fortune. If you do, the cards reveal that many challenges await you in the future, but you will triumph if you believe in yourself and trust your intuition; you gain THE MOON."
          , mayPay (SpendMoney 1) (named "THE MOON")
          )
        ,
          ( "La Bella Luna"
          , "A desperate gambler pleads for a line of credit. You offer to vouch for the stranger (influence). If you pass, the grateful stranger shares his winnings and you feel inspired; you gain $3 and become BLESSED. If you fail, the pit boss holds you responsible for the gambler's losses; you discard $2."
          , Test Influence 0 (Seq [money 3, blessed]) (LoseMoney (N 2))
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Arkham Asylum"
          , "Charles Badoe, the chief psychiatrist, is running a trial of a new experimental \"dream enhancer.\" You may spend $2 to take part in the trial. If you do, you dream of a beautiful city where humans and cats live in harmony and every day is peaceful; you recover three sanity and become BLESSED."
          , mayPay (SpendMoney 2) (Seq [mySanity 3, blessed])
          )
        ,
          ( "Independence Square"
          , "A poor Dunwich family has parked their truck by the square and are selling old furniture and knickknacks out of the back. Most of what they have for sale is worn out and worthless, but a few objects look worthwhile. You may buy one common item from the display for half price (rounded up)."
          , buyOneHalf "Common"
          )
        ,
          ( "La Bella Luna"
          , "You join a hand of blackjack at the Clover Club beneath La Bella Luna. No one's noticed yet that you've been counting cards (influence). If you pass, your deception pays off; you gain $3. If you fail, the pit boss catches on and has you taken out back for a lesson; you suffer two damage."
          , Test Influence 0 (money 3) (damage 2)
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Arkham Asylum"
          , "While in the waiting room, you doze off and get some badly needed sleep. You or an ally recovers two sanity. You feel refreshed when you are awakened by Nurse Heather calling your name. She says the doctor is ready to see you. You may spend $1 for you or an ally to recover two sanity."
          , Seq [sanity 2, mayPay (SpendMoney 1) (sanity 2)]
          )
        ,
          ( "Independence Square"
          , "You sit with the soothsayer Anna Kaslow, and she teaches you how to cast runes to predict your future. You hold a question in your mind as you select runes from a bag (will). If you pass, you see your path to success; you become BLESSED. If you fail, the runes promise destruction; you suffer one horror."
          , Test Will 0 blessed (horror 1)
          )
        ,
          ( "La Bella Luna"
          , "In the VIP area of the Clover Club beneath the restaurant, Peter Clover winks and pins a four-leaf clover to your coat. You become a CLOVER CLUB MEMBER. \"Do me a favor in return someday.\" You offer to pull some strings on his behalf (influence). If you pass, he pays you in advance; you gain $3."
          , Seq [named "CLOVER CLUB MEMBER", pass Influence 0 (money 3)]
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
          , "You find yourself sitting with Old Man Hibbard at a corner table, watching the crowd ebb and flow on the dance floor in the old barn. He keeps pouring whiskey as long as you keep listening to him reminisce about the war in the Philippines. You or an ally recovers two sanity."
          , sanity 2
          )
        ,
          ( "Police Station"
          , "Sheriff Engle doesn't seem to believe your stories about horrible monsters. Either test (influence) to plead your case or spend one remnant to show him some proof. If you pass or spend the remnant, the sheriff gives you a gun and a badge; you become the DEPUTY OF ARKHAM and gain a SERVICE PIECE."
          , orPay
              Influence
              "Spend one remnant"
              (SpendRemnants 1)
              (Seq [named "DEPUTY OF ARKHAM", named "SERVICE PIECE"])
          )
        ,
          ( "Velma's Diner"
          , "Eggs over easy, hash browns, and Velma's excellent coffee. You or an ally recovers two health. \"Mind if I join you?\" The man introduces himself as Ryan Dean, a traveling salesman. \"Have I got a deal for you!\" You may buy one common item from the display for half price (rounded up)."
          , Seq [health 2, buyOneHalf "Common"]
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Hibb's Roadhouse"
          , "Joey \"the Rat\" Vigil whistles to catch your attention. \"I've just, uh, come into possession of some merchandise. You interested?\" You may buy one common item from the display for half price (rounded up)."
          , buyOneHalf "Common"
          )
        ,
          ( "Police Station"
          , "Deputy Dingby says you forgot something last time you were here, but you don't recall doing so. Still, the deputy insists, so you pick through the lost and found anyway. You gain one common item with value three or less."
          , GainE (AnItemValued (Just "Common") (AtMost 3))
          )
        ,
          ( "Velma's Diner"
          , "Velma makes you the best steak and potatoes you've ever tasted. You or an ally recovers two health. \"One of my servers quit on me,\" says Velma. \"Could you fill in for me\" (influence)? If you pass, you become a SERVER AT VELMA'S. If you fail, you not-so-politely decline the offer."
          , Seq [health 2, pass Influence 0 (named "SERVER AT VELMA'S")]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Hibb's Roadhouse"
          , "As you're about to enter the roadhouse, you hear the whining of a hungry dog. You may spend $1 to get something for the mutt to eat and drink. If you do, the canine takes a liking to you and follows you as you leave; you or an ally recovers two sanity and a STRAY DOG joins you."
          , mayPay (SpendMoney 1) (Seq [sanity 2, named "STRAY DOG"])
          )
        ,
          ( "Police Station"
          , "\"Tell me again what happened,\" says the desk sergeant. Either test (observation) or spend one clue to recount the incident in vivid detail. If you pass or spend the clue, he looks the other way as you pick through the evidence locker; you gain one common item with value four or greater."
          , orPay Observation "Spend one clue" (SpendClues 1) (GainE (AnItemValued (Just "Common") (AtLeast 4)))
          )
        ,
          ( "Velma's Diner"
          , "Abner Weems, the local drunk, leans against the wall as he starts telling you about the horrid fish-frog monster he saw at the docks until Velma shoos him away. \"Git, you,\" she says, before turning to you with a sigh. \"What'll it be?\" You may spend $1 for you or an ally to recover three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Hibb's Roadhouse"
          , "People don't usually come here to read, but customers are welcome to read by the fire. You may spend $1 to get something to eat and drink. If you do, the simple human goodness of Great Expectations briefly restores your faith in humanity; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ,
          ( "Police Station"
          , "A child tugs on your sleeve and asks you for help. \"None of the officers will help me find Mrs. Flopsy\" (observation). If you pass, you find the missing rabbit and are the hero of the local children; you become BLESSED. If you fail, you search until it grows dark; you become delayed."
          , Test Observation 0 blessed delayed
          )
        ,
          ( "Velma's Diner"
          , "\"It's some fancy vitamin drink,\" says Velma dubiously. \"My supplier told me to try some, but I dunno. What do you think?\" You may spend $1 to order some with your meal. If you do, you feel a new energy coursing through your veins; you or an ally recovers three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Hibb's Roadhouse"
          , "Everyone is wielding pool cues and broken bottles. Either test (strength) to join the brawl or spend $1 to order refreshments. If you pass or spend the money, it's all a diverting amusement; you or an ally recovers three sanity. If you fail, you spend the night icing your bruised face; you suffer one damage."
          , Choose
              [ ("Test strength", Test Strength 0 (sanity 3) (damage 1))
              , ("Spend $1", Pay (SpendMoney 1) (sanity 3))
              ]
          )
        ,
          ( "Police Station"
          , "An officer questions your involvement in the recent incident. You insist that he call his boss and mention your name (influence). If you pass, the officer sheepishly hangs up the phone and politely asks you how he can help; you gain one common item. If you fail, he tells you the sheriff has never heard of you."
          , pass Influence 0 commonItem
          )
        ,
          ( "Velma's Diner"
          , "The patron in the next booth is drawing bizarre and disturbing icons in the margins of a newspaper. Either test (observation) to covertly analyze the doodles or spend $1 to purchase the newspaper. If you pass or spend the money, the arcane images fortify your spirit against dark forces; you become BLESSED."
          , orPay Observation "Spend $1" (SpendMoney 1) blessed
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Hibb's Roadhouse"
          , "The singer's tune comforts your troubled mind. You or an ally recovers two sanity. Her voice seems somehow hypnotic. You may spend $1 to stay a while longer. If you do, the melody fixes itself in your memory, bringing you good luck when you whistle the tune; you become BLESSED."
          , Seq [sanity 2, mayPay (SpendMoney 1) blessed]
          )
        ,
          ( "Police Station"
          , "As you're about to leave the station, Deputy Dingby stops you and accuses you of littering. Despite your protest that you didn't drop anything, he forces something you've never seen before into your hands. \"I seen you!\" he says. \"I seen you do it.\" You gain one common item with value three or less."
          , GainE (AnItemValued (Just "Common") (AtMost 3))
          )
        ,
          ( "Velma's Diner"
          , "Velma serves up a Reuben that hits the spot. You or an ally recovers two health. A nurse is enjoying a slice of cherry pie. You may spend $1 to order a slice. If you do, you join her at the counter and she offers some first aid for your wounds; you or an ally recovers two health."
          , Seq [health 2, mayPay (SpendMoney 1) (health 2)]
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Hibb's Roadhouse"
          , "You ask the gathered patrons if any of them have noticed the strange forces that have infected Arkham, but they all laugh and shake their heads. \"You've had too much to drink,\" they tell you. Clearly you will find no allies here tonight. You may spend $1 for you or an ally to recover three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ,
          ( "Police Station"
          , "Trembling and bloody, an officer asks you if he has gone mad or if what he saw was real (influence). If you pass, he hands you the key to the armory and tells you to get ready for the fight; you gain one common item with value four or greater."
          , pass Influence 0 (GainE (AnItemValued (Just "Common") (AtLeast 4)))
          )
        ,
          ( "Velma's Diner"
          , "You order a house specialty but ask for several alterations to the dish, requesting that particular spices be added and certain other ingredients be served on the side. The cook isn't pleased, and Velma charges extra for all your fussing. You may spend $1 for you or an ally to recover three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Hibb's Roadhouse"
          , "A young couple buys you a drink to celebrate their recent engagement. You or an ally recovers two sanity. It is a welcome gift, but you can see they are clearly struggling for money. You may spend $1 to provide them with some cash as an engagement gift. If you do, you or an ally recovers two sanity."
          , Seq [sanity 2, mayPay (SpendMoney 1) (sanity 2)]
          )
        ,
          ( "Police Station"
          , "A lunatic in handcuffs loudly boasts of future crimes. Spawn one clue. You relate what you know about the criminal (observation). If you pass, the detective nods and tells an officer to \"find something to keep this citizen safe;\" you gain one common item. If you fail, they hold you as well; you become delayed."
          , Seq [SpawnOneClue, Test Observation 0 commonItem delayed]
          )
        ,
          ( "Velma's Diner"
          , "\"Sir,\" Velma insists as you stumble in through the door. She helps you to a table and fills a coffee cup for you. \"You need a bowl of my stew. That'll fix you right up,\" she says. \"Don't worry, I'll give you a discount. I like your face.\" You may spend $1 for you or an ally to recover three health."
          , mayPay (SpendMoney 1) (health 3)
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
          , "You've heard that if you make an offering during the full moon, a water spirit will favor you. You may spend $3 or one remnant to drop your offering into the reflection of the moon. If you do, a pair of large glistening eyes breach the surface of the water and gaze at you with approval; you become BLESSED."
          , Choose
              [ ("Spend $3", Pay (SpendMoney 3) blessed)
              , ("Spend one remnant", Pay (SpendRemnants 1) blessed)
              , ("Decline", NoEffect)
              ]
          )
        ,
          ( "Tick-Tock Club"
          , "All the clocks in here are set to different times. It's soothing, like time itself has ceased to exist. You kick back your heels and try your hardest to relax. You may spend $1 to get something to eat and drink. If you do, the drink and jazz ease your worries; you or an ally recovers two health and two sanity."
          , mayPay (SpendMoney 1) (RecoverBoth YouOrAlly (N 2) (N 2))
          )
        ,
          ( "Unvisited Isle"
          , "Your flashlight picks out faded carvings on one of the tall standing stones (lore). If you pass, you sense residual arcane energy from some foul ritual; you gain one remnant. If you fail, something small but quick leaps at your face before scuttling off into the darkness; you suffer one horror."
          , Test Lore 0 (remnants 1) (horror 1)
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "River Docks"
          , "You bump into Abner Weems, the local drunk. He cries and tells you how his wife and daughter were taken away by a horrible bat-like creature with three burning eyes. You may become delayed to console him. If you do, he gives you something before he passes out; you gain one common item."
          , mayPay CostDelayed commonItem
          )
        ,
          ( "Tick-Tock Club"
          , "You trade bar tricks with another patron and draw up a crowd. \"See if you can figure out this next one,\" he says (observation). If you pass, he pays for your meal; you or an ally recovers three health. If you fail, you have to buy your own meal; you may spend $1 for you or an ally to recover three health."
          , Test Observation 0 (health 3) (mayPay (SpendMoney 1) (health 3))
          )
        ,
          ( "Unvisited Isle"
          , "In the middle of the oppressive dark woods you come across a clearing and a circle of slim white birch trees. You rest here briefly (will). If you pass, you are filled with an inexplicable sense of peace; you become BLESSED. If you fail, the tree branches twist and reach out; you suffer one horror."
          , Test Will 0 blessed (horror 1)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "River Docks"
          , "You find Joey \"the Rat\" right where you expected he'd be. You show him what you've found, and a grin crosses his lips. \"I know a rich collector in Marseilles who'd pay good money for some of those,\" he says. You may sell any number of remnants for $2 each."
          , RepeatWhilePaying (SpendRemnants 1) (money 2)
          )
        ,
          ( "Tick-Tock Club"
          , "Slim, well-dressed Dainty Donohue takes a personal, and somewhat unsettling, interest in you. \"Make sure my friend only gets the good stuff, Pat,\" he says to the bartender. You may spend $1 for you or an ally to recover two health and two sanity."
          , mayPay (SpendMoney 1) (RecoverBoth YouOrAlly (N 2) (N 2))
          )
        ,
          ( "Unvisited Isle"
          , "Robed figures stand in a circle surrounding an open fire. They all turn to face you and call out with one voice, \"We will be free soon.\" You may gain a DARK PACT condition to join their chant. If you do, they share their gifts; you gain one curio item and focus two skills of your choice, even if it exceeds your focus limit."
          , mayPay (CostCondition "DARK PACT") (Seq [curioItem, focusExceed, focusExceed])
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "River Docks"
          , "The work gang is happy to accept your help loading heavy crates onto the riverboat (strength). You gain $1 for each success you roll. When the job is done, the foreman walks over and jams his thumbs into the braces of his overalls. \"We always need more help,\" he says. You become a STEVEDORE."
          , Seq [Test Strength 0 (GainE (Money TestResult)) NoEffect, named "STEVEDORE"]
          )
        ,
          ( "Tick-Tock Club"
          , "\"You told me to tell you when you'd had enough,\" says the barkeep. You or an ally recovers two sanity. \"But it's no business of mine if you want to keep going.\" You may spend $1 to keep drinking. If you do, the rest of the night is a little hazy; you recover two sanity and discard one focus."
          , Seq [sanity 2, mayPay (SpendMoney 1) (Seq [mySanity 2, DiscardAFocus])]
          )
        ,
          ( "Unvisited Isle"
          , "\"Give me something good,\" cackles the old woman. \"Something for my cooking pot!\" Either test (lore) to teach her a recipe using the island's vegetation or spend one remnant to provide the ingredients. If you pass or spend the remnant, she grubs within her apron for your payment; you gain one curio item."
          , orPay Lore "Spend one remnant" (SpendRemnants 1) curioItem
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "River Docks"
          , "A group of thugs is taking out their frustrations on Abner Weems, the local drunk. Either test (strength) to force them to back off or spend one remnant to intimidate them. If you pass or spend the remnant, Abner is grateful for your intervention; you gain one common item. If you fail, you suffer two damage."
          , Choose
              [ ("Test strength", Test Strength 0 commonItem (damage 2))
              , ("Spend one remnant", Pay (SpendRemnants 1) commonItem)
              ]
          )
        ,
          ( "Tick-Tock Club"
          , "The food is superb and the décor is beautiful. You or an ally recovers one health and one sanity. Small automatons adorn the club. You may spend $2 to help repair a brass figure. If you do, the automaton springs to life and writes, \"We shall watch over you,\" on a piece of parchment; you become BLESSED."
          , Seq [RecoverBoth YouOrAlly (N 1) (N 1), mayPay (SpendMoney 2) blessed]
          )
        ,
          ( "Unvisited Isle"
          , "A bit of digging reveals a mass grave (will). If you pass, you search through the remains and discover several inhuman creatures; you gain one remnant. If you fail, you cannot stand the thought of all these corpses; you suffer two horror."
          , Test Will 0 (remnants 1) (horror 2)
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "River Docks"
          , "\"You need work?\" shouts the foreman. \"I pay good money for strong backs\" (strength)! You gain $1 for each success you roll. At the end of the day, one crate is unaccounted for on the manifest. You and the other workers quietly divvy up its contents among yourselves. You gain one common item."
          , Seq [Test Strength 0 (GainE (Money TestResult)) NoEffect, commonItem]
          )
        ,
          ( "Tick-Tock Club"
          , "The next act didn't show and you're pressed into performing on stage (influence). If you pass, you're a hit and Dainty Donohue tells you to come back any time; you gain $2 and become a PERFORMER. If you fail, you get paid about what you'd expect for your paltry performance; you gain $1."
          , Test Influence 0 (Seq [money 2, named "PERFORMER"]) (money 1)
          )
        ,
          ( "Unvisited Isle"
          , "Cultists left behind ritual components among the rune-covered stones. You gain one remnant. You try to interpret the runes (lore). If you pass, you unlock the secret of a ritual to enhance yourself; you focus two skills of your choice. If you fail, the runes are a devious magical trap; you suffer two horror."
          , Seq [remnants 1, Test Lore 0 (Seq [focusAny, focusAny]) (horror 2)]
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "River Docks"
          , "The stranger waits for you at the appointed hour. The amber lights of the pier reveal his disfigured features. \"The thing that did this to me,\" he says, \"I hear you might have killed it.\" You may spend one remnant to prove to him that he has been avenged. If you do, he pays you what he can; you gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ,
          ( "Tick-Tock Club"
          , "The bar is populated with a wide array of charming, insightful individuals. You are certain that sitting down next to any one of them and buying that person a drink will produce an evening's worth of wit and conversation. You may spend $1 to recover three sanity and focus one skill of your choice."
          , mayPay (SpendMoney 1) (Seq [mySanity 3, focusAny])
          )
        ,
          ( "Unvisited Isle"
          , "A thick fog rolls in over the island. You fear you might never escape it (will). If you pass, you retrace your steps and discover a gift has been left in your boat; you gain one curio item. If you fail, you sleep in the cold, damp mist until morning; you suffer one horror and become delayed."
          , Test Will 0 curioItem (Seq [horror 1, delayed])
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "River Docks"
          , "You find Joey \"the Rat\" skulking along in the shadow of a nearby warehouse. \"Always looking for something interesting,\" he says. \"Hey, I found something you might like, maybe we can make a deal?\" You may spend $3 or one remnant to gain one common item."
          , Choose
              [ ("Spend $3", Pay (SpendMoney 3) commonItem)
              , ("Spend one remnant", Pay (SpendRemnants 1) commonItem)
              , ("Decline", NoEffect)
              ]
          )
        ,
          ( "Tick-Tock Club"
          , "The food is to die for. You or an ally recovers three health. \"Only the talent eats free. Pay up or show us what you've got.\" Either test (influence) to climb the stage or spend $1 to pay your tab. If you pass, Dainty Donohue comps your meal. If you fail or can't pay, you're thrown out; you suffer two damage."
          , Seq
              [ health 3
              , Choose
                  [ ("Test influence", Test Influence 0 NoEffect (damage 2))
                  , ("Spend $1", Pay (SpendMoney 1) NoEffect)
                  ]
              ]
          )
        ,
          ( "Unvisited Isle"
          , "Something has been left on a stump in the center of a clearing. You gain one curio item. The ground around you is marked like a labyrinth (will). If you pass, you resist the magic that binds you here. If you fail, no matter which way you walk you find yourself back at the stump; you become delayed."
          , Seq [curioItem, Test Will 0 NoEffect delayed]
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
          , "A group of students finds you, waving a set of photographs from the telescope. \"You know weird stuff,\" they say, \"what do you think\" (lore)? If you pass, you recognize the bat-wing shape of the Fungi from Yuggoth; you gain one remnant. If you fail, you conclude it's a smudge on the lens."
          , pass Lore 0 (remnants 1)
          )
        ,
          ( "Orne Library"
          , "Abigail Foreman, a young librarian, shows you to the section on ancient folklore that you were looking for (lore). If you pass, you learn some of the rituals of Voodoo practitioners in French Dahomey; you gain one spell. If you fail, you can't sort the witchcraft and chicanery from the real thing."
          , pass Lore 0 spell
          )
        ,
          ( "Science Building"
          , "\"Good, good,\" says the professor. \"Just lie there under the resonator and close your eyes. We'll put this into the imager. Don't worry, you'll be well compensated.\" When you wake up, you're a little richer and bursting with energy. You gain $2 and focus every skill, even if it exceeds your focus limit."
          , Seq (money 2 : [Focus (Just s) True | s <- allSkills])
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Observatory"
          , "There's Venus, right where she should be. And a few degrees further... (observation). If you pass, you carefully note down the predicted syzygy; you remove one doom from any space. If you fail, the calculations don't add up and you grow frustrated trying again and again; you become delayed."
          , Test Observation 0 (RemoveDoomFrom AnySpace (N 1)) delayed
          )
        ,
          ( "Orne Library"
          , "De Quincy, Einstein, Alhazred. You scribble page after page of notes detailing the unexpected connections (will). If you pass, Henry Armitage recognizes your dedication and respect for the library and grants you access to the library's rarest tomes; you gain one spell and RARE BOOKS ACCESS."
          , pass Will 0 (Seq [spell, named "RARE BOOKS ACCESS"])
          )
        ,
          ( "Science Building"
          , "\"Hydrotherapy! Psychotherapy! Magnetic therapy! We have ways to unlock all human potential!\" You look doubtfully at the wild-haired scientist. You may spend one remnant to provide the catalyst for his experiments. If you do, the treatments make you feel more alive than ever; you become BLESSED."
          , mayPay (SpendRemnants 1) blessed
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Observatory"
          , "There's a gap in the schedule, so you are able to spend some time at the telescope (observation). If you pass, you see something streak out of the sky and note where it lands; you gain one remnant. If you fail, you enjoy the comforting twinkle of the slumbering stars; you or an ally recovers one sanity."
          , Test Observation 0 (remnants 1) (sanity 1)
          )
        ,
          ( "Orne Library"
          , "You come upon an old book bound with black leather and filled with names written in blood. You may gain a DARK PACT condition to sign your name. If you do, the book's pages ripple as it flips itself back to the first page, now covered in arcane text; you gain three spells, two remnants, and one tome item."
          , mayPay (CostCondition "DARK PACT") (Seq [spell, spell, spell, remnants 2, tomeItem])
          )
        ,
          ( "Science Building"
          , "You agree to become a test subject for a new drug. You gain $2. \"It's a sort of biological accelerant,\" says the student. \"Let's up the dosage and see what happens\" (strength). If you pass, the drug has no effect. If you fail, your heart palpitates and you begin to foam at the mouth; you suffer two damage."
          , Seq [money 2, Test Strength 0 NoEffect (damage 2)]
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Observatory"
          , "You manipulate the lens to find the Hyades through the telescope while humming a song from a play you once saw (lore). If you pass, the memory of The Mikado cheers you up immensely; you become BLESSED. If you fail, you remember Cassilda's song and the masked man; you suffer one horror."
          , Test Lore 0 blessed (horror 1)
          )
        ,
          ( "Orne Library"
          , "It feels like you've been studying for days. You know you're getting somewhere, but you're just so tired (will). If you pass, you shake off the fatigue and keep at it; you gain one spell. If you fail, you fall asleep on the open book and suffer from nightmares; you suffer one horror."
          , Test Will 0 spell (horror 1)
          )
        ,
          ( "Science Building"
          , "The conference drags on and on contentiously. The young professor from Harvard slaps the table and stands. \"But what proof do you have? Show us now!\" You may spend one clue or one remnant to support your case. If you do, you take some of the newly allocated funding for yourself; you gain $3."
          , Choose
              [ ("Spend one clue", Pay (SpendClues 1) (money 3))
              , ("Spend one remnant", Pay (SpendRemnants 1) (money 3))
              , ("Decline", NoEffect)
              ]
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Observatory"
          , "As you gaze into the night sky, you hear a voice whispering to you through the starry void. You may gain a DARK PACT condition to save the world. If you do, your mind goes black and you wake up holding a twisted piece of black metal; you remove three doom from any one space and gain one remnant."
          , mayPay (CostCondition "DARK PACT") (Seq [RemoveDoomFrom AnySpace (N 3), remnants 1])
          )
        ,
          ( "Orne Library"
          , "You read through the journal of an occult scholar. Much of the writing is obsessive minutiae, and you struggle to absorb the material (lore). If you pass, you understand the deeper meaning; you gain one spell. If you fail, you begin to display obsessive behaviors of your own; you suffer one horror."
          , Test Lore 0 spell (horror 1)
          )
        ,
          ( "Science Building"
          , "\"It is my understanding,\" the dean says, \"that you have had a few experiences on the fringes of known science. If this is true—if you have proof—we would like you to offer a guest lecture for our students. The stipend is more than reasonable.\" You may spend one remnant to gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Observatory"
          , "Strange radio signals have been recorded from a comet that is heading toward Earth. You believe a message is embedded in the broadcast (lore). If you pass, you understand the warning in the signal; you remove one doom from any space. If you fail, you hear the signal in your head; you suffer one horror."
          , Test Lore 0 (RemoveDoomFrom AnySpace (N 1)) (horror 1)
          )
        ,
          ( "Orne Library"
          , "A sweet old woman tells you she's never been in a library before. She's looking for books about legends and folklore. You may become delayed to patiently help her find the material she's seeking. If you do, she thanks you with a small prayer in a language you do not recognize; you become BLESSED."
          , mayPay CostDelayed blessed
          )
        ,
          ( "Science Building"
          , "You notice a group of well-dressed donors touring the campus. As they examine the laboratories, you eavesdrop on their ongoing conversation. You discover that they will pay handsomely for cutting-edge scientific breakthroughs. You may spend one remnant to gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Observatory"
          , "\"Jazz\" Mulligan, the head janitor, is cleaning up the observatory after hours (observation). If you pass, you stop him before he throws away a stack of papers detailing an astronomical breakthrough; you gain one remnant. If you fail, he dances away, softly playing his harmonica."
          , pass Observation 0 (remnants 1)
          )
        ,
          ( "Orne Library"
          , "You tuck yourself into the back of the room as Henry Armitage, the head librarian, gives a far-ranging lecture on the occult (lore). If you pass, you recognize that some of what he's saying illuminates your own knowledge; you gain one spell. If you fail, the lecture is more entertaining than informative."
          , pass Lore 0 spell
          )
        ,
          ( "Science Building"
          , "Dr. Graves is happy to pay you for your answers to his questions. You gain $2. As the professor explains the more exotic aspects of his work, you attempt to stay calm (will). If you pass, he agrees to work with you; EZRA GRAVES joins you. If you fail, his methods are gut-churning; you suffer one horror."
          , Seq [money 2, Test Will 0 (named "EZRA GRAVES") (horror 1)]
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Observatory"
          , "Professor Tremaine is raving in glowing terms about the wonders of the cosmos and the role of astronomy in discovering them. You make eye contact and the professor pounces, taking you for a prospective student and pressing some equipment into your hands. You gain an ASTROLABE."
          , named "ASTROLABE"
          )
        ,
          ( "Orne Library"
          , "Someone left behind a note in the restricted section. The note outlines an arcane ritual. You gain one spell. As you memorize the words, a terrible screeching startles you (will). If you pass, it's just janitor \"Jazz\" Mulligan's harmonica. If you fail, you flee the building in a panic; you suffer two horror."
          , Seq [spell, Test Will 0 NoEffect (horror 2)]
          )
        ,
          ( "Science Building"
          , "Given your unique experiences, a research team offers to pay to run a few tests. You may spend one remnant to supply them with research material. If you do, they excitedly pay you for your contribution and rush off to perform experiments; you gain $3. The results are inconclusive."
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
          , "Minnie Klein, a spirited reporter, leans closer, conspiratorially. \"Got a hot tip for me? Come on, friend, let it slip!\" You may spend one clue or one remnant to tell her about your findings. If you do, she asks you many clarifying questions and makes sure you're willing to bring her more; you gain $3."
          , Choose
              [ ("Spend one clue", Pay (SpendClues 1) (money 3))
              , ("Spend one remnant", Pay (SpendRemnants 1) (money 3))
              , ("Decline", NoEffect)
              ]
          )
        ,
          ( "Curiositie Shoppe"
          , "On a shelf behind a stack of dusty books, you find an intricate wooden puppet. As you reach out to take a closer look, the thing comes alive! Either test (lore) to assert control over the creature or spend $3 to purchase it. If you pass or spend the money, you gain the WOODEN HOMUNCULUS."
          , orPay Lore "Spend $3" (SpendMoney 3) (named "WOODEN HOMUNCULUS")
          )
        ,
          ( "Train Station"
          , "Bill Washington finishes loading his truck and pulls away, but some luggage falls off the back. You move to claim it for yourself (influence). If you pass, your act is convincing and no one questions who it belongs to; you gain the ABANDONED LUGGAGE. If you fail, someone is watching and you decide to leave it be."
          , pass Influence 0 (named "ABANDONED LUGGAGE")
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Arkham Advertiser"
          , "Editor Doyle Jefferies doesn't like the story about a strange creature that you've submitted. Either test (observation) to recall more explicit details or spend one remnant to show him the proof. If you pass or spend the remnant, Doyle approves the story and pays you for your work; you gain $3."
          , orPay Observation "Spend one remnant" (SpendRemnants 1) (money 3)
          )
        ,
          ( "Curiositie Shoppe"
          , "\"Terracotta busts from Babylon,\" whispers Oliver Thomas, the owner. \"Incense from Nineveh. Carved soapstone creatures fished from the waters of Malta. Tell me what you're looking for, and I'll take a look in the back.\" You may buy any number of curio items from the display."
          , buyAny "Curio"
          )
        ,
          ( "Train Station"
          , "An unseen assailant shoves into your back, propelling you onto the tracks (will). If you pass, you calmly grab a stranger's outstretched hand and heave yourself to safety; you gain one ally. If you fail, you panic and the train rushes by, painfully clipping your arm; you suffer two damage."
          , Test Will 0 ally (damage 2)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Arkham Advertiser"
          , "Chewing his cigar, editor Doyle Jefferies stalks through the newsroom. \"Where the hell is everybody today? You there!\" Doyle calls out to you, blowing smoke in your face. \"You looking for a job, friend?\" You gain a REPORTING GIG."
          , named "REPORTING GIG"
          )
        ,
          ( "Curiositie Shoppe"
          , "While picking through eclectic knick-knacks, you come across a jade pendant carved with an eye-in-star design. The amulet makes you feel safe when you gaze upon it. You may spend $3 to purchase the precious piece. If you do, you feel emboldened when you wear it; you become BLESSED."
          , mayPay (SpendMoney 3) blessed
          )
        ,
          ( "Train Station"
          , "You pace in front of the ticketing window. The exhausted-looking ticketing agent tells you something about bad weather and a derailment on the Boston-Arkham line. You may become delayed to keep waiting for your acquaintance's train to arrive. If you do, you gain one ally."
          , mayPay CostDelayed ally
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Arkham Advertiser"
          , "The Advertiser posts a reward for anyone who can clear the vermin out of their warehouse. You gain $2. Either test (influence) to negotiate a better reward or spend one remnant to prove it's not just rats. If you pass or spend the remnant, editor Doyle Jefferies agrees to your demands; you gain an additional $2."
          , Seq [money 2, orPay Influence "Spend one remnant" (SpendRemnants 1) (money 2)]
          )
        ,
          ( "Curiositie Shoppe"
          , "A tortoiseshell cat stalks along the top of a high shelf, pushing a necklace off and into your hands. Padding farther, it nudges at a small eyeglass case. You catch that too. Then an antique inkwell. \"You planning to buy all that?\" Oliver Thomas asks. You may buy any number of curio items from the display."
          , buyAny "Curio"
          )
        ,
          ( "Train Station"
          , "Joey \"the Rat\" is huddled in the shadows and beckons you. \"What're ya buyin'?\" You may buy one common item from the display or test (will) to threaten him. If you pass, you tell him you'll call the cops on him and he forces a \"gift\" into your hands before fleeing; you gain one common item."
          , Choose [("Buy one common item", buyOne "Common"), ("Test will", pass Will 0 commonItem)]
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Arkham Advertiser"
          , "Editor Doyle Jefferies doesn't seem interested in your story. \"That's page-five stuff at best,\" he growls. \"If it bleeds, it leads!\" You may spend one remnant to give him something worthy of the front page. If you do, he claps you on the back, saying \"don't bury the lede next time;\" you gain $3."
          , mayPay (SpendRemnants 1) (money 3)
          )
        ,
          ( "Curiositie Shoppe"
          , "You may buy any number of curio items from the display. As you are paying, you notice an immense book on a nearby stand. \"It's not for sale. But if you're careful you can take a look\" (lore). If you pass, you decipher a particular passage that may prove useful; you gain one spell."
          , Seq [buyAny "Curio", pass Lore 0 spell]
          )
        ,
          ( "Train Station"
          , "A woman steps off the train and waves at you. You try to remember who she is (will). If you pass, you sense that you can trust her when she tells you that you will soon become the savior of the world; you become BLESSED. If you fail, she was waving at someone behind you."
          , pass Will 0 blessed
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Arkham Advertiser"
          , "\"The people of Arkham need to know they're in danger,\" Minnie Klein tells you, \"but Doyle won't run the story without proof.\" You may spend one remnant to make sure her article is published. If you do, those whose lives are saved remember you in their thoughts and prayers; you become BLESSED."
          , mayPay (SpendRemnants 1) blessed
          )
        ,
          ( "Curiositie Shoppe"
          , "You arrive bright and early and wait for Oliver Thomas to unlock the door. \"Good idea, stopping by first thing. A couple stopped by yesterday and said they'd come back this afternoon. This may be your last chance.\" You may buy one curio item from the display for half price (rounded up)."
          , buyOneHalf "Curio"
          )
        ,
          ( "Train Station"
          , "A new arrival asks you to help track down a lost valise. You speak to Bill Washington, the old porter, about finding the bag (influence). If you pass, Bill retrieves the lost luggage and the stranger thanks you profusely; you gain one ally. If you fail, Bill is too busy to help right now."
          , pass Influence 0 ally
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Arkham Advertiser"
          , "Editor Doyle Jefferies tells you about a hot tip that he needs you to follow up on. Spawn one clue. Either test (influence) to ask for payment up front or spend one remnant to show him the types of challenges you face. If you pass or spend the remnant, Doyle reluctantly agrees; you gain $3."
          , Seq [SpawnOneClue, orPay Influence "Spend one remnant" (SpendRemnants 1) (money 3)]
          )
        ,
          ( "Curiositie Shoppe"
          , "You hear the door lock just as the lights go out. Something crawls past you in the dark (will). If you pass, you make your way to the counter and find a package with your name on it; you gain one curio item. If you fail, you climb onto a high table and yell for help; you suffer one horror."
          , Test Will 0 curioItem (horror 1)
          )
        ,
          ( "Train Station"
          , "You stare into the distant horizon and think about all the things you've endured and all that is still to come. You may retire your investigator and choose a new investigator. If you don't, a stranger pats you on the back and says, \"You look like you could use a friend;\" you gain one ally."
          , Choose [("Retire your investigator", Retire), ("Don't retire", ally)]
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Arkham Advertiser"
          , "\"You know anything about the supernatural?\" asks editor Doyle Jefferies. \"We've got a stack of tips and no one willing to do the legwork.\" Either test (lore) to tell him what you know or spend one remnant to establish your credentials. If you do, he pays you well for your unique insight; you gain $3."
          , orPay Lore "Spend one remnant" (SpendRemnants 1) (money 3)
          )
        ,
          ( "Curiositie Shoppe"
          , "You find your path blocked by several large crates. \"Just got back from an estate sale,\" calls the owner, Oliver Thomas. \"Go ahead and look through the boxes. I'm not sure what's in there but it's all available for purchase!\" You may buy any number of curio items from the display."
          , buyAny "Curio"
          )
        ,
          ( "Train Station"
          , "You sit with Bill Washington, the old train hand. He shows you something that was left behind, and you ask if you can keep it (influence). If you pass, \"Them folks back in Boston by now, so I don't see why not;\" you gain one common item. If you fail, he says he'll hold onto it in case the owner returns."
          , pass Influence 0 commonItem
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
          , "You trace your fingers along the pictographs on the cave wall (lore). If you pass, fragments of a secret history blossom like a garden in your brain; you gain one spell. If you fail, your mind reels at the terrible history you see; you suffer one horror."
          , Test Lore 0 spell (horror 1)
          )
        ,
          ( "General Store"
          , "You swap some boasts and tall tales with Mr. Hatle and the other old geezers playing checkers by the potbellied stove. Eventually, Davy Schoffner comes over and asks if you're planning to buy anything or if you'd all kindly be quiet and leave. You may buy any number of common items from the display."
          , buyAny "Common"
          )
        ,
          ( "Graveyard"
          , "You whistle as you walk by the graveyard. Maybe it's superstitious, but you always feel it brings you luck. When you're past the graveyard and you stop your whistling, you find a shiny penny in your path. You become BLESSED."
          , blessed
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Black Cave"
          , "The stone tablets are too heavy to move. You'll have to read them here. You gain one spell. The tide is coming in and the water laps at your ankles (will). If you pass, you hold your breath and swim out of the cave. If you fail, the undertow dashes you against the rocks; you suffer two damage."
          , Seq [spell, Test Will 0 NoEffect (damage 2)]
          )
        ,
          ( "General Store"
          , "A bedraggled gray tabby eyes you with suspicion from the alley nearby. Either test (influence) to pretend to ignore the cat or spend $1 to coax it out with fresh fish. If you pass or spend the money, the feline takes an interest; the STRAY CAT joins you. If you fail, the feline is wise to your tricks and ignores you harder."
          , orPay Influence "Spend $1" (SpendMoney 1) (named "STRAY CAT")
          )
        ,
          ( "Graveyard"
          , "You kick at the rats, trying to uncover what the horrid swarm is so interested in (strength). If you pass, you drive the vermin away and turn your attention to the alien creature they were gnawing on; you gain one remnant. If you fail, the rats bite and gnaw at your legs; you suffer one damage."
          , Test Strength 0 (remnants 1) (damage 1)
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Black Cave"
          , "You hide in the shadows and watch the robed figures incant their strange ritual. You gain one spell. The magic seems to flow through you (will). If you pass, you resist the effects and ward the area; you remove one doom from any space in your neighborhood. If you fail, you become CURSED."
          , Seq [spell, Test Will 0 (RemoveDoomFrom SpaceInYourNeighborhood (N 1)) cursed]
          )
        ,
          ( "General Store"
          , "Schoffner's General is busy today. You may buy any number of common items from the display. As you're leaving, Nathan is loading up the truck to make a few deliveries. You may help him. If you do, he offers to give you a ride; you may move up to three spaces."
          , Seq [buyAny "Common", May "Help Nathan" (MoveUpTo 3)]
          )
        ,
          ( "Graveyard"
          , "Someone grabs you from behind! You struggle with your attacker, hardly able to see them in the moonless night (strength). If you pass, they run away, leaving their torn coat pocket and wallet in your hand; you gain $3. If you fail, you catch a brief glimpse of their alien face; you suffer one horror."
          , Test Strength 0 (money 3) (horror 1)
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Black Cave"
          , "You find a water-logged journal that must belong to the Sheldon gang. It appears to be written in some sort of code (lore). If you pass, you break the cipher and discover a hidden cache of smuggled goods; you gain the CONTRABAND WHISKEY. If you fail, you're sure it is meant to lead you, but where?"
          , pass Lore 0 (named "CONTRABAND WHISKEY")
          )
        ,
          ( "General Store"
          , "You bang on the door again. \"Hold your horses!\" shouts the owner, Davy Schoffner, from inside. \"You know what time it is? Just let me get my drawers on.\" After a moment, the door opens and you rush inside. You may buy any number of common items from the display."
          , buyAny "Common"
          )
        ,
          ( "Graveyard"
          , "The groundskeeper Leonard Coburn asks you to help dig a fresh grave. Either test (strength) to get the job done quickly or become delayed to take your time. If you pass or become delayed, you uncover something strange; you gain one remnant and $3. If you fail, you wear yourself out and collapse."
          , Choose
              [ ("Test strength", pass Strength 0 (Seq [remnants 1, money 3]))
              , ("Become delayed", Pay CostDelayed (Seq [remnants 1, money 3]))
              ]
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Black Cave"
          , "Spurs of rock surround the passageway like a toothy maw. You steel yourself to enter (will). If you pass, you discover wax and chalk markings that suggest a ritual was held here and find something that was left behind; you gain one curio item. If you fail, you retreat in fear; you suffer one horror."
          , Test Will 0 curioItem (horror 1)
          )
        ,
          ( "General Store"
          , "The storm clouds part and a shaft of golden sunlight spills down upon you. The smell of the rain and warmth of the sun make you feel invincible. You become BLESSED. \"Flash sale! Everything half off!\" shouts Davy Schoffner. You may buy one common item from the display for half price (rounded up)."
          , Seq [blessed, buyOneHalf "Common"]
          )
        ,
          ( "Graveyard"
          , "The hairs on your neck stand on end (will). If you pass, you communicate with a lonely ghost who directs you to tarnished silver coins and urges you to spend them well; you gain $3. If you fail, the icy feeling of being watched follows you no matter how fast you flee; you suffer two horror."
          , Test Will 0 (money 3) (horror 2)
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Black Cave"
          , "An elaborate mosaic made of shells, glass, and tiles depicts a profane image of a toad-like monstrosity. You may become delayed to painstakingly remove the picture piece by piece. If you do, you feel as if the oppressive sense of foulness that once permeated the cave has been reduced; you become BLESSED."
          , mayPay CostDelayed blessed
          )
        ,
          ( "General Store"
          , "You notice the rifle that usually hangs on the wall has gone missing. \"I've got it loaded behind the counter,\" says Davy. \"We've been getting a lot of dangerous oddballs coming around, but don't worry. We plan on staying open.\" You may buy any number of common items from the display."
          , buyAny "Common"
          )
        ,
          ( "Graveyard"
          , "The darkness and fog make you wish you hadn't agreed to dig these graves. Suddenly, something cold and wet touches the back of your leg (will). If you pass, it's just a friendly dog with a wallet in its mouth; you gain $3. If you fail, you run screaming into the night; you suffer one horror."
          , Test Will 0 (money 3) (horror 1)
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Black Cave"
          , "The blackness around you is watching and listening. Either test (will) to steel your mind and stare down the darkness or spend one remnant to appease it. If you pass or spend the remnant, the darkness flows into your mind; you gain one spell. If you fail, the pitiless blackness drives you back toward the light."
          , orPay Will "Spend one remnant" (SpendRemnants 1) spell
          )
        ,
          ( "General Store"
          , "The owner is busy with another customer. Nathan, the delivery boy, walks up to you. \"If you give me a list of the things you need,\" he mumbles, \"I can see what we have and ring you up.\" He searches his apron pockets for a pencil. You may buy any number of common items from the display."
          , buyAny "Common"
          )
        ,
          ( "Graveyard"
          , "A canine figure lurks by an open grave, gnawing on a severed limb. You loose a mighty roar (will). If you pass, the ghoul flees and leaves behind the remains of its meal; you gain one remnant. If you fail, the ghoul howls and knocks you down before escaping into the dark; you suffer one horror."
          , Test Will 0 (remnants 1) (horror 1)
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Black Cave"
          , "According to local legend, a witch once lived in these caves. Either test (lore) to remember the exact location or spend one clue to check your notes. If you pass or spend the clue, you discover old, broken trinkets; you gain one curio item. If you fail, you hear the whispers of an old woman in the cave."
          , orPay Lore "Spend one clue" (SpendClues 1) curioItem
          )
        ,
          ( "General Store"
          , "Davy Schoffner, the owner, waves his pencil in the air, taking a mental tally of his wares. \"Inventory time,\" he tells you, \"Please buy something. I've put it all on sale. I don't want to be here all night counting it.\" You may buy one common item from the display for half price (rounded up)."
          , buyOneHalf "Common"
          )
        ,
          ( "Graveyard"
          , "As you enter, you notice a sign on the gatehouse indicating that the groundskeeper Leonard Coburn is hiring a gravedigger. You may apply for the position. If you do, you become a GRAVEDIGGER. If you don't, you enjoy your leisurely stroll through the graveyard; you or an ally recovers one sanity."
          , Choose [("Apply for the position", named "GRAVEDIGGER"), ("Take a stroll", sanity 1)]
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
          , "As you explore the library, you find genealogical records of your own family. You may spend one remnant to compare the documents to your findings. If you do, you trace your lineage to the Marsh family of Innsmouth; you suffer one direct horror and gain the INNSMOUTH LOOK."
          , mayPay (SpendRemnants 1) (Seq [DirectHorror (N 1), named "INNSMOUTH LOOK"])
          )
        ,
          ( "Ma's Boarding House"
          , "Body and mind heavy as lead, you collapse on the couch—you're asleep before you can pay for a room. You or an ally recovers two health. Ma shows mercy and lets you sleep for a couple of hours before waking you, not unkindly, and sending you on your way. You move to an adjacent street and become delayed."
          , Seq [health 2, MoveDirectlyTo AdjacentStreet, delayed]
          )
        ,
          ( "South Church"
          , "You sit in the silence of the nearly empty church. The building feels like a safe haven, despite the crumbling walls and cracked pews. You may spend $1 to donate to the church's repair costs. If you do, you feel better knowing you have somewhere to return to; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Historical Society"
          , "The dejected curator writes \"canceled\" on the placard advertising tonight's guest speaker. Either test (influence) to offer to replace the speaker or spend one remnant to donate your finding for discussion. If you pass or spend the remnant, someone you speak with is moved by your speech; you gain one ally."
          , orPay Influence "Spend one remnant" (SpendRemnants 1) ally
          )
        ,
          ( "Ma's Boarding House"
          , "You arrive just in time for dinner. Ma places the heaping plate of roast beef, carrots, and potatoes in front of you and wipes her hands on her apron. You or an ally recovers two health. \"Second helping, deary?\" asks Ma, holding another plate. You may spend $1 for you or an ally to recover two health."
          , Seq [health 2, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "South Church"
          , "You step into the hush of the confessional and release a sigh. \"What's troubling you, my child?\" asks the kindly voice of Father Michael. All of your fears come tumbling out of you. Father Michael assures you that your path is true and just. You recover two sanity and become BLESSED."
          , Seq [mySanity 2, blessed]
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Historical Society"
          , "You browse the collection of the historical society and comment about a particular piece that stands out to you. \"Why, this isn't part of the collection,\" says the curator Mr. Peabody. \"How did this get here? I suppose you'd better take it with you.\" You gain one curio item."
          , curioItem
          )
        ,
          ( "Ma's Boarding House"
          , "\"Join me for a taste?\" asks \"Jazz\" Mulligan, the head janitor from Miskatonic University, as Ma sets out a fresh apple pie. \"Oops, I think I forgot my wallet.\" You may spend $1 to pay for his share. If you do, you share a companionable evening eating pie and chatting; you or an ally recovers three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ,
          ( "South Church"
          , "You step into the hush of the confessional and release a sigh. An unnatural darkness clings to the chamber, and a sinister rattling voice whispers to you from behind the curtain, \"The power you seek is yours for the taking.\" You may gain a DARK PACT condition to gain a DARK BLESSING."
          , mayPay (CostCondition "DARK PACT") (named "DARK BLESSING")
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Historical Society"
          , "You ask the curator, Mr. Peabody, to let you borrow something from the society's collection (influence). If you pass, he grants your request, \"but be careful with it, if you please;\" you gain one curio item. If you fail, Mr. Peabody explains, at length, that these items are of historical value and aren't for your use."
          , pass Influence 0 curioItem
          )
        ,
          ( "Ma's Boarding House"
          , "A short rest at Ma's is just what you need. You may spend $1 for you or an ally to recover three health. Ma tells you about a hunter who stayed with her but vanished in the night. That was weeks ago, and now Ma is looking for someone to take care of the man's dog. The HUNTING DOG joins you."
          , Seq [mayPay (SpendMoney 1) (health 3), named "HUNTING DOG"]
          )
        ,
          ( "South Church"
          , "J. S. Bach was a devout Protestant, but the Catholics don't seem to be holding that against him. The majestic chords of his Mass in B Minor echo through the church. You allow the melody to flow through you and carry away your worries and fears. You or an ally recovers two sanity."
          , sanity 2
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Historical Society"
          , "Carl Sanford, the leader of the Silver Twilight Lodge, says he foresees great things in your future. You become BLESSED. He mentions that the \"strange happenings\" in Arkham worry him. You may spend one remnant to share your own concerns. If you do, Sanford arranges an introduction; you gain one ally."
          , Seq [blessed, mayPay (SpendRemnants 1) ally]
          )
        ,
          ( "Ma's Boarding House"
          , "The storm has knocked out the power, but you pass the evening by Ma's fireplace and feel renewed. You or an ally recovers two health. The howling wind has a way of creeping into the boarding house, but Ma's homemade stew will warm you right up. You may spend $1 for you or an ally to recover two health."
          , Seq [health 2, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "South Church"
          , "The church is filled with people who are terrified and devastated. You may spend $1 to help provide food and shelter for those who have lost everything due to the dark forces roaming the streets of Arkham. If you do, you see hope for the city's recovery; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Historical Society"
          , "Another guest of the society is fascinated by one of the displays describing Arkham's witch trials (influence). If you pass, you strike up a friendly conversation and discover you share a cause; you gain one ally. If you fail, your attempts at joviality backfire and the guest leaves in a huff."
          , pass Influence 0 ally
          )
        ,
          ( "Ma's Boarding House"
          , "A guest damaged some antique furniture. Either test (influence) to assuage Ma's wrath or spend $3 to pay for the repairs. If you pass or spend the money, the apologetic guest asks if you need any help; you gain one ally. If you fail, Ma's wrath turns on you as well; move to an adjacent street."
          , Choose
              [ ("Test influence", Test Influence 0 ally (MoveDirectlyTo AdjacentStreet))
              , ("Spend $3", Pay (SpendMoney 3) ally)
              ]
          )
        ,
          ( "South Church"
          , "Father Michael asks you to contribute toward a dinner provided for the poor and homeless. You may spend $1 to help pay for groceries. If you do, you spend the evening serving food and sharing a pleasant meal with Arkham's less fortunate; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Historical Society"
          , "Only donating members of the society have access to several of the museum's exhibits. You may spend one remnant to donate your findings. If you do, you study a map of Arkham with strange markings and find something that has been abandoned under the table; you gain one curio item and spawn one clue."
          , mayPay (SpendRemnants 1) (Seq [curioItem, SpawnOneClue])
          )
        ,
          ( "Ma's Boarding House"
          , "One of the guests introduces himself as \"Sawbones,\" and explains that he was a medic during the Great War. After a rambling story about the horrors he saw there, he offers to help you if you'll just buy him a little something to keep his hands steady. You may spend $1 for you or an ally to recover three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ,
          ( "South Church"
          , "The church is filled with inspirational artwork and you get so engrossed that you are unaware of how late it has become. You recover four sanity and become delayed. The church bell pulls you from your reverie (will). If you pass, you resolve to preserve this beauty; you focus one skill of your choice."
          , Seq [mySanity 4, delayed, pass Will 0 focusAny]
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Historical Society"
          , "All of the tickets to tonight's event have been sold. Either test (influence) to convince the doorman that you're a guest speaker or spend $3 to buy someone's ticket. If you pass or spend the money, the exhibit features several oddities from Arkham's storied past; you gain one curio item."
          , orPay Influence "Spend $3" (SpendMoney 3) curioItem
          )
        ,
          ( "Ma's Boarding House"
          , "Ryan Dean, a traveling salesman, proffers a bottle of his \"cure-all tonic.\" \"The stuff works wonders, but it's not cheap!\" You may spend $2 to pay what he's asking. If you spend the money, you find the medicine has a foul taste but it works miracles; you recover three health and become BLESSED."
          , mayPay (SpendMoney 2) (Seq [myHealth 3, blessed])
          )
        ,
          ( "South Church"
          , "A missionary has returned to tell stories of her travels in the remotest parts of the world. You may spend $1 to support her ongoing work. If you do, she gratefully shares a traditional chant for good luck; you or an ally recovers three sanity."
          , mayPay (SpendMoney 1) (sanity 3)
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
          , "A woman lights candles all throughout the old graveyard, but only criminals and witches are interred here (will). If you pass, you befriend the woman and she explains her grandmother was a witch; DAYANA ESPERENCE joins you and you gain one remnant. If you fail, you avoid the place."
          , pass Will 0 (Seq [named "DAYANA ESPERENCE", remnants 1])
          )
        ,
          ( "St. Mary's Hospital"
          , "Looking furtively left and right, the orderly opens the glass case and takes out a small bottle of painkillers. You or an ally recovers two health. \"Any more and you gotta pay. I'm not running a charity here.\" You may spend $1 for you or an ally to recover two health."
          , Seq [health 2, mayPay (SpendMoney 1) (health 2)]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "\"That's enough browsing!\" Startled, you jerk up to see Miriam Beecher, the proprietor, at your shoulder. \"If you're not buying, then get out!\" Reveal the top three spells in the deck. You may buy one of them for half price (rounded up). Put the rest on the bottom of the deck."
          , spells 3 (Just 1) True
          )
        ]
      )
    ,
      ( 2
      ,
        [
          ( "Hangman's Hill"
          , "You find a trapdoor under the moldering runner in the decaying church, but it's warped and stuck shut (strength). If you pass, you discover laboratory equipment in a secret basement and the remains of some strange creature; you gain one remnant. If you fail, whatever's under there will remain a mystery."
          , pass Strength 0 (remnants 1)
          )
        ,
          ( "St. Mary's Hospital"
          , "\"Please wait your turn,\" says Nurse Sharon. \"There are a lot of patients ahead of you.\" Either test (influence) to put on your best smile or spend $1 to make yourself a priority. If you pass or spend the money, she agrees to see you now; you or an ally recovers three health. If you fail, you don't have time to wait."
          , orPay Influence "Spend $1" (SpendMoney 1) (health 3)
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "You see a familiar-looking old book and unfold the blood-spattered page from your pocket. The markings in the margins match. You may spend $3 to purchase the rare text. If you do, you align the torn edge along the book's inner spine and read the writing in its entirety; you become BLESSED."
          , mayPay (SpendMoney 3) blessed
          )
        ]
      )
    ,
      ( 3
      ,
        [
          ( "Hangman's Hill"
          , "A coven of witches has gathered in the woods, performing some sort of occult ritual. You try to keep calm (will). If you pass, you observe them from the shadows and steal some ritual components; you gain one spell and one remnant. If you fail, they hear your gasps of dismay; you become CURSED."
          , Test Will 0 (Seq [spell, remnants 1]) cursed
          )
        ,
          ( "St. Mary's Hospital"
          , "Nurse Sharon is quick and efficient, if not precisely gentle. You or an ally recovers two health. Either test (influence) to request some painkillers or spend $1 to buy them outright. If you pass or spend the money, Doctor Mortimore writes you a prescription for some morphine; you or an ally recovers two health."
          , Seq [health 2, orPay Influence "Spend $1" (SpendMoney 1) (health 2)]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "A sleek black cat rubs against your leg. The BLACK CAT joins you. \"That's no normal feline,\" says Miriam Beecher. Either test (lore) to show her you're no normal human or spend $2 to pay for her advice. If you pass or spend the money, Miriam and the cat teach you a charm; you gain one spell."
          , Seq [named "BLACK CAT", orPay Lore "Spend $2" (SpendMoney 2) spell]
          )
        ]
      )
    ,
      ( 4
      ,
        [
          ( "Hangman's Hill"
          , "\"It fell off a truck, you get me?\" explains Joey Vigil. \"You buying or what?\" Either test (strength) to extort him or spend $3 to buy. If you pass or spend the money, Joey \"the Rat\" leaves you with your new property; you gain one common item. If you fail, Joey's tougher than he looks; you suffer one damage."
          , Choose
              [ ("Test strength", Test Strength 0 commonItem (damage 1))
              , ("Spend $3", Pay (SpendMoney 3) commonItem)
              ]
          )
        ,
          ( "St. Mary's Hospital"
          , "\"Well you're just covered in bumps and bruises, honey,\" notes Doctor Maheswaren. You or an ally recovers two health. \"What have you been up to?\" You may spend one clue or one remnant to tell her. If you do, she shares what she's experienced; place one clue from the clue pool on the scenario sheet."
          , Seq
              [ health 2
              , Choose
                  [ ("Spend one clue", Pay (SpendClues 1) (PlaceCluesOnSheet (N 1)))
                  , ("Spend one remnant", Pay (SpendRemnants 1) (PlaceCluesOnSheet (N 1)))
                  , ("Decline", NoEffect)
                  ]
              ]
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "\"These books were owned by the Comte de Saint Germain himself,\" explains Miriam Beecher. \"So you know I can't let 'em go cheap.\" Reveal the top four spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
          , spells 4 Nothing False
          )
        ]
      )
    ,
      ( 5
      ,
        [
          ( "Hangman's Hill"
          , "Recent rains have washed up an old coffin. You steel yourself and pry it open (will). If you pass, the body within isn't human and something was buried with it; you gain one common item and one remnant. If you fail, the corpse's sightless eye sockets stare into your soul; you suffer one horror."
          , Test Will 0 (Seq [commonItem, remnants 1]) (horror 1)
          )
        ,
          ( "St. Mary's Hospital"
          , "Nurse Sharon calls your name. \"It's been a pretty quiet day. We'll have you fixed up and out the door in no time.\" After Doctor Mortimore examines you, he recommends a few quick and inexpensive procedures. You may spend $1 for you or an ally to recover three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "Miriam Beecher agrees to teach you meditation exercises. You focus lore even if it exceeds your focus limit. \"That was just a taste,\" she says. \"I charge for the advanced classes.\" Reveal the top three spells in the deck. You may buy one of them. Then put the rest on the bottom of the deck."
          , Seq [Focus (Just Lore) True, spells 3 (Just 1) False]
          )
        ]
      )
    ,
      ( 6
      ,
        [
          ( "Hangman's Hill"
          , "You see something on the bank of Hangman's Brook. You gain one common item. You wade into the water looking for more (will). If you pass, you find something strange wrapped in oilskin; you gain one remnant. If you fail, you find nothing before the cold water forces you to abandon your search."
          , Seq [commonItem, pass Will 0 (remnants 1)]
          )
        ,
          ( "St. Mary's Hospital"
          , "The hospital is too busy and none of the regular doctors or nurses have any time for you. An old Chinese man proposes acupuncture. You may spend $2 to have him stick you with needles. If you do, it doesn't do anything for your wounds but you feel surprisingly refreshed; you become BLESSED."
          , mayPay (SpendMoney 2) blessed
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "The proprietor Miriam Beecher motions to you. \"For a real aficionado like yourself, I keep a special collection of our rarest offerings in the back room. Care for a look?\" Reveal the top four spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
          , spells 4 Nothing False
          )
        ]
      )
    ,
      ( 7
      ,
        [
          ( "Hangman's Hill"
          , "You wait until the Sheldon Gang's thugs leave before sneaking in to pry open a crate (strength). If you pass, it seems the rum-runners are smuggling more than just rum; you gain one common item. If you fail, you take too long and the Sheldon Gang returns to rough you up; you suffer two damage."
          , Test Strength 0 commonItem (damage 2)
          )
        ,
          ( "St. Mary's Hospital"
          , "While you are waiting, you visit the hospital's cafeteria. Once you smell the food you realize how long it has been since you had a real meal. Maybe all you really need is something to eat. You may spend $1 for you or an ally to recover three health."
          , mayPay (SpendMoney 1) (health 3)
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "\"You're in luck,\" Miriam Beecher tells you. \"I just got this box of books from a collector in Boston. I haven't had a chance to sort it yet. Have a look and make me an offer!\" Reveal the top three spells in the deck. You may buy any number of them. Put the rest on the bottom of the deck."
          , spells 3 Nothing False
          )
        ]
      )
    ,
      ( 8
      ,
        [
          ( "Hangman's Hill"
          , "A haggard man is sitting on the hill, eating the petals of witchweed flowers. He laughs when he sees you and says, \"Tell me a story or listen to mine. Show me a marvel, my good fortune's thine.\" You may spend one focus to play along. If you do, he tells you terrifying truths; you become BLESSED."
          , mayPay (SpendFocus 1) blessed
          )
        ,
          ( "St. Mary's Hospital"
          , "Doctor Maheswaren looks over the results. \"It could be something serious. You're in luck, though. We've just begun a trial of a new serum that has been working wonders.\" Joining the trial is expensive, but Maheswaren is very enthusiastic about the results. You may spend $2 to gain the MYSTERIOUS SERUM."
          , mayPay (SpendMoney 2) (named "MYSTERIOUS SERUM")
          )
        ,
          ( "Ye Olde Magick Shoppe"
          , "The book calls to you, and when you touch it the knowledge within rushes into your mind like a living thing. You gain one spell. Your mind staggers as it attempts to absorb this new cosmic understanding (will). If you fail, you feel as if the spell is an alien intelligence; you suffer two horror."
          , Seq [spell, Test Will 0 NoEffect (horror 2)]
          )
        ]
      )
    ]
