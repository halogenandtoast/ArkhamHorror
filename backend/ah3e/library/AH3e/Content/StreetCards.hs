module AH3e.Content.StreetCards (cards) where

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
cards = map (uncurry card) streets

card :: Int -> [(StreetType, Text, Effect)] -> CardDef
card n encounters =
  CardDef
    { code = CardCode ("street-" <> T.justifyRight 2 '0' (tshow n))
    , name = "Street " <> tshow n <> "/8"
    , expansion = CoreSet
    , copies = 1
    , kind = StreetCard (Map.fromList [(st, Encounter txt eff) | (st, txt, eff) <- encounters])
    }

streets :: [(Int, [(StreetType, Text, Effect)])]
streets =
  [
    ( 1
    ,
      [
        ( Residential
        , "Joey Vigil's sharp whistle catches your attention from a nearby alley. \"I know you need a little help and can't afford to ask too many questions,\" he says, \"so I got a special deal for you.\" You may buy one common item from the display for half price (rounded up)."
        , buyOneHalf "Common"
        )
      ,
        ( Bridge
        , "A leathery old man stands against the rail in the center of the river, fishing rod in hand and a bucket of crushed ice at his feet. Seeing you, he nods and, in traditional laconic New England fashion, offers you a cold beer and a rest. You may become delayed for you or an ally to recover two sanity."
        , mayPay CostDelayed (sanity 2)
        )
      ,
        ( Scenic
        , "Something rustles through the bushes to your right. It's probably nothing, and you struggle to keep calm (will). If you pass, a friendly dog bursts from the leaves and demands affection; you or an ally recovers three sanity. If you fail, the rustling becomes cracking and crashing and you run for it."
        , pass Will 0 (sanity 3)
        )
      ]
    )
  ,
    ( 2
    ,
      [
        ( Residential
        , "Abner Weems, the local drunk, staggers in front of you and grabs your lapels. \"My wife,\" he moans, \"my daughter. I miss them so.\" He presses his face against you and sobs, leaving you damp and smelly. You may become delayed to comfort him. If you do, he's grateful; you gain one curio item."
        , mayPay CostDelayed curioItem
        )
      ,
        ( Bridge
        , "Nathan, the delivery boy from Schoffner's General Store, walks past you with a hand-truck, and something falls from the stack of boxes. You may return the goods. If you do, he thanks you profusely; gain $1 and recover one health and one sanity. Otherwise, you gain one common item."
        , Choose
            [ ("Return the goods", Seq [money 1, RecoverBoth You (N 1) (N 1)])
            , ("Keep them", commonItem)
            ]
        )
      ,
        ( Scenic
        , "You come across a park bench beneath a stand of birch trees, their leaves golden in the setting sun. You sit and enjoy a few quiet moments of birdsong, warmth, and peace. You or an ally recovers one health and one sanity."
        , RecoverBoth YouOrAlly (N 1) (N 1)
        )
      ]
    )
  ,
    ( 3
    ,
      [
        ( Residential
        , "Ryan Dean stands on the bed of an old truck, talking up the efficacy of his latest product to a crowd of locals (influence). If you pass, you see through his con and he slips you a few bills to keep you quiet; you gain $2. If you fail, you buy a bottle of Clark Stanley's Snake Oil Liniment; you discard $2."
        , Test Influence 0 (money 2) (LoseMoney (N 2))
        )
      ,
        ( Bridge
        , "Abner Weems, the town drunk, is leaning over the edge of the bridge and for a moment it seems he might jump (influence). If you pass, Abner reveals that he was just fetching something he spotted on the edge of the bridge; you gain one common item. If you fail, Abner cusses you out and staggers off."
        , pass Influence 0 commonItem
        )
      ,
        ( Scenic
        , "Something glimmers in the shadows beneath the trees (observation). If you pass, you find a small cache of shiny rocks and coins, presumably stashed by a crow or rodent; you gain $2. If you fail, you pause to look but can't see anything of interest from the path aside from a small passing deer."
        , pass Observation 0 (money 2)
        )
      ]
    )
  ,
    ( 4
    ,
      [
        ( Residential
        , "The grim clouds and dim light match your mood as you trudge through Arkham's streets. The empty windows and doorways around you seem to stare, mocking your confusion and your quest, but then a shining coin catches your eye. You gain $1. Walking more easily now, you continue on."
        , money 1
        )
      ,
        ( Bridge
        , "You can't find the source of the persistent \"psst!\" at first, until you think to look under the bridge where a disheveled-looking Joey \"the Rat\" Vigil is skulking. \"I need to raise some funds and quick,\" he says. You may buy one common item from the display for half price (rounded up)."
        , buyOneHalf "Common"
        )
      ,
        ( Scenic
        , "You hear a cry for help\x2014a robed figure with a curved knife is assaulting some poor innocent person (strength)! If you pass, you drive the assailant away for the grateful praise of your new best friend for life; gain one ally. If you fail, you are too late to prevent another of Arkham's mysterious murders."
        , pass Strength 0 ally
        )
      ]
    )
  ,
    ( 5
    ,
      [
        ( Residential
        , "An old woman startles you from your reverie and begs your help to cross the street. She chatters about her grandchildren all the way to the other side. Her ignorance of the darkness you fight is somehow reassuring. You or an ally recovers one sanity, and you focus one skill of your choice."
        , Seq [sanity 1, focusAny]
        )
      ,
        ( Bridge
        , "From your vantage point, you can see a flock of gulls arguing loudly over something at the edge of the water (observation). If you pass, you discover that their prize is the corpse of a robed figure, which you quickly search; you gain one remnant. If you fail, it's probably just a flock of gulls being gulls."
        , pass Observation 0 (remnants 1)
        )
      ,
        ( Scenic
        , "A tremendous old apple tree leans over the gravel road, its limbs heavy with round, red fruit. You help yourself to a particularly fine-looking apple; you or an ally recovers two health. After a few bites, it's clear the apple is a little shy of ripe and the tartness makes your lips pucker, but you finish it anyway."
        , health 2
        )
      ]
    )
  ,
    ( 6
    ,
      [
        ( Residential
        , "You remember a shortcut near here (observation). If you pass, you duck along an alley and cut a full block from your route; you may move up to two spaces, ignoring monsters during this movement. If you fail, you double back after you spot a monster; spawn one monster in your space and exhaust it."
        , Test Observation 0 (MoveUpToIgnoringMonsters 2) (SpawnMonsterIn YourSpace True)
        )
      ,
        ( Bridge
        , "A truck rumbles by, loaded with newspapers outbound from the Arkham Advertiser. The truck hits a bump and one paper tumbles free. It is immediately caught by the wind and blows past you (observation). If you pass, you snatch the paper and read it carefully; spawn one clue."
        , pass Observation 0 SpawnOneClue
        )
      ,
        ( Scenic
        , "Something has been carved into the bark of a tree (lore). If you pass, you recognize the proud declaration of youthful love some twenty years gone in the scars on the tree bark; you or an ally recovers two sanity. If you fail, the evidence of widespread and persistent cult activity makes you shudder."
        , pass Lore 0 (sanity 2)
        )
      ]
    )
  ,
    ( 7
    ,
      [
        ( Residential
        , "\"Stop right there!\" A figure rushes past you with Deputy Dingby hot on the trail (observation). If you pass, you see where the thief stashes their loot; you gain one common item. If you fail, Dingby arrests you while you argue that you look nothing like the thief; you become delayed."
        , Test Observation 0 commonItem delayed
        )
      ,
        ( Bridge
        , "Andre Hopkins is pushing his cart past you and the smell of roasting peanuts makes your mouth water. \"Want something?\" he asks you. \"I still got some left.\" You may spend $1 for you or an ally to recover three health."
        , mayPay (SpendMoney 1) (health 3)
        )
      ,
        ( Scenic
        , "The rustling of the leaves around you seems to be the whispers of an unknown god, something that dwelt here before the coming of mankind, something that will dwell here still long after mankind is gone. You scurry on, eager to get away from the trees. You may move one space."
        , May "Move one space" (MoveUpTo 1)
        )
      ]
    )
  ,
    ( 8
    ,
      [
        ( Residential
        , "You pass Anna Kaslow on the street, and she whirls and grabs your arm. \"You!\" she hisses. \"You're the one I've seen in my visions. Let me teach you (lore).\" If you pass, she proves to be more than just a simple fortune-teller; you gain one spell. If you fail, you dismiss her ravings as chicanery or dementia."
        , pass Lore 0 spell
        )
      ,
        ( Bridge
        , "Respectable, elderly Carl Sanford stands at the edge of the bridge with two burly men in bespoke suits. They seem to be watching something in the water, until they turn away as if satisfied. As they pass you, Sanford slips a few bills into your hand. \"You didn't see us here,\" he says. You gain $2."
        , money 2
        )
      ,
        ( Scenic
        , "A basket of fruit stands by the side of the road, with a sign reading \"Honor system. Apples $1 per pear.\" The basket contains both apples and pears. You may eat the fruit to recover two health and you may spend $1 to pay for it. If you eat without paying, you feel guilty all day; you become CURSED."
        , May "Eat the fruit" (Seq [myHealth 2, MayPay (SpendMoney 1) NoEffect cursed])
        )
      ]
    )
  ]
