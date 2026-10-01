{- | The travel route deck. A travel route card prints one encounter per route
type, the way a street card prints one per street type, and nearly every one of
them offers to carry you onward (the @travel-onward@ effect).
-}
module AH3e.Content.UnderDarkWaves.TravelRoutes (cards) where

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
cards = fromBox UnderDarkWaves (map (uncurry card) routes)

card :: Int -> [(RouteType, Text, Effect)] -> CardDef
card n encounters =
  CardDef
    { code = CardCode ("travel-route-" <> T.justifyRight 2 '0' (tshow n))
    , name = "Travel Route " <> tshow n <> "/8"
    , expansion = CoreSet
    , copies = 1
    , kind = TravelRouteCard (Map.fromList [(rt, Encounter txt eff) | (rt, txt, eff) <- encounters])
    }

-- | "You may move one space or move to another <route>."
onward :: Effect
onward = Custom "travel-onward"

routes :: [(Int, [(RouteType, Text, Effect)])]
routes =
  [
    ( 1
    ,
      [
        ( CountryRoad
        , "There are only a handful of signs on this road and most of them are hidden behind overgrown trees. You quickly find that your map isn't reliable, and to reach your destination you need to pay careful attention (observation). If you pass, you may move one space or move to another country road."
        , pass Observation 0 onward
        )
      ,
        ( FerryTerminal
        , "Most of the time, you can sneak aboard a ferry and travel for free. You may move one space or move to another ferry terminal. This time, however, the crew is inspecting every ticket. You may spend $1 to buy a ticket or test influence to talk your way past the crew. If you pass or spend the money, you relax on the deck; you or an ally may recover two sanity."
        , Seq [onward, orPay Influence "Spend $1" (SpendMoney 1) (sanity 2)]
        )
      ,
        ( TrainPlatform
        , "Traveling at the busiest time of day helps to maintain your anonymity. You may move one space or move to another train platform. With this many passengers on board it is inevitable that one or more ambitious merchants will move from car to car, selling their wares. You may buy one common item from the display."
        , Seq [onward, buyOne "Common"]
        )
      ]
    )
  ,
    ( 2
    ,
      [
        ( CountryRoad
        , "A recent windstorm has left the road blocked by a ravaged tree trunk (strength). If you pass, you haul the fallen log aside and go on your way; you may move one space or move to another country road. If you fail, you are stranded under the moonless sky, and feel many eyes watching you; suffer one horror."
        , Test Strength 0 onward (horror 1)
        )
      ,
        ( FerryTerminal
        , "Several passengers are heading to a riverside festival. You may move one space or move to another ferry terminal. When you step off the boat you are greeted by the sounds of the event and the smell of popcorn and hot dogs. You may spend $1 for you or an ally to recover two health."
        , Seq [onward, mayPay (SpendMoney 1) (health 2)]
        )
      ,
        ( TrainPlatform
        , "You make it on board, but the train is packed with dozens of travelers hoping to get out of town before things get even worse. You may move one space or move to another train platform. One of the porters offers you a restful spot in the relatively quiet baggage car, for a price. You may spend $1 for you or an ally to recover one health."
        , Seq [onward, mayPay (SpendMoney 1) (health 1)]
        )
      ]
    )
  ,
    ( 3
    ,
      [
        ( CountryRoad
        , "After driving over a large stone, you hear a loud crack. When you examine the vehicle, you see the axle is broken. Suffer one damage from the long, exhausting walk unless you spend $1 to hire a truck to tow you to your destination. Either way, you may move one space or move to another country road."
        , Seq [MayPay (SpendMoney 1) NoEffect (damage 1), onward]
        )
      ,
        ( FerryTerminal
        , "The police will not let the ferry leave until they find a fugitive hiding somewhere on the boat. In the interest of getting to your destination, you help the police find their man (observation). If you pass, the fugitive is caught with your help and the police pay you for the tip off; gain $2 and you may move one space or move to another ferry terminal."
        , pass Observation 0 (Seq [money 2, onward])
        )
      ,
        ( TrainPlatform
        , "You sit next to a traveling salesman during the journey, and he promises you a sample of his wares when you arrive. You may move one space or move to another train platform. Once there, the salesman sheepishly confesses that his products will come in tomorrow. You may become delayed to gain one common item."
        , Seq [onward, mayPay CostDelayed commonItem]
        )
      ]
    )
  ,
    ( 4
    ,
      [
        ( CountryRoad
        , "You may move one space or move to another country road. As you approach your destination, you see some misshapen figure running through the woods. You may become delayed to lure this abomination away from the city. If you do, become BLESSED. If you do not, spawn one monster."
        , Seq [onward, MayPay CostDelayed blessed SpawnMonster]
        )
      ,
        ( FerryTerminal
        , "As you travel, you strike up a conversation with the crew. You may move one space or move to another ferry terminal. The captain tells you that he's been seeing monstrous things in the river and he thinks he may be going mad. You may spend one remnant to prove that what he saw is real. If you do, the captain gratefully offers you a gift; gain one curio."
        , Seq [onward, mayPay (SpendRemnants 1) curioItem]
        )
      ,
        ( TrainPlatform
        , "For once, the train actually leaves on time. You may move one space or move to another train platform. If you do, the train hits a large beast on the way, and the crew needs help to push the freakish body off the tracks (strength). If you pass, you collect a fang; gain one remnant. If you fail, you strain yourself; suffer one damage."
        , Custom "travel-onward:beast"
        )
      ]
    )
  ,
    ( 5
    ,
      [
        ( CountryRoad
        , "This road grows more indistinct until eventually there is no road at all. You think you can find your way by studying the geography and plant life (lore). If you pass, you find the right path flanked by unusual flowers; gain one remnant and you may move one space or move to another country road."
        , pass Lore 0 (Seq [remnants 1, onward])
        )
      ,
        ( FerryTerminal
        , "You may move one space or move to another ferry terminal. A young man sits next to you on your voyage. He tells you about his plans to start a new life once he arrives. You can see that he needs more money than he has to succeed. You offer to buy something from him to help him out. You may buy one common item from the display."
        , Seq [onward, buyOne "Common"]
        )
      ,
        ( TrainPlatform
        , "The only tickets available at the moment are for a shared compartment. You may move one space or move to another train platform. If you do, you try to engage the stranger sharing your room in meaningful conversation (influence). If you pass, you make a new friend; gain one ally."
        , Custom "travel-onward:stranger"
        )
      ]
    )
  ,
    ( 6
    ,
      [
        ( CountryRoad
        , "The police at the road block insist that you turn back. You may spend one remnant to convince them that your journey is vital. If you do, they let you by with something you can use to defend yourself; gain one common item and you may move one space or move to another country road."
        , mayPay (SpendRemnants 1) (Seq [commonItem, onward])
        )
      ,
        ( FerryTerminal
        , "A man on the dock demands that you answer a riddle (lore). If you pass, he whispers a strange chant in your ear; gain one spell. If you fail, he tells you that your path will be hidden in shadow; become TAINTED. When you board the boat, no one else on the ferry saw him there. You may move one space or move to another ferry terminal."
        , Seq [Test Lore 0 spell tainted, onward]
        )
      ,
        ( TrainPlatform
        , "You board a train transporting dozens of injured men from a Boston hospital. You may move one space or move to another train platform. The attending doctors would welcome any medical knowledge you possess (lore). If you pass, the doctors gratefully provide you with medicine; you or an ally may recover two health."
        , Seq [onward, pass Lore 0 (health 2)]
        )
      ]
    )
  ,
    ( 7
    ,
      [
        ( CountryRoad
        , "You search a mangled, rust-covered car in the ditch. Gain one curio. You hear ghostly screams from the victims of this wreck (will). If you pass, the voices pass and you continue on your way; you may move one space or move to another country road. If you fail, the voices continue to haunt you; become TAINTED."
        , Seq [curioItem, Test Will 0 onward tainted]
        )
      ,
        ( FerryTerminal
        , "You may move one space or move to another ferry terminal. The weather is finally pleasant and you spend the journey relaxing in a deck chair, drifting in and out of sleep. When you arrive, you feel better rested than you have in weeks. You may spend one focus to recover two sanity."
        , Seq [onward, mayPay (SpendFocus 1) (mySanity 2)]
        )
      ,
        ( TrainPlatform
        , "Before you get on the train, you suspect someone is following you. You hope to lose this pursuer in the station before boarding (observation). If you pass, you successfully depart without being followed; you may move one space or move to another train platform. If you fail, you feel this presence constantly; become TAINTED."
        , Test Observation 0 onward tainted
        )
      ]
    )
  ,
    ( 8
    ,
      [
        ( CountryRoad
        , "You may move one space or move to another country road. As you cross a covered bridge, the air turns sour. You stop and study the structure (observation). If you pass, you find a rune carved into the wood; gain one remnant. If you fail, the world seems darker on the other side; become TAINTED."
        , Seq [onward, Test Observation 0 (remnants 1) tainted]
        )
      ,
        ( FerryTerminal
        , "A storm threatens to capsize the boat and the passengers are terrified (will). If you pass, you calm the crowd and wait out the storm; you may focus two skills of your choice. If you fail, you give in to fear; suffer one horror. Once the storm passes, your damaged craft limps to your destination; you may move one space or move to another ferry terminal."
        , Seq [Test Will 0 (Seq [focusAny, focusAny]) (horror 1), onward]
        )
      ,
        ( TrainPlatform
        , "The train is crowded, but it runs perfectly on schedule. You may move one space or move to another train platform. On the way, your fellow passengers are curious about your struggles and are particularly excited to see any actual proof of the supernatural. You may spend one remnant to gain $2."
        , Seq [onward, mayPay (SpendRemnants 1) (money 2)]
        )
      ]
    )
  ]
