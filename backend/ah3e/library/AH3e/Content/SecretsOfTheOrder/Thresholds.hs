{- | The threshold deck. A threshold card prints one encounter per threshold type,
the way a travel route card prints one per route type, and every wild gateway
offers to carry you onward (the @gateway-onward@ effect).
-}
module AH3e.Content.SecretsOfTheOrder.Thresholds (cards) where

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
cards = fromBox SecretsOfTheOrder (map (uncurry card) thresholds)

card :: Int -> [(ThresholdType, Text, Effect)] -> CardDef
card n encounters =
  CardDef
    { code = CardCode ("threshold-" <> T.justifyRight 2 '0' (tshow n))
    , name = "Threshold " <> tshow n <> "/6"
    , expansion = CoreSet
    , copies = 1
    , kind = ThresholdCard (Map.fromList [(tt, Encounter txt eff) | (tt, txt, eff) <- encounters])
    }

-- | "You may move one space or to another wild gateway."
onward :: Effect
onward = Custom "gateway-onward"

-- | "Remove one doom from any space", which only offers the spaces holding any.
anywhere :: Int -> Effect
anywhere n = RemoveDoomFrom AnySpace (N n)

thresholds :: [(Int, [(ThresholdType, Text, Effect)])]
thresholds =
  [
    ( 1
    ,
      [
        ( DerelictPortal
        , "Near the shimmering part in the veil between worlds, you see a small creature that you mistake for an opossum before you spot its wriggling tentacles. The zoog clutches a leather wallet in its forepaws and looks at you quizzically. You may spend one remnant to feed it and gain $2."
        , mayPay (SpendRemnants 1) (money 2)
        )
      ,
        ( HiddenPath
        , "A dry wind from a distant place carries the whispers of the forgotten dead (will). If you pass, the voices guide you to the remains of a doomed traveler; gain one common item. If you fail, the baleful sirocco leaves you parched and drained; suffer one damage and become FATIGUED."
        , Test Will 0 commonItem (Seq [damage 1, fatigued])
        )
      ,
        ( WildGateway
        , "You may move one space or to another wild gateway. Another soul who traveled this way has left a small stone shrine in tribute to a minor spirit. You think the proper offering might ease your journey (lore). If you pass, you recite a simple poem and hear laughing from behind you; you or an ally may recover one health and one sanity."
        , Seq [onward, pass Lore 0 (RecoverBoth YouOrAlly (N 1) (N 1))]
        )
      ]
    )
  ,
    ( 2
    ,
      [
        ( DerelictPortal
        , "An old woman sags under the weight of a lifetime of worldly belongings. You spot something useful and ask her to part with it (influence). If you pass, gain one common item. If you fail, you realize that this fate may await anyone who clings too tightly to their possessions; suffer one horror."
        , Test Influence 0 commonItem (horror 1)
        )
      ,
        ( HiddenPath
        , "The twisting path wends its way through a hollow of human-visaged stones and grasping vines. When the path forks, you search carefully to determine the shorter path to your goal (observation). If you pass, a stone that reminds you of your grandmother points the way; you may move two spaces. If you fail, you wander aimlessly before you find your way out."
        , pass Observation 0 (MoveUpTo 2)
        )
      ,
        ( WildGateway
        , "A rippling doorway in the air leads you into a trading post in a desert of black sand. The merchants' wares catch your eye, but they have no interest in your money. You may spend one remnant to gain one curio. Whether or not you buy something, you may move one space or to another wild gateway."
        , Seq [mayPay (SpendRemnants 1) curioItem, onward]
        )
      ]
    )
  ,
    ( 3
    ,
      [
        ( DerelictPortal
        , "It doesn't look like the young woman selling candied nuts on the corner has even noticed the iridescent doorway hanging in the air behind her. You may spend $1 for you or an ally to recover one health. You consider telling her to move her cart, but she won't look up from her crossword puzzle."
        , mayPay (SpendMoney 1) (health 1)
        )
      ,
        ( HiddenPath
        , "The inn at the crossroads is a quaint and quiet place that feels almost like New England. The menu is simple and the company is pleasant, so you tarry a while. You may spend one remnant for you or an ally to recover one health and one sanity. When the fire burns low, you reluctantly return to the task at hand and leave this haven behind."
        , mayPay (SpendRemnants 1) (RecoverBoth YouOrAlly (N 1) (N 1))
        )
      ,
        ( WildGateway
        , "You see yourself vanish around a corner up ahead (lore). If you pass, you realize that you have doubled back on yesterday and catch up on your to-do list; remove one doom from any space. If you fail, you fear you have been replaced; suffer one horror. Whether you pass or not, you may move one space or to another wild gateway."
        , Seq [Test Lore 0 (anywhere 1) (horror 1), onward]
        )
      ]
    )
  ,
    ( 4
    ,
      [
        ( DerelictPortal
        , "An inhuman silhouette appears in the rippling veil before you. Seeing an opportunity to stop the beast before it enters your world, you lie in wait (will). If you pass, you get the jump on the creature and dispatch it easily; gain one remnant. If you fail, you freeze when you see it fully; spawn one monster."
        , Test Will 0 (remnants 1) SpawnMonster
        )
      ,
        ( HiddenPath
        , "The path before you shivers expectantly, and something unseen kicks up a flurry of dried leaves (lore). If you pass, you use the light of your flashlight to keep the presence at bay and hurry past; you may move one space. If you fail, the rustling leaves swirl around you and whisk you away; move directly to the unstable space."
        , Test Lore 0 (MoveUpTo 1) (MoveDirectlyTo TheUnstableSpace)
        )
      ,
        ( WildGateway
        , "The blazing hole in the air before you flickers and spits like a guttering candle. You'll need to pick your moment carefully, or you may not be able to travel through the portal (observation). If you pass, you successfully leap through while it is open; you may move one space or to another wild gateway."
        , pass Observation 0 onward
        )
      ]
    )
  ,
    ( 5
    ,
      [
        ( DerelictPortal
        , "A trio of urchins picks through a small pile of detritus that they claim fell from this doorway when it opened. You try to convince them that this place is dangerous (influence). If you pass, they believe you, and give you something they found; gain one curio. If you fail, they mock you cruelly."
        , pass Influence 0 curioItem
        )
      ,
        ( HiddenPath
        , "The shade of a wanderer, long waylaid in their travels, waits glumly along your path. \"Only a gift freely given can untether me from this place.\" You may spend one remnant to free them. If you do, they open your mind to new philosophies; you may focus one skill of your choice, even if it exceeds your focus limit."
        , mayPay (SpendRemnants 1) focusExceed
        )
      ,
        ( WildGateway
        , "The wild flares of the doorway before you can only be quieted with an offering of spirit or body, but you know innately that your own will not suffice. You may spend one remnant or choose another investigator to become FATIGUED. If you do, move one space or to another wild gateway."
        , Custom "wild-gateway-toll"
        )
      ]
    )
  ,
    ( 6
    ,
      [
        ( DerelictPortal
        , "Joey \"the Rat\" glances uncomfortably over his shoulder at the shimmering doorway that has appeared against a worn brick wall. \"If you're headed that way, you might need something to help you out.\" You may buy one common item from the display."
        , buyOne "Common"
        )
      ,
        ( HiddenPath
        , "A rumbling like distant thunder washes over you from the magical portal, as something moves by in the space between worlds. You try to conceal yourself from the passing presence (observation). If you pass, you remain hidden until you have a chance to pick through the wreckage it leaves behind; gain one curio. If you fail, spawn one monster."
        , Test Observation 0 curioItem SpawnMonster
        )
      ,
        ( WildGateway
        , "You may move one space or to another wild gateway. As the ground returns beneath your feet and the lurching in your stomach subsides, you see the traces of a circle drawn at your feet (lore). If you pass, you can tell that something else came this way before you and left something behind; gain one remnant."
        , Seq [onward, pass Lore 0 (remnants 1)]
        )
      ]
    )
  ]
