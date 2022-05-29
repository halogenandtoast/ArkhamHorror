module Arkham.Scenario.Scenarios.ThePallidMask
  ( ThePallidMask(..)
  , thePallidMask
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Difficulty
import Arkham.Label
import Arkham.Id
import Arkham.Investigator.Attrs ( InvestigatorAttrs, Field(..) )
import Arkham.Scenario.Attrs
import Arkham.Scenario.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Token

newtype ThePallidMask = ThePallidMask ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Locations are placed directional, printed on the cards the following
-- directions are possible:
--
-- Left: 2
-- Right: 11
-- Above: 5
-- Below: 6
--
-- This means that our starting position is 2,6 and we have a grid that is
-- 13 x 11, in order to convert we use labels to figure out the new position

thePallidMask :: Difficulty -> ThePallidMask
thePallidMask difficulty = ThePallidMask $
  baseAttrs "03240" "The Pallid Mask" difficulty
    & locationLayoutL
    ?~ [ "pos0011 pos0111 pos0211 pos0311 pos0411 pos0511 pos0611 pos0711 pos0811 pos0911 pos1011 pos1111 pos1211 pos1311"
       , "pos0010 pos0110 pos0210 pos0310 pos0410 pos0510 pos0610 pos0710 pos0810 pos0910 pos1010 pos1110 pos1210 pos1310"
       , "pos0009 pos0109 pos0209 pos0309 pos0409 pos0509 pos0609 pos0709 pos0809 pos0909 pos1009 pos1109 pos1209 pos1309"
       , "pos0008 pos0108 pos0208 pos0308 pos0408 pos0508 pos0608 pos0708 pos0808 pos0908 pos1008 pos1108 pos1208 pos1308"
       , "pos0007 pos0107 pos0207 pos0307 pos0407 pos0507 pos0607 pos0707 pos0807 pos0907 pos1007 pos1107 pos1207 pos1307"
       , "pos0006 pos0106 pos0206 pos0306 pos0406 pos0506 pos0606 pos0706 pos0806 pos0906 pos1006 pos1106 pos1206 pos1306"
       , "pos0005 pos0105 pos0205 pos0305 pos0405 pos0505 pos0605 pos0705 pos0805 pos0905 pos1005 pos1105 pos1205 pos1305"
       , "pos0004 pos0104 pos0204 pos0304 pos0404 pos0504 pos0604 pos0704 pos0804 pos0904 pos1004 pos1104 pos1204 pos1304"
       , "pos0003 pos0103 pos0203 pos0303 pos0403 pos0503 pos0603 pos0703 pos0803 pos0903 pos1003 pos1103 pos1203 pos1303"
       , "pos0002 pos0102 pos0202 pos0302 pos0402 pos0502 pos0602 pos0702 pos0802 pos0902 pos1002 pos1102 pos1202 pos1302"
       , "pos0001 pos0101 pos0201 pos0301 pos0401 pos0501 pos0601 pos0701 pos0801 pos0901 pos1001 pos1101 pos1201 pos1301"
       , "pos0000 pos0100 pos0200 pos0300 pos0400 pos0500 pos0600 pos0700 pos0800 pos0900 pos1000 pos1100 pos1200 pos1300"
       ]

posLabelToPosition :: Label -> (Int, Int)
posLabelToPosition lbl = case drop 3 (unpack . unLabel $ lbl) of
  (x10 : x1 : y10 : y1 : []) -> (toI x10 * 10 + toI x1, toI y10 * 10 + toI y1)
  _ -> error "Invalid position label"
 where
   toI = \case
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    _ -> error "not a digit"

positionToLabel :: (Int, Int) -> Label
positionToLabel (x, y) = Label . pack $ "pos" <> fromI x <> fromI y
  where
    fromI n | n < 10 = "0" <> show n
            | otherwise = show n

startPosition :: (Int, Int)
startPosition = (2, 6)

instance HasRecord env ThePallidMask where
  hasRecord _ _ = pure False
  hasRecordSet _ _ = pure []
  hasRecordCount _ _ = pure 0

instance (Query LocationMatcher env, Projection env InvestigatorAttrs, HasCount (Maybe Distance) env (LocationId, LocationId), HasTokenValue env InvestigatorId) => HasTokenValue env ThePallidMask where
  getTokenValue iid tokenFace (ThePallidMask attrs) = case tokenFace of
    Skull -> do
      -- -X where X is the number of locations away from the starting location
      startingLocation <- selectJust $ LocationWithLabel . unLabel $ positionToLabel startPosition
      yourLocation <- fromJustNote "no location" <$> field InvestigatorLocation iid
      distance <- unDistance . fromJustNote "no distance?" <$> getDistance startingLocation yourLocation
      pure $ toTokenValue attrs Skull (min 5 distance) distance
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

instance ScenarioRunner env => RunMessage env ThePallidMask where
  runMessage msg (ThePallidMask attrs) =
    ThePallidMask <$> runMessage msg attrs
