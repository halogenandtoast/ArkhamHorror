module Arkham.Event.Cards.EmergencyCache3 (
  emergencyCache3,
  EmergencyCache3 (..),
) where

import Arkham.Prelude

import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher
import Arkham.Message

newtype EmergencyCache3 = EmergencyCache3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache3 :: EventCard EmergencyCache3
emergencyCache3 = event EmergencyCache3 Cards.emergencyCache3

instance RunMessage EmergencyCache3 where
  runMessage msg e@(EmergencyCache3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      supplyAssets <-
        selectList
          $ AssetControlledBy (InvestigatorWithId iid)
          <> AssetWithUses Supply
      if null supplyAssets
        then pushAll [TakeResources iid 4 (toSource attrs) False]
        else do
          pushAll
            $ replicate 4
            $ chooseOne
              iid
              [ Label "Take Resource" [TakeResources iid 1 (toSource attrs) False]
              , Label
                  "Add Supply"
                  [ chooseOrRunOne
                      iid
                      [ targetLabel asset [AddUses asset Supply 1]
                      | asset <- supplyAssets
                      ]
                  ]
              ]
      pure e
    _ -> EmergencyCache3 <$> runMessage msg attrs
