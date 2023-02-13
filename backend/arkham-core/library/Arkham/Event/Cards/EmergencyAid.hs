module Arkham.Event.Cards.EmergencyAid
  ( emergencyAid
  , EmergencyAid(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Damage
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype EmergencyAid = EmergencyAid EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyAid :: EventCard EmergencyAid
emergencyAid = event EmergencyAid Cards.emergencyAid

instance RunMessage EmergencyAid where
  runMessage msg e@(EmergencyAid attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      iids <- selectList $ colocatedWith iid

      choices <- flip mapMaybeM iids $ \iid' -> do
        healableAllies <-
          selectList
          $ HealableAsset (toSource attrs) DamageType
          $ AllyAsset
          <> assetControlledBy iid'
        healable <- canHaveDamageHealed attrs iid'

        pure $ if healable || notNull healableAllies
          then Just $ targetLabel
            iid'
            [ chooseOrRunOne iid
              $ [ targetLabel iid' [HealDamage (InvestigatorTarget iid') (toSource attrs) 2]
                | healable
                ]
              <> [ targetLabel asset [HealDamage (AssetTarget asset) (toSource attrs) 2]
                 | asset <- healableAllies
                 ]
            ]
          else Nothing

      pushAll [chooseOne iid choices]
      pure e
    _ -> EmergencyAid <$> runMessage msg attrs
