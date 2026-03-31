module Arkham.Event.Events.InfuseLife (infuseLife) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype InfuseLife = InfuseLife EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

infuseLife :: EventCard InfuseLife
infuseLife = event InfuseLife Cards.infuseLife

instance RunMessage InfuseLife where
  runMessage msg e@(InfuseLife attrs) = runQueueT $ case msg of
    PlayThisEvent _iid (is attrs -> True) -> do
      doStep 3 msg
      pure e
    DoStep n msg'@(PlayThisEvent iid (is attrs -> True)) | n > 0 -> do
      assets <- selectTargets $ HealableAsset (toSource attrs) #damage (AllyAsset <> assetAtLocationWith iid)
      investigators <- selectTargets $ HealableInvestigator (toSource attrs) #damage $ colocatedWith iid
      unless (null assets && null investigators) do
        chooseOneM iid do
          labeled "Done healing" nothing
          targets (assets <> investigators) \target -> do
            healDamage target attrs 1
            doStep (n - 1) msg'
      pure e
    _ -> InfuseLife <$> liftRunMessage msg attrs
