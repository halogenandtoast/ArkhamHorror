module Arkham.Helpers.Healing where

import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Prelude
import Arkham.Source

chooseHealDamageOrHorror
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
chooseHealDamageOrHorror source iid = do
  chooseOrRunOneM iid do
    whenM (canHaveDamageHealed source iid) do
      damageLabeled iid do
        healDamage iid source 1
    whenM (canHaveHorrorHealed source iid) do
      horrorLabeled iid do
        healHorror iid source 1
