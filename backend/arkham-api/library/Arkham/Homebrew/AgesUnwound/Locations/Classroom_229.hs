module Arkham.Homebrew.AgesUnwound.Locations.Classroom_229 (classroom_229) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n, getStandardActions)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype Classroom_229 = Classroom_229 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | /Energised and Unstable/. One of three distinct Classrooms, all with symbol T.
classroom_229 :: LocationCard Classroom_229
classroom_229 = setLabel "classroom3" $ location Classroom_229 Cards.classroom_229 2 (PerPlayer 1)

{- | "Haunted - For each standard action you have remaining, either lose 1 action
or take 1 direct damage."
-}
instance HasAbilities Classroom_229 where
  getAbilities (Classroom_229 a) =
    extendRevealed1 a $ campaignI18n $ hauntedI "classroomEnergisedAndUnstable.haunted" a 1

instance RunMessage Classroom_229 where
  runMessage msg l@(Classroom_229 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- the count is fixed here, before the first action is given up
      n <- getStandardActions iid
      doStep n msg
      pure l
    DoStep n (UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      canLose <- (> 0) <$> getStandardActions iid
      chooseOrRunOneM iid $ withI18n do
        when canLose
          $ countVar 1
          $ labeled "loseActions"
          $ loseStandardActions iid (attrs.ability 1) 1
        countVar 1 $ labeled "takeDirectDamage" $ directDamageAndHorror iid (attrs.ability 1) 1 0
      doNextStep msg
      pure l
    _ -> Classroom_229 <$> liftRunMessage msg attrs
