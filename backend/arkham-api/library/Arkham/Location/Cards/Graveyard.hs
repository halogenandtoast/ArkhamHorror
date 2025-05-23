module Arkham.Location.Cards.Graveyard (graveyard) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Helpers.Query (getJustLocationByName)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards (graveyard)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Name

newtype Graveyard = Graveyard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graveyard :: LocationCard Graveyard
graveyard = location Graveyard Cards.graveyard 1 (PerPlayer 2)

instance HasAbilities Graveyard where
  getAbilities (Graveyard x) = extendRevealed1 x $ skillTestAbility $ mkAbility x 1 $ forced $ Enters #after You (be x)

instance RunMessage Graveyard where
  runMessage msg l@(Graveyard attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      rivertown <- getJustLocationByName "Rivertown"
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 2
        whenM (getCanMoveTo iid attrs rivertown) do
          nameVar (mkName "Rivertown") $ labeled' "moveTo" $ moveTo (attrs.ability 1) iid rivertown
      pure l
    _ -> Graveyard <$> liftRunMessage msg attrs
