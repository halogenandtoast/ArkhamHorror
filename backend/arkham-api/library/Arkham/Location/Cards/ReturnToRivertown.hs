module Arkham.Location.Cards.ReturnToRivertown (returnToRivertown) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype ReturnToRivertown = ReturnToRivertown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToRivertown :: LocationCard ReturnToRivertown
returnToRivertown = location ReturnToRivertown Cards.returnToRivertown 2 (Static 0)

instance HasAbilities ReturnToRivertown where
  getAbilities (ReturnToRivertown a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a (LocationWithBreaches $ atLeast 1)) actionAbility

instance RunMessage ReturnToRivertown where
  runMessage msg l@(ReturnToRivertown attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scenarioI18n
        $ chooseAmount' iid "breachesToRemove" "$breaches" 0 (countLocationBreaches attrs) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$breaches" -> breaches) (isTarget attrs -> True) -> do
      sid <- getRandom
      act <- selectJust AnyAct
      removeBreaches attrs breaches
      placeBreaches act breaches
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed breaches)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      repeated n $ chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> ReturnToRivertown <$> liftRunMessage msg attrs
