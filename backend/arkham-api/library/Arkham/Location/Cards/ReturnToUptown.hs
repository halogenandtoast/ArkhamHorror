module Arkham.Location.Cards.ReturnToUptown (returnToUptown) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype ReturnToUptown = ReturnToUptown LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToUptown :: LocationCard ReturnToUptown
returnToUptown = location ReturnToUptown Cards.returnToUptown 2 (Static 0)

instance HasAbilities ReturnToUptown where
  getAbilities (ReturnToUptown a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a (LocationWithBreaches $ atLeast 1)) actionAbility

instance RunMessage ReturnToUptown where
  runMessage msg l@(ReturnToUptown attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scenarioI18n
        $ chooseAmount' iid "breachesToRemove" "$breaches" 0 (countLocationBreaches attrs) attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$breaches" -> breaches) (isTarget attrs -> True) -> do
      sid <- getRandom
      act <- selectJust AnyAct
      removeBreaches attrs breaches
      placeBreaches act breaches
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed breaches)
      pure l
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      assignDamage iid (attrs.ability 1) n
      pure l
    _ -> ReturnToUptown <$> liftRunMessage msg attrs
