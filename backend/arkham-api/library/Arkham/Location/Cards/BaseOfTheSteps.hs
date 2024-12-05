module Arkham.Location.Cards.BaseOfTheSteps (baseOfTheSteps, BaseOfTheSteps (..)) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (discardCard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype BaseOfTheSteps = BaseOfTheSteps LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseOfTheSteps :: LocationCard BaseOfTheSteps
baseOfTheSteps = location BaseOfTheSteps Cards.baseOfTheSteps 3 (PerPlayer 1)

instance HasModifiersFor BaseOfTheSteps where
  getModifiersFor (BaseOfTheSteps a) = whenUnrevealed a $ maybeModifySelf a do
    liftGuardM $ selectAny $ locationIs Cards.sevenHundredSteps <> LocationWithAnyClues
    pure [Blocked]

instance HasAbilities BaseOfTheSteps where
  getAbilities (BaseOfTheSteps a) =
    extendRevealed1 a
      $ skillTestAbility
      $ forcedAbility a 1
      $ Enters #after (You <> HandWith (LengthIs $ atLeast 1)) (be a)

instance RunMessage BaseOfTheSteps where
  runMessage msg l@(BaseOfTheSteps attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (InvestigatorHandLengthCalculation iid)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      hand <- field InvestigatorHand iid
      for_ hand \card -> do
        focusCards [card] \unfocus -> do
          chooseOneM iid do
            labeled "Discard" $ discardCard iid (attrs.ability 1) card
            labeled "Take 1 horror" $ assignHorror iid (attrs.ability 1) 1
          push unfocus
      pure l
    _ -> BaseOfTheSteps <$> liftRunMessage msg attrs
