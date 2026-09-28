module Arkham.Homebrew.CircusExMortis.Locations.UpperBalcony (upperBalcony) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestTargetedEnemy, isParley)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Projection

newtype UpperBalcony = UpperBalcony LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

upperBalcony :: LocationCard UpperBalcony
upperBalcony = location UpperBalcony Cards.upperBalcony 2 (PerPlayer 1)

instance HasModifiersFor UpperBalcony where
  getModifiersFor (UpperBalcony a) =
    whenJustM getSkillTestInvestigator \iid -> maybeModified_ a iid do
      liftGuardM isParley
      liftGuardM $ iid <=~> investigatorAt a
      handSize <- lift $ fieldMap InvestigatorHand length iid
      guard $ handSize >= 4
      pure [AnySkillValue $ handSize `div` 4]

instance HasAbilities UpperBalcony where
  getAbilities (UpperBalcony a) =
    extendRevealed1 a
      $ restricted a 1 Here
      $ freeReaction
      $ SkillTestResult #after You (WhileEvadingAnEnemy $ enemyAt a) (SuccessResult $ atLeast 2)

instance RunMessage UpperBalcony where
  runMessage msg l@(UpperBalcony attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenJustM getSkillTestTargetedEnemy \eid -> defeatEnemy eid iid (attrs.ability 1)
      pure l
    _ -> UpperBalcony <$> liftRunMessage msg attrs
