module Arkham.Homebrew.CircusExMortis.Locations.TheBigTopThirdRing (theBigTopThirdRing) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (bigTopRings, campaignI18n)
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype TheBigTopThirdRing = TheBigTopThirdRing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBigTopThirdRing :: LocationCard TheBigTopThirdRing
theBigTopThirdRing =
  location TheBigTopThirdRing Cards.theBigTopThirdRing 3 (PerPlayer 1)
    & setLabel "theBigTopThirdRing"

instance HasModifiersFor TheBigTopThirdRing where
  getModifiersFor (TheBigTopThirdRing a) =
    modifySelect a (investigatorAt a.id) [MovingToDoesNotProvokeAttacksOfOpportunity bigTopRings]

instance HasAbilities TheBigTopThirdRing where
  getAbilities (TheBigTopThirdRing a) =
    extendRevealed1 a
      $ playerLimit PerRound
      $ restricted a 1 Here
      $ freeReaction (SkillTestResult #after You AnySkillTest (FailureResult $ EqualTo $ Static 1))

instance RunMessage TheBigTopThirdRing where
  runMessage msg l@(TheBigTopThirdRing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      connected <- select $ accessibleFrom ForMovement (toId attrs)
      canEvade <- selectAny $ enemyCanBeEvadedBy (attrs.ability 1)
      -- an immediate free move/evade rather than a granted extra action: the
      -- AdditionalAction machinery has no combined move-or-evade type
      chooseOneM iid $ campaignI18n $ scope "theBigTopThirdRing" do
        labeledValidate' (notNull connected) "takeMove" do
          chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
        labeledValidate' canEvade "takeEvade" $ chooseEvadeEnemy sid iid (attrs.ability 1)
      pure l
    _ -> TheBigTopThirdRing <$> liftRunMessage msg attrs
