module Arkham.Homebrew.CircusExMortis.Locations.TheBigTopThirdRing (theBigTopThirdRing) where

import Arkham.Ability
import Arkham.ForMovement
import Arkham.GameValue
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

-- "Moving between The Big Top locations does not provoke attacks of opportunity."
-- The engine never provokes attacks of opportunity on a move action, so this clause
-- is already satisfied; nothing to implement. See report note on the Big Top grouping.

newtype TheBigTopThirdRing = TheBigTopThirdRing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBigTopThirdRing :: LocationCard TheBigTopThirdRing
theBigTopThirdRing =
  location TheBigTopThirdRing Cards.theBigTopThirdRing 3 (PerPlayer 1)
    & setLabel "theBigTopThirdRing"

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
      -- TODO(homebrew): modeled as an immediate free move/evade rather than a granted
      -- extra action; the AdditionalAction machinery has no combined move-or-evade type.
      chooseOneM iid $ campaignI18n do
        when (notNull connected) do
          labeled' "theBigTopThirdRing.takeMove" do
            chooseTargetM iid connected $ moveTo (attrs.ability 1) iid
        when canEvade do
          labeled' "theBigTopThirdRing.takeEvade" $ chooseEvadeEnemy sid iid (attrs.ability 1)
      pure l
    _ -> TheBigTopThirdRing <$> liftRunMessage msg attrs
