module Arkham.Location.Cards.TheScarletKeys.RiddlesAndRain.BigBen (bigBen) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.SkillTest.Target
import Arkham.Location.CardDefs.TheScarletKeys.RiddlesAndRain qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype BigBen = BigBen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bigBen :: LocationCard BigBen
bigBen = symbolLabel $ location BigBen Cards.bigBen 4 (PerPlayer 1)

instance HasAbilities BigBen where
  getAbilities (BigBen a) =
    extendRevealed1 a
      $ playerLimit PerTurn
      $ skillTestAbility
      $ restricted a 1 (DuringTurn Anyone <> Here <> exists (orConnected_ a.match <> WithConcealed))
      $ FastAbility Free

instance RunMessage BigBen where
  runMessage msg l@(BigBen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ orConnected_ attrs.match <> WithConcealed
      chooseHandleTargetM iid (attrs.ability 1) locations
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      sid <- getRandom
      cs <- toId <$$> getConcealedAtForExpose attrs lid
      chooseOrRunTargetM iid cs \c -> beginSkillTest sid iid (attrs.ability 1) c #agility (Fixed 2)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenJustM getSkillTestTarget \case
        ConcealedCardTarget card -> revealConcealed iid (attrs.ability 1) card
        _ -> pure ()
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignHorror iid attrs 1
      pure l
    _ -> BigBen <$> liftRunMessage msg attrs
