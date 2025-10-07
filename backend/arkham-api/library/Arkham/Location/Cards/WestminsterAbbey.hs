module Arkham.Location.Cards.WestminsterAbbey (westminsterAbbey) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Helpers.SkillTest.Target (getSkillTestTarget)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher.Location
import Arkham.Message.Lifted.Choose

newtype WestminsterAbbey = WestminsterAbbey LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westminsterAbbey :: LocationCard WestminsterAbbey
westminsterAbbey = symbolLabel $ location WestminsterAbbey Cards.westminsterAbbey 1 (PerPlayer 1)

instance HasAbilities WestminsterAbbey where
  getAbilities (WestminsterAbbey a) =
    extendRevealed1 a $ skillTestAbility $ groupLimit PerGame $ restricted a 1 Here parleyAction_

instance RunMessage WestminsterAbbey where
  runMessage msg l@(WestminsterAbbey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select Anywhere
      sid <- getRandom
      chooseTargetM iid locations \lid -> do
        beginSkillTest sid iid (attrs.ability 1) lid #willpower (Fixed 1)
      pure l
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      whenJustM getSkillTestTarget \case
        LocationTarget lid -> do
          concealed <- map toId <$> getConcealedAtAll lid
          chooseNM iid n $ targets concealed $ revealConcealed iid (attrs.ability 1)
        _ -> pure ()
      pure l
    _ -> WestminsterAbbey <$> liftRunMessage msg attrs
