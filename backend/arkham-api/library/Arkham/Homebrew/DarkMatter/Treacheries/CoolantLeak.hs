module Arkham.Homebrew.DarkMatter.Treacheries.CoolantLeak (coolantLeak) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Treachery.Import.Lifted

newtype CoolantLeak = CoolantLeak TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

coolantLeak :: TreacheryCard CoolantLeak
coolantLeak = treachery CoolantLeak Cards.coolantLeak

instance RunMessage CoolantLeak where
  runMessage msg t@(CoolantLeak attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasEvaSuit <- selectAny $ assetIs Assets.evaSuit <> assetControlledBy iid
      sid <- getRandom
      when hasEvaSuit $ skillTestModifier sid attrs sid SkillTestAutomaticallySucceeds
      revelationSkillTest sid iid attrs #agility (Fixed 4)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n (FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      ok <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      chooseOneM iid $ withI18n do
        countVar 1 $ labeledValidate' ok "discardCardsFromHand" $ chooseAndDiscardCard iid attrs
        countVar 1 $ labeled "takeDamage" $ assignDamage iid attrs 1
      pure t
    _ -> CoolantLeak <$> liftRunMessage msg attrs
