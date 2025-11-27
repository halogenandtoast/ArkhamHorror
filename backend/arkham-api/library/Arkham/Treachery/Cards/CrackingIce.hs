module Arkham.Treachery.Cards.CrackingIce (crackingIce) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CrackingIce = CrackingIce TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crackingIce :: TreacheryCard CrackingIce
crackingIce = treachery CrackingIce Cards.crackingIce

instance RunMessage CrackingIce where
  runMessage msg t@(CrackingIce attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      doStep 1 msg
      pure t
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      crackingIces <- select $ treacheryIs Cards.crackingIce
      when (length crackingIces >= 3) $ do
        eachInvestigator \iid -> do
          sid <- getRandom
          revelationSkillTest sid iid attrs #agility (Fixed 4)
        for_ crackingIces $ toDiscard attrs
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      chooseOneM iid $ withI18n $ countVar 1 $ do
        labeled' "takeDamage" $ assignDamage iid attrs 1
        labeled' "takeHorror" $ assignHorror iid attrs 1
        labeled' "loseActions" $ loseActions iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> CrackingIce <$> liftRunMessage msg attrs
