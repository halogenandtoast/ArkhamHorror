module Arkham.Treachery.Cards.FurtiveLocals (furtiveLocals, FurtiveLocals (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (campaignI18n)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FurtiveLocals = FurtiveLocals TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

furtiveLocals :: TreacheryCard FurtiveLocals
furtiveLocals = treachery FurtiveLocals Cards.furtiveLocals

instance HasModifiersFor FurtiveLocals where
  getModifiersFor (FurtiveLocals a) = modifySelect a Anyone [CannotPerformAction #parley]

instance HasAbilities FurtiveLocals where
  getAbilities (FurtiveLocals a) =
    [ limitedAbility (MaxPer Cards.furtiveLocals PerRound 1) $ mkAbility a 1 $ forced $ RoundEnds #when
    ]

instance RunMessage FurtiveLocals where
  runMessage msg t@(FurtiveLocals attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid do
        withI18n $ countVar 1 $ labeledI "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        campaignI18n $ scope "furtiveLocals" $ labeled' "putIntoPlay"
          $ placeTreachery attrs NextToAgenda
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> FurtiveLocals <$> liftRunMessage msg attrs
