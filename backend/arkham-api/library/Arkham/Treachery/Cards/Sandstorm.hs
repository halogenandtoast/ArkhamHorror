module Arkham.Treachery.Cards.Sandstorm (sandstorm) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhenM)
import Arkham.I18n
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Desert))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Sandstorm = Sandstorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sandstorm :: TreacheryCard Sandstorm
sandstorm = treachery Sandstorm Cards.sandstorm

instance HasModifiersFor Sandstorm where
  getModifiersFor (Sandstorm attrs) = do
    modifySelfWhenM
      attrs
      (selectAny $ locationWithInvestigator attrs.drawnBy <> LocationWithTrait Desert)
      [AddKeyword Keyword.Peril]

instance RunMessage Sandstorm where
  runMessage msg t@(Sandstorm attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ DiscardableAsset <> AssetControlledBy (InvestigatorWithId iid)
      chooseOrRunOneM iid do
        withI18n $ countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
        targets assets (toDiscardBy iid attrs)
      pure t
    _ -> Sandstorm <$> liftRunMessage msg attrs
