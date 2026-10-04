module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.EtherealMelody (etherealMelody) where

import Arkham.Ability
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (placeMusicTreachery)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype EtherealMelody = EtherealMelody TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealMelody :: TreacheryCard EtherealMelody
etherealMelody = treachery EtherealMelody Cards.etherealMelody

instance HasAbilities EtherealMelody where
  -- "After you perform the same type of action twice in a row: Take 1 damage."
  getAbilities (EtherealMelody a) =
    [mkAbility a 1 $ forced $ PerformedSameTypeOfAction #after Anyone AnyAction]

instance RunMessage EtherealMelody where
  runMessage msg t@(EtherealMelody attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeMusicTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure t
    _ -> EtherealMelody <$> liftRunMessage msg attrs
