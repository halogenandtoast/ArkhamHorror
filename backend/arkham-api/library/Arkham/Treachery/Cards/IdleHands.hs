module Arkham.Treachery.Cards.IdleHands (idleHands) where

import Arkham.Ability
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IdleHands = IdleHands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Normally we would not set waiting here, but it is safe for this card and we
-- want it to be marked waiting because the first ability has discard in the
-- cost
idleHands :: TreacheryCard IdleHands
idleHands = treacheryWith IdleHands Cards.idleHands (waitingL .~ True)

instance HasAbilities IdleHands where
  getAbilities (IdleHands attrs) =
    [ restricted attrs 1 (InYourThreatArea <> DuringTurn You)
        $ FastAbility (DiscardCost FromPlay (toTarget attrs) <> DamageCost (attrs.ability 1) YouTarget 2)
    , restricted attrs 2 InYourThreatArea $ forced $ TurnEnds #when You
    ]

instance RunMessage IdleHands where
  runMessage msg t@(IdleHands attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainActions iid (attrs.ability 1) 1
      pure $ overAttrs (waitingL .~ False) t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid attrs 1
      pure t
    Do (AfterRevelation _ tid) | tid == attrs.id -> pure t
    _ -> IdleHands <$> liftRunMessage msg attrs
