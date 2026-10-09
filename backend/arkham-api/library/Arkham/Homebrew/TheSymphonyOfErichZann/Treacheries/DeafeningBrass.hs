module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.DeafeningBrass (deafeningBrass) where

import Arkham.ChaosToken
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (placeMusicTreachery)
import Arkham.Matcher
import Arkham.Treachery.Import.Lifted

newtype DeafeningBrass = DeafeningBrass TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deafeningBrass :: TreacheryCard DeafeningBrass
deafeningBrass = treachery DeafeningBrass Cards.deafeningBrass

instance HasModifiersFor DeafeningBrass where
  -- "Treat each '+1', '0', and '-1' token revealed as a [skull] token instead."
  -- On the tokens, not on the investigator: getModifiedChaosTokenFace reads
  -- ForcedChaosTokenChange off ChaosTokenTarget, and an investigator-target copy
  -- only feeds the client's display.
  getModifiersFor (DeafeningBrass a) =
    modifySelect
      a
      (ChaosTokenRevealedBy Anyone)
      [ForcedChaosTokenChange face [Skull] | face <- [PlusOne, Zero, MinusOne]]

instance RunMessage DeafeningBrass where
  runMessage msg t@(DeafeningBrass attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      placeMusicTreachery attrs
      pure t
    _ -> DeafeningBrass <$> liftRunMessage msg attrs
