module Arkham.Homebrew.AgesUnwound.Events.UnstableEnergies (unstableEnergies) where

import Arkham.Ability
import Arkham.Card
import Arkham.Event.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Events qualified as Cards
import Arkham.Matcher
import Arkham.Modifier

newtype UnstableEnergies = UnstableEnergies EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unstableEnergies :: EventCard UnstableEnergies
unstableEnergies = event UnstableEnergies Cards.unstableEnergies

{- | "Fight. Add your [willpower] to your skill value for this attack. This
attack deals +2 damage. /
Forced - If Unstable Energies is in your hand at the end of your turn: Reveal it
and take 1 damage."
-}
instance HasAbilities UnstableEnergies where
  getAbilities (UnstableEnergies a) =
    [restricted a 1 InYourHand $ forced $ TurnEnds #when You]

instance RunMessage UnstableEnergies where
  runMessage msg e@(UnstableEnergies attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      skillTestModifiers sid attrs iid [AddSkillValue #willpower, DamageDealt 2]
      chooseFightEnemy sid iid attrs
      pure e
    -- it is revealed, not discarded: like Dark Memory it stays in hand
    InHand iid' (UseThisAbility iid (isSource attrs -> True) 1) | iid' == iid -> do
      push $ RevealCard (toCardId attrs)
      assignDamage iid (CardIdSource $ toCardId attrs) 1
      pure e
    _ -> UnstableEnergies <$> liftRunMessage msg attrs
