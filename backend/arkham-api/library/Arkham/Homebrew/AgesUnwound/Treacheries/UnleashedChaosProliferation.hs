module Arkham.Homebrew.AgesUnwound.Treacheries.UnleashedChaosProliferation (
  unleashedChaosProliferation,
) where

import Arkham.Card
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnleashedChaosProliferation = UnleashedChaosProliferation TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unleashedChaosProliferation :: TreacheryCard UnleashedChaosProliferation
unleashedChaosProliferation = treachery UnleashedChaosProliferation Cards.unleashedChaosIProliferationI

{- | "Revelation - Draw a card and gain a resource. Draw the top card of the
encounter deck. Until Unleashed Chaos (Proliferation) leaves play, treat it as an
exact copy of the drawn card and resolve it as if you had just drawn it."

TODO(ages-unwound): nothing in the engine can retarget a treachery's card
definition, so the copy is a freshly generated card of the drawn card's def,
drawn immediately after it. The observable result matches ("the drawn card
resolves twice", and a permanent leaves two of itself in play) except that this
card goes to the encounter discard instead of standing in for the copy.
-}
instance RunMessage UnleashedChaosProliferation where
  runMessage msg t@(UnleashedChaosProliferation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      drawCards iid attrs 1
      gainResources iid attrs 1
      mcard <- headMay <$> getEncounterDeck
      case mcard of
        Nothing -> pure ()
        Just card -> do
          drawEncounterCard iid attrs
          copy <- genEncounterCard (toCardDef card)
          push $ InvestigatorDrewEncounterCard iid copy
      pure t
    _ -> UnleashedChaosProliferation <$> liftRunMessage msg attrs
