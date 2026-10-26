module Arkham.Homebrew.AgesUnwound.Treacheries.Syzygy (syzygy) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Future, Past, Present))
import Arkham.Treachery.Import.Lifted

newtype Syzygy = Syzygy TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

syzygy :: TreacheryCard Syzygy
syzygy = treachery Syzygy Cards.syzygy

{- | "__Revelation__ - Each investigator at a [[Past]] location loses 1 action.
Each investigator at a [[Present]] location takes 1 damage and 1 horror. Each
investigator at a [[Future]] location gains 1 action and draws the top card of the
encounter deck."

The action loss is 'loseActions': remaining actions go first, and an
investigator left with only additional actions loses those, whatever their
limitation (FAQ 1.10). The campaign's /standard action/ term governs the effects
that *count* actions, not the ones that take them away.
-}
instance RunMessage Syzygy where
  runMessage msg t@(Syzygy attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ LocationWithTrait Past) \iid ->
        loseActions iid attrs 1
      selectEach (InvestigatorAt $ LocationWithTrait Present) \iid -> do
        assignDamage iid attrs 1
        assignHorror iid attrs 1
      selectEach (InvestigatorAt $ LocationWithTrait Future) \iid -> do
        gainActions iid attrs 1
        drawEncounterCard iid attrs
      pure t
    _ -> Syzygy <$> liftRunMessage msg attrs
