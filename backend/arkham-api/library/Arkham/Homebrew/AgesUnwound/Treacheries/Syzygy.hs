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

The action loss is 'loseStandardActions', the campaign's rule for "loses 1
action": it spends remaining actions first and then only takes additional actions
without a limitation on their use.
-}
instance RunMessage Syzygy where
  runMessage msg t@(Syzygy attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ LocationWithTrait Past) \iid ->
        loseStandardActions iid attrs 1
      selectEach (InvestigatorAt $ LocationWithTrait Present) \iid -> do
        assignDamage iid attrs 1
        assignHorror iid attrs 1
      selectEach (InvestigatorAt $ LocationWithTrait Future) \iid -> do
        gainActions iid attrs 1
        drawEncounterCard iid attrs
      pure t
    _ -> Syzygy <$> liftRunMessage msg attrs
