module Arkham.Treachery.Cards.ThePaleMaskBeckons (thePaleMaskBeckons) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Card
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ThePaleMaskBeckons = ThePaleMaskBeckons TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaleMaskBeckons :: TreacheryCard ThePaleMaskBeckons
thePaleMaskBeckons = treachery ThePaleMaskBeckons Cards.thePaleMaskBeckons

instance RunMessage ThePaleMaskBeckons where
  runMessage msg t@(ThePaleMaskBeckons attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (enemyIs Cards.theManInThePallidMask) >>= \case
        Just enemy -> eachInvestigator (initiateEnemyAttack enemy attrs)
        Nothing -> do
          enemy <- fetchCard Cards.theManInThePallidMask
          obtainCard enemy
          withOwner Cards.theManInThePallidMask shuffleDeck
          push $ DrewPlayerEnemy iid enemy
      pure t
    _ -> ThePaleMaskBeckons <$> liftRunMessage msg attrs
