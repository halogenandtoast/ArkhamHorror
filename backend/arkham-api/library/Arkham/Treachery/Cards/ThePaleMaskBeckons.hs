module Arkham.Treachery.Cards.ThePaleMaskBeckons (thePaleMaskBeckons) where

import Arkham.Card
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
          enemy <- getCampaignStoryCard Cards.theManInThePallidMask
          pushAll
            [ RemoveFromBearersDeckOrDiscard enemy
            , DrewPlayerEnemy iid (PlayerCard enemy)
            ]
      pure t
    _ -> ThePaleMaskBeckons <$> liftRunMessage msg attrs
