module Arkham.Agenda.Cards.TracesToNowhere.IntoThePit (intoThePit) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.CardDefs.TracesToNowhere qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Window (spawnedEnemy)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Projection
import Arkham.Trait (Trait (Otherworld, Syndicate))

newtype IntoThePit = IntoThePit AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoThePit :: AgendaCard IntoThePit
intoThePit = agenda (4, A) IntoThePit Cards.intoThePit (Static 8)

instance HasAbilities IntoThePit where
  getAbilities (IntoThePit a) =
    [ mkAbility a 1 $ forced $ PlacedDoomCounter #after AnySource (targetIs a)
    , mkAbility a 2
        $ forced
        $ EnemySpawns #after (PlacementAt $ LocationWithTrait Otherworld) (EnemyWithTrait Syndicate)
    ]

instance RunMessage IntoThePit where
  runMessage msg a@(IntoThePit attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      shambler <- selectJust $ enemyIs Enemies.dimensionalShamblerDraggingThemBelow
      withLocationOf shambler \lid -> do
        cards <- field LocationCardsUnderneath lid
        for_ (nonEmpty cards) \xs -> do
          card <- sample xs
          push $ RemoveFromUnderneath (toTarget lid) [card]
          placeUnderneath shambler [card]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (spawnedEnemy -> enemy) _ -> do
      shambler <- selectJust $ enemyIs Enemies.dimensionalShamblerDraggingThemBelow
      card <- field EnemyCard enemy
      push $ RemovedFromPlay (EnemySource enemy)
      card' <- setFacedown True card
      placeUnderneath shambler [card']
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (kill attrs)
      pure a
    _ -> IntoThePit <$> liftRunMessage msg attrs
