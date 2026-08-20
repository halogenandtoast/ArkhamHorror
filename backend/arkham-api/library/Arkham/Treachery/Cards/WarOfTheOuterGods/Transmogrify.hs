module Arkham.Treachery.Cards.WarOfTheOuterGods.Transmogrify (transmogrify) where

import Arkham.Card (toCard)
import Arkham.Enemy.CardDefs.WarOfTheOuterGods qualified as Enemies
import Arkham.Location.CardDefs.WarOfTheOuterGods qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Insect))
import Arkham.Treachery.CardDefs.WarOfTheOuterGods qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Transmogrify = Transmogrify TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

transmogrify :: TreacheryCard Transmogrify
transmogrify = treachery Transmogrify Cards.transmogrify

instance RunMessage Transmogrify where
  runMessage msg t@(Transmogrify attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      insects <- select $ EnemyWithTrait Insect <> not_ IsSwarm
      if null insects
        then
          findEncounterCard iid attrs $ cardsAre [Enemies.trylogog, Enemies.trylogogWarOfTheOuterGods]
        else do
          assets <- select $ assetControlledBy iid
          chooseOneM iid $ targets assets \asset ->
            chooseTargetM iid insects (`placeAssetAsSwarm` asset)
      pure t
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      burningPit <- selectOne $ locationIs Locations.theBurningPit
      for_ burningPit $ spawnEnemyAt_ card
      pure t
    _ -> Transmogrify <$> liftRunMessage msg attrs
