module Arkham.Location.Cards.ChildrenOfBlood.RiverOfBlood.MainStreetDusk (mainStreetDusk) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.I18n
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait (Trait (Civilian, Monster))

newtype MainStreetDusk = MainStreetDusk LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mainStreetDusk :: LocationCard MainStreetDusk
mainStreetDusk = symbolLabel $ location MainStreetDusk Cards.mainStreetDusk 3 (PerPlayer 2)

instance HasAbilities MainStreetDusk where
  getAbilities (MainStreetDusk a) =
    extendRevealed1 a
      $ restricted
        a
        1
        ( oneOf
            [ EncounterDeckIsNotEmpty
            , exists (EnemyWithTrait Monster <> EnemyCanMove <> not_ (at_ civilianLocation))
                <> exists (EnemyWithTrait Civilian)
            ]
        )
      $ forced
      $ DiscoveringLastClue #after You (be a)
   where
    civilianLocation = LocationWithEnemy (EnemyWithTrait Civilian)

instance RunMessage MainStreetDusk where
  runMessage msg l@(MainStreetDusk attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      monsters <- select $ NearestEnemyToLocation attrs.id (EnemyWithTrait Monster)
      movers <-
        catMaybes <$> for monsters \monster -> runMaybeT do
          guardM $ monster <=~> EnemyCanMove
          locMonster <- MaybeT $ field EnemyLocation monster
          civilian <- MaybeT $ selectOne $ NearestEnemyToLocation locMonster (EnemyWithTrait Civilian)
          locCivilian <- MaybeT $ field EnemyLocation civilian
          guard $ locCivilian /= locMonster
          pure (monster, locCivilian)
      canDraw <- notNull . unDeck <$> getEncounterDeck
      when (canDraw || notNull movers) do
        chooseOrRunOneM iid do
          when canDraw
            $ withI18n
            $ labeledI18n "drawTopCardOfEncounterDeck"
            $ drawEncounterCard iid (attrs.ability 1)
          for_ movers \(monster, destination) ->
            targeting monster $ push $ MoveToward (toTarget monster) (LocationWithId destination)
      pure l
    _ -> MainStreetDusk <$> liftRunMessage msg attrs
