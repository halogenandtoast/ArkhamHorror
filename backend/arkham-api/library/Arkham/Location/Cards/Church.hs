module Arkham.Location.Cards.Church (church) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Scenarios.TheBlobThatAteEverything.Helpers
import Arkham.Trait (Trait (Ooze))

newtype Church = Church LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

church :: LocationCard Church
church = locationWith Church Cards.church 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities Church where
  getAbilities (Church a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "church.evade"
      $ groupLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> thisExists a LocationWithoutClues
            <> exists (EnemyWithTrait Ooze <> EnemyAt (oneOf [be a, connectedFrom (be a)]))
        )
        actionAbility

instance RunMessage Church where
  runMessage msg l@(Church attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      oozes <-
        select
          $ EnemyWithTrait Ooze
          <> EnemyAt (oneOf [be attrs, connectedFrom (be attrs)])
      destinations <- select $ connectedFrom (be attrs)
      chooseTargetM iid oozes \enemy ->
        chooseTargetM iid destinations \destination -> do
          enemyMoveTo (attrs.ability 1) enemy destination
          automaticallyEvadeEnemy iid enemy
      pure l
    _ -> Church <$> liftRunMessage msg attrs
