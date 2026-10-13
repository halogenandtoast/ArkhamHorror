module Arkham.Homebrew.AgainstTheWendigo.Enemies.AngrySarceeMen (angrySarceeMen) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log (whenHasRecord)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Sarcee)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype AngrySarceeMen = AngrySarceeMen EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

angrySarceeMen :: EnemyCard AngrySarceeMen
angrySarceeMen =
  enemy AngrySarceeMen Cards.angrySarceeMen
    & setSpawnAt (NearestLocationToYou $ LocationWithTrait Sarcee)

instance HasModifiersFor AngrySarceeMen where
  getModifiersFor (AngrySarceeMen a) = do
    modifySelect a (InvestigatorAt $ locationWithEnemy a) [CannotDiscoverClues]
    whenHasRecord TheSarceeAreHuntingYouDown do
      modifySelf a [RemoveKeyword #aloof, AddKeyword #hunter]

instance HasAbilities AngrySarceeMen where
  getAbilities (AngrySarceeMen a) =
    extend
      a
      [ restricted a 1 (not_ $ hasRecordCriteria TheSarceeAreHuntingYouDown)
          $ forced
          $ EnemyDealtDamage #after AnyDamageEffect (be a) AnySource
      , restricted a 2 (hasRecordCriteria TheSarceeAreHuntingYouDown)
          $ silent
          $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage AngrySarceeMen where
  runMessage msg e@(AngrySarceeMen attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record TheSarceeAreHuntingYouDown
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      shuffleBackIntoEncounterDeck (attrs.ability 2) attrs
      pure e
    _ -> AngrySarceeMen <$> liftRunMessage msg attrs
