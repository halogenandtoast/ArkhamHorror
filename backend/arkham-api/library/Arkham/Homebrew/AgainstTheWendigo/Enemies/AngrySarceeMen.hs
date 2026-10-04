module Arkham.Homebrew.AgainstTheWendigo.Enemies.AngrySarceeMen (angrySarceeMen) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Log (getHasRecord)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Key
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Sarcee)
import Arkham.Keyword qualified as Keyword
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
    -- "You cannot find clues if you are in the same location as Angry Sarcee Men."
    investigators <- modifySelect a (InvestigatorAt $ locationWithEnemy a) [CannotDiscoverClues]
    -- Once the Sarcee are hunting you they stop ignoring you and give chase.
    hunting <- getHasRecord TheSarceeAreHuntingYouDown
    self <-
      modifySelf a
        $ guard hunting *> [RemoveKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]
    pure $ investigators <> self

instance HasAbilities AngrySarceeMen where
  getAbilities (AngrySarceeMen a) =
    [ restricted a 1 (not_ $ hasRecordCriteria TheSarceeAreHuntingYouDown)
        $ forced
        $ EnemyDealtDamage #after AnyDamageEffect (be a) AnySource
    , -- "If Angry Sarcee Men are defeated, shuffle them into the encounter deck."
      restricted a 2 (hasRecordCriteria TheSarceeAreHuntingYouDown)
        $ forced
        $ EnemyDefeated #when Anyone ByAny (be a)
    ]

instance RunMessage AngrySarceeMen where
  runMessage msg e@(AngrySarceeMen attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      record TheSarceeAreHuntingYouDown
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ ShuffleBackIntoEncounterDeck (toSource attrs) (toTarget attrs)
      pure e
    _ -> AngrySarceeMen <$> liftRunMessage msg attrs
