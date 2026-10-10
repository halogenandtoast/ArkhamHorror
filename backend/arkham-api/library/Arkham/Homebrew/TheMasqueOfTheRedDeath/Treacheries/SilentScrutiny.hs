module Arkham.Homebrew.TheMasqueOfTheRedDeath.Treacheries.SilentScrutiny (silentScrutiny) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Trait (Trait (Cultist))
import Arkham.Treachery.Import.Lifted

newtype SilentScrutiny = SilentScrutiny TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silentScrutiny :: TreacheryCard SilentScrutiny
silentScrutiny = treachery SilentScrutiny Cards.silentScrutiny

{- | "parleying with a [[Cultist]] asset or enemy at the attached location"

A parley against a story asset makes that asset the test's source, an enemy
parley makes it the target, so the two halves read different fields.
-}
parleyingWithCultistAt :: LocationId -> SkillTestMatcher
parleyingWithCultistAt lid =
  SkillTestOneOf
    [ WhileParleyingWithAnEnemy $ EnemyWithTrait Cultist <> EnemyAt (LocationWithId lid)
    , SkillTestMatches
        [WhileParleying, SkillTestOnAsset $ AssetWithTrait Cultist <> AssetAt (LocationWithId lid)]
    ]

instance HasModifiersFor SilentScrutiny where
  -- "Clues cannot be discovered from the attached location."
  getModifiersFor (SilentScrutiny a) = case a.placement of
    AttachedToLocation lid -> modifySelect a Anyone [CannotDiscoverCluesAt (LocationWithId lid)]
    _ -> pure mempty

instance HasAbilities SilentScrutiny where
  getAbilities (SilentScrutiny a) = case a.placement of
    AttachedToLocation lid ->
      [mkAbility a 1 $ forced $ SkillTestResult #after You (parleyingWithCultistAt lid) #success]
    _ -> []

instance RunMessage SilentScrutiny where
  runMessage msg t@(SilentScrutiny attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <-
        select
          $ NearestLocationTo iid
          $ LocationWithClues (atLeast 1)
          <> LocationWithoutTreachery (treacheryIs Cards.silentScrutiny)
      if null locations
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets locations $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> SilentScrutiny <$> liftRunMessage msg attrs
