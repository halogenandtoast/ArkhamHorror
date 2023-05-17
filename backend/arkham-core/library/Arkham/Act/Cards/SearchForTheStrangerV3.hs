module Arkham.Act.Cards.SearchForTheStrangerV3 (
  SearchForTheStrangerV3 (..),
  searchForTheStrangerV3,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype SearchForTheStrangerV3 = SearchForTheStrangerV3 ActAttrs
  deriving anyclass (IsAct)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV3 :: ActCard SearchForTheStrangerV3
searchForTheStrangerV3 =
  act (2, A) SearchForTheStrangerV3 Cards.searchForTheStrangerV3 Nothing

instance HasModifiersFor SearchForTheStrangerV3 where
  getModifiersFor (EnemyTarget eid) (SearchForTheStrangerV3 a) = do
    isTheManInThePallidMask <-
      eid
        `isMatch` enemyIs Enemies.theManInThePallidMask
    pure $ toModifiers a [CanOnlyBeDefeatedByDamage | isTheManInThePallidMask]
  getModifiersFor (InvestigatorTarget _) (SearchForTheStrangerV3 a) =
    pure $ toModifiers a [CannotDiscoverClues]
  getModifiersFor _ _ = pure []

instance HasAbilities SearchForTheStrangerV3 where
  getAbilities (SearchForTheStrangerV3 x) =
    [ mkAbility x 1 $
        ForcedAbility $
          EnemyWouldBeDefeated Timing.When $
            enemyIs
              Enemies.theManInThePallidMask
    ]

instance RunMessage SearchForTheStrangerV3 where
  runMessage msg a@(SearchForTheStrangerV3 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      hastur <- getSetAsideCard Enemies.hasturLordOfCarcosa
      theManInThePallidMask <-
        selectJust
          (enemyIs Enemies.theManInThePallidMask)
      location <- selectJust $ locationWithEnemy theManInThePallidMask
      createHastur <- createEnemyAt_ hastur location Nothing
      pushAll
        [ createHastur
        , RemoveEnemy theManInThePallidMask
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> SearchForTheStrangerV3 <$> runMessage msg attrs
