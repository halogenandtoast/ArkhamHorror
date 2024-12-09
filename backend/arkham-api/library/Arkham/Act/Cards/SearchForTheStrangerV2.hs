module Arkham.Act.Cards.SearchForTheStrangerV2 (SearchForTheStrangerV2 (..), searchForTheStrangerV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude

newtype SearchForTheStrangerV2 = SearchForTheStrangerV2 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV2 :: ActCard SearchForTheStrangerV2
searchForTheStrangerV2 =
  act (2, A) SearchForTheStrangerV2 Cards.searchForTheStrangerV2 Nothing

instance HasModifiersFor SearchForTheStrangerV2 where
  getModifiersFor (SearchForTheStrangerV2 a) = do
    theMan <-
      modifySelectMap a (enemyIs Enemies.theManInThePallidMask) \eid ->
        [CanOnlyBeDefeatedBy (SourceIs $ EnemySource eid)]
    investigators <- modifySelect a Anyone [CannotDiscoverClues]
    pure $ theMan <> investigators

instance HasAbilities SearchForTheStrangerV2 where
  getAbilities (SearchForTheStrangerV2 x) =
    [mkAbility x 1 $ forced $ EnemyWouldBeDefeated #when $ enemyIs Enemies.theManInThePallidMask]

instance RunMessage SearchForTheStrangerV2 where
  runMessage msg a@(SearchForTheStrangerV2 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      hastur <- getSetAsideCard Enemies.hasturTheKingInYellow
      theManInThePallidMask <- selectJust (enemyIs Enemies.theManInThePallidMask)
      createHastur <- toMessage <$> createEnemy hastur Global
      pushAll
        [ createHastur
        , RemoveEnemy theManInThePallidMask
        , AdvanceActDeck (actDeckId attrs) (toSource attrs)
        ]
      pure a
    _ -> SearchForTheStrangerV2 <$> runMessage msg attrs
