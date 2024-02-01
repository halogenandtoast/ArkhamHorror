module Arkham.Location.Cards.WhateleyRuins_251 (
  whateleyRuins_251,
  WhateleyRuins_251 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Helpers.Enemy
import Arkham.Location.Cards qualified as Cards (whateleyRuins_251)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype WhateleyRuins_251 = WhateleyRuins_251 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

whateleyRuins_251 :: LocationCard WhateleyRuins_251
whateleyRuins_251 = location WhateleyRuins_251 Cards.whateleyRuins_251 2 (PerPlayer 2)

instance HasModifiersFor WhateleyRuins_251 where
  getModifiersFor (InvestigatorTarget iid) (WhateleyRuins_251 attrs) = do
    here <- iid `isAt` attrs
    pure $ toModifiers attrs [SkillModifier #willpower (-1) | here]
  getModifiersFor _ _ = pure []

instance HasAbilities WhateleyRuins_251 where
  getAbilities (WhateleyRuins_251 attrs) = withRevealedAbilities attrs [restrictedAbility attrs 1 Here actionAbility]

instance RunMessage WhateleyRuins_251 where
  runMessage msg l@(WhateleyRuins_251 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) attrs #intellect 4
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      abominations <- getBroodOfYogSothoth
      abominationsWithLocation <- forToSnd abominations (selectJust . locationWithEnemy)
      abominationsWithLocationAndAccessibleLocations <- for abominationsWithLocation $ \(abomination, locationId) ->
        (abomination,locationId,) <$> getEnemyAccessibleLocations abomination

      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel eid [chooseOne player $ targetLabels destinations (only . EnemyMove eid)]
          | (eid, _, destinations) <- abominationsWithLocationAndAccessibleLocations
          ]
      pure l
    _ -> WhateleyRuins_251 <$> runMessage msg attrs
