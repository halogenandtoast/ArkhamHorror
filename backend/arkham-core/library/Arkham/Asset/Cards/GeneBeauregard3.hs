module Arkham.Asset.Cards.GeneBeauregard3 (
  geneBeauregard3,
  GeneBeauregard3 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Prelude

newtype GeneBeauregard3 = GeneBeauregard3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

geneBeauregard3 :: AssetCard GeneBeauregard3
geneBeauregard3 = ally GeneBeauregard3 Cards.geneBeauregard3 (2, 2)

instance HasModifiersFor GeneBeauregard3 where
  getModifiersFor (InvestigatorTarget iid) (GeneBeauregard3 a) | a `controlledBy` iid = do
    pure $ toModifiers a [SkillModifier #intellect 1, SkillModifier #agility 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GeneBeauregard3 where
  getAbilities (GeneBeauregard3 x) =
    [ controlledAbility
        x
        1
        ( DuringTurn You
            <> AnyCriterion
              [ exists (YourLocation <> LocationWithAnyClues) <> exists ConnectedLocation
              , exists (ConnectedLocation <> LocationWithAnyClues)
              , exists (NonEliteEnemy <> EnemyAt YourLocation <> EnemyCanEnter ConnectedLocation)
              , exists (NonEliteEnemy <> EnemyAt ConnectedLocation <> EnemyCanEnter YourLocation)
              ]
        )
        $ ReactionAbility (Moves #after You AnySource Anywhere Anywhere) (exhaust x)
    ]

instance RunMessage GeneBeauregard3 where
  runMessage msg a@(GeneBeauregard3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      option1 <-
        andM [selectAny $ locationWithInvestigator iid <> LocationWithAnyClues, selectAny ConnectedLocation]
      option2 <- selectAny $ ConnectedLocation <> LocationWithAnyClues
      option3 <- selectAny $ NonEliteEnemy <> enemyAtLocationWith iid <> EnemyCanEnter ConnectedLocation
      option4 <-
        selectAny
          $ NonEliteEnemy
          <> EnemyAt ConnectedLocation
          <> EnemyCanEnter (locationWithInvestigator iid)
      player <- getPlayer iid
      let chooseOption = HandleAbilityOption iid (toSource attrs)

      push
        $ chooseOrRunOne player
        $ [Label "Move 1 clue from your location to a connecting location" [chooseOption 1] | option1]
        <> [Label "Move 1 clue from a connecting location to your location" [chooseOption 2] | option2]
        <> [Label "Move an enemy from your location to a connecting location" [chooseOption 3] | option3]
        <> [Label "Move an enemy from a connecting location to your location" [chooseOption 4] | option4]

      pure a
    HandleAbilityOption iid (isSource attrs -> True) 1 -> do
      yourLocation <- selectJust $ locationWithInvestigator iid <> LocationWithAnyClues
      connected <- selectList ConnectedLocation
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [targetLabel l [MovedClues (toSource yourLocation) (toTarget l) 1] | l <- connected]

      pure a
    HandleAbilityOption iid (isSource attrs -> True) 2 -> do
      yourLocation <- selectJust $ locationWithInvestigator iid <> LocationWithAnyClues
      connected <- selectList ConnectedLocation
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [targetLabel l [MovedClues (toSource l) (toTarget yourLocation) 1] | l <- connected]

      pure a
    HandleAbilityOption iid (isSource attrs -> True) 3 -> do
      enemies <- selectList $ enemyAtLocationWith iid <> NonEliteEnemy
      connected <- selectList ConnectedLocation
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel
            enemy
            [chooseOrRunOne player [targetLabel location [EnemyMove enemy location]] | location <- connected]
          | enemy <- enemies
          ]

      pure a
    HandleAbilityOption iid (isSource attrs -> True) 4 -> do
      enemies <- selectList $ EnemyAt ConnectedLocation <> NonEliteEnemy
      location <- getJustLocation iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel location [EnemyMove enemy location]
          | enemy <- enemies
          ]

      pure a
    _ -> GeneBeauregard3 <$> runMessage msg attrs
