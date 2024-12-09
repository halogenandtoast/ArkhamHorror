module Arkham.Asset.Assets.GeneBeauregard3 (
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
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

geneBeauregard3 :: AssetCard GeneBeauregard3
geneBeauregard3 = ally GeneBeauregard3 Cards.geneBeauregard3 (2, 2)

instance HasModifiersFor GeneBeauregard3 where
  getModifiersFor (GeneBeauregard3 a) = controllerGets a [SkillModifier #intellect 1, SkillModifier #agility 1]

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
        $ ReactionAbility (Enters #after You Anywhere) (exhaust x)
    ]

instance RunMessage GeneBeauregard3 where
  runMessage msg a@(GeneBeauregard3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let connectedLocation = ConnectedTo (locationWithInvestigator iid)
      option1 <-
        andM [selectAny $ locationWithInvestigator iid <> LocationWithAnyClues, selectAny connectedLocation]
      option2 <- selectAny $ connectedLocation <> LocationWithAnyClues
      option3 <- selectAny $ NonEliteEnemy <> enemyAtLocationWith iid <> EnemyCanEnter connectedLocation
      option4 <-
        selectAny
          $ NonEliteEnemy
          <> EnemyAt connectedLocation
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
      connected <- select $ ConnectedTo (locationWithInvestigator iid)
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel l [MovedClues (attrs.ability 1) (toSource yourLocation) (toTarget l) 1] | l <- connected
          ]

      pure a
    HandleAbilityOption iid (isSource attrs -> True) 2 -> do
      yourLocation <- selectJust $ locationWithInvestigator iid
      connected <- select $ ConnectedTo (locationWithInvestigator iid) <> LocationWithAnyClues
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel l [MovedClues (attrs.ability 1) (toSource l) (toTarget yourLocation) 1] | l <- connected
          ]

      pure a
    HandleAbilityOption iid (isSource attrs -> True) 3 -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy
      connected <- select $ ConnectedTo (locationWithInvestigator iid)
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
      enemies <- select $ at_ (ConnectedTo (locationWithInvestigator iid)) <> NonEliteEnemy
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
