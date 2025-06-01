module Arkham.Asset.Assets.GeneBeauregard3 (geneBeauregard3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype GeneBeauregard3 = GeneBeauregard3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

geneBeauregard3 :: AssetCard GeneBeauregard3
geneBeauregard3 = ally GeneBeauregard3 Cards.geneBeauregard3 (2, 2)

instance HasModifiersFor GeneBeauregard3 where
  getModifiersFor (GeneBeauregard3 a) = controllerGets a [SkillModifier #intellect 1, SkillModifier #agility 1]

instance HasAbilities GeneBeauregard3 where
  getAbilities (GeneBeauregard3 x) =
    [ controlled
        x
        1
        ( DuringTurn You
            <> oneOf
              [ exists (YourLocation <> CanMoveCluesFromLocation) <> exists ConnectedLocation
              , exists (ConnectedLocation <> CanMoveCluesFromLocation)
              , exists (NonEliteEnemy <> EnemyAt YourLocation <> EnemyCanEnter ConnectedLocation)
              , exists (NonEliteEnemy <> EnemyAt ConnectedLocation <> EnemyCanEnter YourLocation)
              ]
        )
        $ triggered (Enters #after You Anywhere) (exhaust x)
    ]

instance RunMessage GeneBeauregard3 where
  runMessage msg a@(GeneBeauregard3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let connectedLocation = ConnectedTo (locationWithInvestigator iid)
      option1 <-
        andM [selectAny $ locationWithInvestigator iid <> LocationWithAnyClues, selectAny connectedLocation]
      option2 <- selectAny $ connectedLocation <> LocationWithAnyClues
      option3 <- selectAny $ NonEliteEnemy <> enemyAtLocationWith iid <> EnemyCanEnter connectedLocation
      option4 <-
        selectAny $ NonEliteEnemy <> at_ connectedLocation <> EnemyCanEnter (locationWithInvestigator iid)
      let chooseOption = push . HandleAbilityOption iid (toSource attrs)

      chooseOrRunOneM iid do
        when option1 do
          labeled "Move 1 clue from your location to a connecting location" $ chooseOption 1
        when option2 do
          labeled "Move 1 clue from a connecting location to your location" $ chooseOption 2
        when option3 do
          labeled "Move an enemy from your location to a connecting location" $ chooseOption 3
        when option4 do
          labeled "Move an enemy from a connecting location to your location" $ chooseOption 4

      pure a
    HandleAbilityOption iid (isSource attrs -> True) 1 -> do
      yourLocation <- selectJust $ locationWithInvestigator iid <> LocationWithAnyClues
      connected <- select $ ConnectedTo (locationWithInvestigator iid)
      chooseOrRunOneM iid do
        targets connected \l -> moveTokens (attrs.ability 1) yourLocation l #clue 1
      pure a
    HandleAbilityOption iid (isSource attrs -> True) 2 -> do
      yourLocation <- selectJust $ locationWithInvestigator iid
      connected <- select $ ConnectedTo (locationWithInvestigator iid) <> LocationWithAnyClues
      chooseOrRunOneM iid do
        targets connected \l -> moveTokens (attrs.ability 1) l yourLocation #clue 1
      pure a
    HandleAbilityOption iid (isSource attrs -> True) 3 -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy
      connected <- select $ ConnectedTo (locationWithInvestigator iid)
      chooseOrRunOneM iid do
        targets enemies \enemy ->
          chooseOrRunOneM iid $ targets connected (enemyMoveTo enemy)
      pure a
    HandleAbilityOption iid (isSource attrs -> True) 4 -> do
      enemies <- select $ at_ (ConnectedTo (locationWithInvestigator iid)) <> NonEliteEnemy
      location <- getJustLocation iid
      chooseOrRunOneM iid $ targets enemies (`enemyMoveTo` location)
      pure a
    _ -> GeneBeauregard3 <$> liftRunMessage msg attrs
