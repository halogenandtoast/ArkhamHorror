module Arkham.Act.Cards.SearchForTheStrangerV1 (searchForTheStrangerV1) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype SearchForTheStrangerV1 = SearchForTheStrangerV1 ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchForTheStrangerV1 :: ActCard SearchForTheStrangerV1
searchForTheStrangerV1 = act (2, A) SearchForTheStrangerV1 Cards.searchForTheStrangerV1 Nothing

instance HasModifiersFor SearchForTheStrangerV1 where
  getModifiersFor (SearchForTheStrangerV1 a) = do
    modifySelect a (EnemyWithTitle "The Main in the Pallid Mask") [CannotBeDefeated]

instance HasAbilities SearchForTheStrangerV1 where
  getAbilities (SearchForTheStrangerV1 a) =
    [ restricted a 1 (OnLocation $ LocationWithEnemy $ enemyIs Enemies.theManInThePallidMask)
        $ Objective
        $ ActionAbility []
        $ ActionCost 3
    ]

instance RunMessage SearchForTheStrangerV1 where
  runMessage msg a@(SearchForTheStrangerV1 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      leadInvestigatorId <- getLead
      thePallidMask <- genCard Assets.thePallidMask
      addToHand leadInvestigatorId (only thePallidMask)
      advanceActDeck attrs
      pure a
    _ -> SearchForTheStrangerV1 <$> liftRunMessage msg attrs
