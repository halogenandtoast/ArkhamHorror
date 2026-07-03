module Arkham.Act.Cards.FateOfTheValeV2 (fateOfTheValeV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Creation
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (getPlayerCountValue)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.I18n
import Arkham.Location.Types (Field (LocationCardsUnderneath))
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Trait qualified as Trait

newtype FateOfTheValeV2 = FateOfTheValeV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfTheValeV2 :: ActCard FateOfTheValeV2
fateOfTheValeV2 = act (3, A) FateOfTheValeV2 Cards.fateOfTheValeV2 Nothing

instance HasAbilities FateOfTheValeV2 where
  getAbilities (FateOfTheValeV2 a) =
    extend
      a
      [ skillTestAbility $ restricted a 1 (DuringTurn You) $ parleyAction (HandDiscardCost 1 #any)
      , onlyOnce
          $ restricted
            a
            2
            ( notExists (EnemyWithTrait Trait.Resident)
                <> notExists (LocationWithCardsUnderneath $ HasCard $ CardWithTrait Trait.Resident)
            )
          $ Objective
          $ FastAbility (GroupClueCost (PerPlayer 3) Anywhere)
      ]

instance RunMessage FateOfTheValeV2 where
  runMessage msg a@(FateOfTheValeV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        skillLabeled #willpower $ parley sid iid (attrs.ability 1) attrs #willpower (Fixed 3)
        skillLabeled #intellect $ parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withLocationOf iid \lid -> do
        beneath <- field LocationCardsUnderneath lid
        for_ [card | card <- beneath, cardMatch card (CardWithTrait Trait.Resident)] \card -> do
          focusCards [card] do
            chooseOneM iid $ withI18n do
              labeled' "continue" $ unfocusCards >> obtainCard card
            cluesNeeded <- getPlayerCountValue (PerPlayer 1)
            spendable <- getSpendableClueCount [iid]
            chooseOrRunOneM iid $ withI18n do
              when (spendable >= cluesNeeded) do
                countVar cluesNeeded $ labeled' "spendClues" do
                  spendClues iid cluesNeeded
                  createEnemyAtEdit_ card lid createExhausted
              labeled' "skip" $ createEnemyAt_ card lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs (attrs.ability 2)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R3
      pure a
    _ -> FateOfTheValeV2 <$> liftRunMessage msg attrs
