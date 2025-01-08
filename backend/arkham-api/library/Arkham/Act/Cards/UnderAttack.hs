module Arkham.Act.Cards.UnderAttack (underAttack) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait (Trait (Eidolon))

newtype UnderAttack = UnderAttack ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underAttack :: ActCard UnderAttack
underAttack = act (3, A) UnderAttack Cards.underAttack Nothing

instance HasAbilities UnderAttack where
  getAbilities (UnderAttack a) =
    extend
      a
      [ restricted (proxied (LocationWithModifier $ ScenarioModifier "camp") a) 1 Here
          $ ActionAbility [#resign] (ActionCost 1 <> GroupClueCost (Static 4) YourLocation)
      , restricted a 1 (not_ $ exists $ InPlayEnemy $ EnemyWithTrait Eidolon)
          $ Objective
          $ forced AnyWindow
      , restricted a 2 AllUndefeatedInvestigatorsResigned
          $ Objective
          $ forced AnyWindow
      ]

instance HasModifiersFor UnderAttack where
  getModifiersFor (UnderAttack a) = do
    eidolon <- findAllCards (`cardMatch` CardWithTrait Eidolon)
    modifyEach a eidolon [GainVictory 0]

instance RunMessage UnderAttack where
  runMessage msg a@(UnderAttack attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      noEidolons <- selectNone $ InPlayEnemy $ EnemyWithTrait Eidolon
      push $ if noEidolons then R1 else R2
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      resign iid
      pure a
    _ -> UnderAttack <$> liftRunMessage msg attrs
