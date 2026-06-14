module Arkham.Act.Cards.LostPilgrims (lostPilgrims) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.ForMovement
import {-# SOURCE #-} Arkham.GameEnv (findAllCards)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario (getVictoryDisplay)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message (pattern PassedThisSkillTestBy)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Cultist, Stowaway))

newtype LostPilgrims = LostPilgrims ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostPilgrims :: ActCard LostPilgrims
lostPilgrims = act (2, A) LostPilgrims Cards.lostPilgrims Nothing

instance HasModifiersFor LostPilgrims where
  getModifiersFor (LostPilgrims a) = do
    modifySelect a (EnemyWithTrait Cultist) [AddKeyword Keyword.Aloof]
    cultistCards <- findAllCards (`cardMatch` CardWithTrait Cultist)
    modifyEach a cultistCards [GainVictory 0]

instance HasAbilities LostPilgrims where
  getAbilities (LostPilgrims a) =
    extend
      a
      [ skillTestAbility
          $ restricted a 1 (exists $ EnemyWithTrait Cultist <> at_ YourLocation) parleyAction_
      , mkAbility a 2 $ forced $ PhaseEnds #when #enemy
      , mkAbility a 3 $ Objective $ forced $ RoundEnds #when
      ]

instance RunMessage LostPilgrims where
  runMessage msg a@(LostPilgrims attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        skillLabeled #intellect $ parley sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
        skillLabeled #combat $ parley sid iid (attrs.ability 1) attrs #combat (Fixed 2)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n > 0 -> do
      let
        go 0 _ = pure ()
        go k moved = do
          cultists <-
            select
              $ EnemyWithTrait Cultist
              <> enemyAtLocationWith iid
              <> not_ (mapOneOf EnemyWithId moved)
          case cultists of
            [] -> pure ()
            _ -> chooseOrRunOneM iid $ targets cultists \cultist -> do
              mloc <- getLocationOf cultist
              for_ mloc \loc -> do
                connectedLocations <- select $ accessibleFrom ForMovement loc
                chooseOrRunOneM iid $ targets connectedLocations (enemyMoveTo (attrs.ability 1) cultist)
              lift $ go (k - 1) (cultist : moved)
      go n []
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      cultists <- select $ EnemyWithTrait Cultist
      for_ cultists \cultist -> do
        stowaways <- selectCount $ EnemyWithTrait Stowaway <> EnemyAt (locationWithEnemy cultist)
        when (stowaways > 0) $ nonAttackEnemyDamage Nothing (attrs.ability 2) stowaways cultist
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      inVictory <- count (`cardMatch` CardWithTrait Cultist) <$> getVictoryDisplay
      let underAct = count (`cardMatch` CardWithTrait Cultist) attrs.underneath
      when (inVictory + underAct >= 5) $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      let underAct = count (`cardMatch` CardWithTrait Cultist) attrs.underneath
      push $ if underAct >= 3 then R1 else R2
      pure a
    _ -> LostPilgrims <$> liftRunMessage msg attrs
