module Arkham.Enemy.Cards.EzelZenRezl (ezelZenRezl) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (Insect))

newtype EzelZenRezl = EzelZenRezl EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ezelZenRezl :: EnemyCard EzelZenRezl
ezelZenRezl = enemy EzelZenRezl Cards.ezelZenRezl

instance HasModifiersFor EzelZenRezl where
  getModifiersFor (EzelZenRezl a) = do
    n <- perPlayer 8
    modifySelf a [HealthModifier n]

instance HasAbilities EzelZenRezl where
  getAbilities (EzelZenRezl a) =
    extend1 a
      $ restricted
        a
        1
        (exists (investigatorAt (locationWithEnemy a)) <> exists (EnemyWithTrait Insect))
      $ forced
      $ PhaseEnds #when #mythos

instance RunMessage EzelZenRezl where
  runMessage msg e@(EzelZenRezl attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (investigatorAt (locationWithEnemy attrs)) \iid ->
        forTarget iid msg
      pure e
    ForTarget (InvestigatorTarget iid) (UseThisAbility _ (isSource attrs -> True) 1) -> do
      hand <- iid.hand
      assets <- select $ assetControlledBy iid
      whenM (selectAny $ EnemyWithTrait Insect) do
        when (notNull hand) do
          chooseOneM iid $ targets hand \card -> do
            insects <- select $ EnemyWithTrait Insect
            chooseTargetM iid insects \insect -> placeCardAsSwarm insect card
        when (notNull assets) do
          chooseOneM iid $ targets assets \asset -> do
            insects <- select $ EnemyWithTrait Insect
            chooseTargetM iid insects \insect -> placeAssetAsSwarm insect asset
      pure e
    _ -> EzelZenRezl <$> liftRunMessage msg attrs
