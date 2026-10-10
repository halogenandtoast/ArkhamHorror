module Arkham.Homebrew.AgesUnwound.Enemies.Arsonists (arsonists) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Arsonists = Arsonists EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arsonists :: EnemyCard Arsonists
arsonists = enemy Arsonists Cards.arsonists

{- | "Forced - At the end of the enemy phase, if Arsonists are ready and
unengaged: Each investigator at Arsonists' location takes 1 damage and 1
horror."

Aloof, so they do not engage on their own; leaving them standing at your
location is what costs you.
-}
instance HasAbilities Arsonists where
  getAbilities (Arsonists a) =
    extend1 a
      $ restricted a 1 (thisExists a $ ReadyEnemy <> UnengagedEnemy)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage Arsonists where
  runMessage msg e@(Arsonists attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt $ locationWithEnemy attrs.id) \iid ->
        assignDamageAndHorror iid (attrs.ability 1) 1 1
      pure e
    _ -> Arsonists <$> liftRunMessage msg attrs
