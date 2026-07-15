module Arkham.Homebrew.CircusExMortis.Enemies.NewMoonMagician (newMoonMagician) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.Tokens (pattern MoonToken)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype NewMoonMagician = NewMoonMagician EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonMagician :: EnemyCard NewMoonMagician
newMoonMagician =
  enemy NewMoonMagician Cards.newMoonMagician
    & setSpawnAt EmptyLocation

instance HasModifiersFor NewMoonMagician where
  getModifiersFor (NewMoonMagician a) = do
    moonInBag <- selectAny $ chaosToken_ (ChaosTokenFaceIs MoonToken)
    if moonInBag
      then modifySelf a [AddKeyword Keyword.Aloof]
      else modifySelf a [AddKeyword Keyword.Hunter, EnemyFight 2]

instance HasAbilities NewMoonMagician where
  getAbilities (NewMoonMagician a) =
    extend1 a
      $ restricted a 1 (exists (NearestToEnemy (be a)) <> exists (ChaosTokenFaceIs MoonToken))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage NewMoonMagician where
  runMessage msg e@(NewMoonMagician attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ NearestToEnemy (be attrs)
      selectOne (chaosToken_ (ChaosTokenFaceIs MoonToken)) >>= traverse_ \token ->
        leadChooseOneM $ targets investigators \iid -> sealChaosToken iid iid token
      pure e
    _ -> NewMoonMagician <$> liftRunMessage msg attrs
