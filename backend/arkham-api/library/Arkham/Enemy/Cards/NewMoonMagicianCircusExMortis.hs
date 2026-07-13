module Arkham.Enemy.Cards.NewMoonMagicianCircusExMortis (newMoonMagicianCircusExMortis) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype NewMoonMagicianCircusExMortis = NewMoonMagicianCircusExMortis EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newMoonMagicianCircusExMortis :: EnemyCard NewMoonMagicianCircusExMortis
newMoonMagicianCircusExMortis =
  enemy NewMoonMagicianCircusExMortis Cards.newMoonMagicianCircusExMortis
    & setSpawnAt EmptyLocation

instance HasModifiersFor NewMoonMagicianCircusExMortis where
  getModifiersFor (NewMoonMagicianCircusExMortis a) = do
    moonInBag <- selectAny $ chaosToken_ #moon
    if moonInBag
      then modifySelf a [AddKeyword Keyword.Aloof]
      else modifySelf a [AddKeyword Keyword.Hunter, EnemyFight 2]

instance HasAbilities NewMoonMagicianCircusExMortis where
  getAbilities (NewMoonMagicianCircusExMortis a) =
    extend1 a
      $ restricted a 1 (exists (NearestToEnemy (be a)) <> exists (#moon :: ChaosTokenMatcher))
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage NewMoonMagicianCircusExMortis where
  runMessage msg e@(NewMoonMagicianCircusExMortis attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ NearestToEnemy (be attrs)
      selectOne (chaosToken_ #moon) >>= traverse_ \token ->
        leadChooseOneM $ targets investigators \iid -> sealChaosToken iid iid token
      pure e
    _ -> NewMoonMagicianCircusExMortis <$> liftRunMessage msg attrs
