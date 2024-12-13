module Arkham.Enemy.Cards.ProfessorWilliamDyerProfessorOfGeology (
  professorWilliamDyerProfessorOfGeology,
)
where

import Arkham.Ability
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Matcher

newtype ProfessorWilliamDyerProfessorOfGeology = ProfessorWilliamDyerProfessorOfGeology EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWilliamDyerProfessorOfGeology :: EnemyCard ProfessorWilliamDyerProfessorOfGeology
professorWilliamDyerProfessorOfGeology =
  enemy
    ProfessorWilliamDyerProfessorOfGeology
    Cards.professorWilliamDyerProfessorOfGeology
    (4, Static 2, 2)
    (0, 2)

instance HasAbilities ProfessorWilliamDyerProfessorOfGeology where
  getAbilities (ProfessorWilliamDyerProfessorOfGeology a) =
    extend
      a
      [ restricted a 1 OnSameLocation $ parleyAction (UpTo (Fixed 2) $ HandDiscardCost 1 #any)
      , mkAbility a 2 $ forced $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage ProfessorWilliamDyerProfessorOfGeology where
  runMessage msg e@(ProfessorWilliamDyerProfessorOfGeology attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ (discardedCards -> cards) -> do
      placeTokens (attrs.ability 1) attrs #resource (length cards)
      doStep 2 msg
      pure e
    DoStep 2 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      n <- perPlayer 3
      when (attrs.token #resource >= n) $ addToVictory attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      eliminatePartner attrs
      pure e
    _ -> ProfessorWilliamDyerProfessorOfGeology <$> liftRunMessage msg attrs
