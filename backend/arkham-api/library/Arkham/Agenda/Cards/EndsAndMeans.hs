module Arkham.Agenda.Cards.EndsAndMeans (endsAndMeans) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Trait (Trait (Sanctum, SilverTwilight))

newtype EndsAndMeans = EndsAndMeans AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endsAndMeans :: AgendaCard EndsAndMeans
endsAndMeans = agenda (2, A) EndsAndMeans Cards.endsAndMeans (Static 10)

instance HasModifiersFor EndsAndMeans where
  getModifiersFor (EndsAndMeans a) = do
    modifySelect
      a
      (EnemyWithTrait SilverTwilight <> at_ (LocationWithTrait Sanctum))
      [RemoveKeyword Aloof]

instance HasAbilities EndsAndMeans where
  getAbilities (EndsAndMeans a) =
    [mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny $ EnemyWithTrait SilverTwilight]

instance RunMessage EndsAndMeans where
  runMessage msg a@(EndsAndMeans attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach AnyAct $ toDiscard GameSource
      summonedBeast <- getSetAsideCard Enemies.summonedBeast
      mPuzzleBox <- selectOne (assetIs Assets.puzzleBox)
      createEnemyAtLocationMatching_ summonedBeast
        $ maybe (LocationWithTrait Sanctum) locationWithAsset mPuzzleBox
      for_ mPuzzleBox removeFromGame
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      enemiesWithDoom <- select $ EnemyAt (locationWithEnemy enemy) <> EnemyWithAnyDoom
      for_ enemiesWithDoom \enemy' -> do
        removeDoom attrs enemy' 1
        placeDoomOnAgenda 1
      pure a
    _ -> EndsAndMeans <$> liftRunMessage msg attrs
