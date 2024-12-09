module Arkham.Agenda.Cards.EndsAndMeans (
  EndsAndMeans (..),
  endsAndMeans,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Timing qualified as Timing
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
    [ mkAbility a 1 $ ForcedAbility $ EnemyDefeated Timing.When You ByAny $ EnemyWithTrait SilverTwilight
    ]

instance RunMessage EndsAndMeans where
  runMessage msg a@(EndsAndMeans attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      mPuzzleBox <- selectOne (assetIs Assets.puzzleBox)
      acts <- select AnyAct
      summonedBeast <- getSetAsideCard Enemies.summonedBeast
      createSummonedBeast <-
        createEnemyAtLocationMatching_ summonedBeast
          $ maybe
            (LocationWithTrait Sanctum)
            locationWithAsset
            mPuzzleBox

      pushAll
        $ map (toDiscard GameSource) acts
        <> [createSummonedBeast]
        <> [RemoveFromGame (toTarget puzzleBox) | puzzleBox <- maybeToList mPuzzleBox]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      enemiesWithDoom <- select $ EnemyAt (locationWithEnemy enemy) <> EnemyWithAnyDoom
      pushAll
        $ concat
          [[RemoveDoom (toSource attrs) (toTarget enemy') 1, placeDoomOnAgenda] | enemy' <- enemiesWithDoom]
      pure a
    _ -> EndsAndMeans <$> runMessage msg attrs
