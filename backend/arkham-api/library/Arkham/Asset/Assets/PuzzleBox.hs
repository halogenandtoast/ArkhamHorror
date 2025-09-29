module Arkham.Asset.Assets.PuzzleBox (puzzleBox) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Location.Brazier
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype PuzzleBox = PuzzleBox AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

puzzleBox :: AssetCard PuzzleBox
puzzleBox = asset PuzzleBox Cards.puzzleBox

instance HasAbilities PuzzleBox where
  getAbilities (PuzzleBox attrs) =
    [ controlled attrs 1 (exists (NotInvestigator You)) $ forced $ InvestigatorDefeated #when ByAny You
    , noAOO
        $ groupLimit PerGame
        $ controlled
          attrs
          2
          ( oneOf
              [ exists (enemyIs Enemies.theSpectralWatcher <> NotEnemy ExhaustedEnemy)
              , exists (enemyIs Enemies.theSpectralWatcher <> ExhaustedEnemy) <> CanDealDamage
              , exists (YourLocation <> LocationWithBrazier Lit)
              ]
          )
          actionAbility
    ]

instance RunMessage PuzzleBox where
  runMessage msg a@(PuzzleBox attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      others <- select $ not_ (InvestigatorWithId iid)
      chooseTargetM iid others (`takeControlOfAsset` attrs.id)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      exhaustedSpectralWatcher <- selectOne $ enemyIs Enemies.theSpectralWatcher <> ExhaustedEnemy
      readySpectralWatcher <- selectOne $ enemyIs Enemies.theSpectralWatcher <> ReadyEnemy
      locationLit <- selectOne $ LocationWithBrazier Lit <> locationWithInvestigator iid
      canDealDamage <- withoutModifier iid CannotDealDamage
      chooseOrRunOneM iid $ scenarioI18n do
        for_ locationLit \location ->
          labeled' "puzzleBox.unlight" $ updateLocation location LocationBrazier (Just Unlit)
        for_ readySpectralWatcher $ labeled' "puzzleBox.exhaust" . exhaustThis
        when canDealDamage do
          for_ exhaustedSpectralWatcher
            $ labeled' "puzzleBox.damage"
            . nonAttackEnemyDamage (Just iid) (attrs.ability 2) 5
      pure a
    _ -> PuzzleBox <$> liftRunMessage msg attrs
