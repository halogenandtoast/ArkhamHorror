module Arkham.Act.Cards.StrangeRelicsMariaDeSilva (
  StrangeRelicsMariaDeSilva (..),
  strangeRelicsMariaDeSilva,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher hiding (AssetCard)
import Arkham.Name
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype StrangeRelicsMariaDeSilva = StrangeRelicsMariaDeSilva ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeRelicsMariaDeSilva :: ActCard StrangeRelicsMariaDeSilva
strangeRelicsMariaDeSilva =
  act (2, E) StrangeRelicsMariaDeSilva Cards.strangeRelicsMariaDeSilva Nothing

instance HasAbilities StrangeRelicsMariaDeSilva where
  getAbilities (StrangeRelicsMariaDeSilva a) =
    [ restrictedAbility
      a
      1
      ( AssetExists
          $ assetIs Assets.mariaDeSilva
          <> AssetWithClues
            (AtLeast $ PerPlayer 1)
      )
      $ Objective
      $ ForcedAbility AnyWindow
    | onSide E a
    ]

instance RunMessage StrangeRelicsMariaDeSilva where
  runMessage msg a@(StrangeRelicsMariaDeSilva attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide F attrs -> do
      maria <- selectJust $ assetIs Assets.mariaDeSilva
      mariasLocation <- selectJust $ LocationWithAsset $ AssetWithId maria
      cardId <- field AssetCardId maria
      let
        mariaDeSilvaKnowsMoreThanSheLetsOn =
          lookupCard
            Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn
            cardId

      createMariaDeSilva <- createEnemyAt_ mariaDeSilvaKnowsMoreThanSheLetsOn mariasLocation Nothing
      pushAll
        [ createMariaDeSilva
        , Flipped (toSource maria) mariaDeSilvaKnowsMoreThanSheLetsOn
        , NextAdvanceActStep aid 1
        , AdvanceToAct (actDeckId attrs) Acts.theBrotherhoodIsRevealed E (toSource attrs)
        ]
      pure a
    NextAdvanceActStep aid 1 | aid == actId attrs && onSide B attrs -> do
      maria <- selectJust $ enemyIs Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn
      pushAll
        [Remember $ IchtacasPrey (Labeled (toName Enemies.mariaDeSilvaKnowsMoreThanSheLetsOn) maria)]
      pure a
    _ -> StrangeRelicsMariaDeSilva <$> runMessage msg attrs
