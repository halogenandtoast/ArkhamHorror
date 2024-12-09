module Arkham.Asset.Assets.LuckyDice2 (luckyDice2, luckyDice2Effect, LuckyDice2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (IgnoreChaosToken)
import Arkham.Message qualified as Msg
import Arkham.Window qualified as Window

newtype LuckyDice2 = LuckyDice2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyDice2 :: AssetCard LuckyDice2
luckyDice2 = asset LuckyDice2 Cards.luckyDice2

instance HasAbilities LuckyDice2 where
  getAbilities (LuckyDice2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (RevealChaosToken #after You (not_ #autofail)) (ResourceCost 2)
    ]

instance RunMessage LuckyDice2 where
  runMessage msg a@(LuckyDice2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      createCardEffect Cards.luckyDice2 Nothing (attrs.ability 1) (ChaosTokenTarget token)
      drawAnotherChaosToken iid
      cancelledOrIgnoredCardOrGameEffect attrs
      pure a
    _ -> LuckyDice2 <$> liftRunMessage msg attrs

newtype LuckyDice2Effect = LuckyDice2Effect (EffectAttrs `With` Metadata)
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newtype Metadata = Metadata {alreadyTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

luckyDice2Effect :: EffectArgs -> LuckyDice2Effect
luckyDice2Effect = cardEffect (LuckyDice2Effect . (`with` Metadata False)) Cards.luckyDice2

instance HasModifiersFor LuckyDice2Effect where
  getModifiersFor (LuckyDice2Effect (a `With` _)) =
    modified_ a a.target [IgnoreChaosToken]

instance RunMessage LuckyDice2Effect where
  runMessage msg e@(LuckyDice2Effect (attrs `With` (Metadata hasDrawn))) = runQueueT $ case msg of
    Msg.RevealChaosToken _ _ token -> do
      when (not hasDrawn && token.face == AutoFail) do
        for_ attrs.source.asset removeFromGame
      pure $ LuckyDice2Effect $ attrs `with` Metadata True
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> LuckyDice2Effect . (`with` Metadata hasDrawn) <$> liftRunMessage msg attrs
