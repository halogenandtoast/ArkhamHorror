module Arkham.Asset.Assets.LuckyDice3 (luckyDice3, luckyDice3Effect, LuckyDice3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (IgnoreChaosToken)
import Arkham.Message qualified as Msg
import Arkham.Projection
import Arkham.Window qualified as Window

newtype LuckyDice3 = LuckyDice3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyDice3 :: AssetCard LuckyDice3
luckyDice3 = asset LuckyDice3 Cards.luckyDice3

instance HasAbilities LuckyDice3 where
  getAbilities (LuckyDice3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (RevealChaosToken #after You $ not_ $ oneOf [#autofail, #curse])
          (AddCurseTokenCost 1)
    ]

instance RunMessage LuckyDice3 where
  runMessage msg a@(LuckyDice3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      createCardEffect Cards.luckyDice3 Nothing (attrs.ability 1) token
      drawAnotherChaosToken iid
      cancelledOrIgnoredCardOrGameEffect attrs
      pure a
    _ -> LuckyDice3 <$> liftRunMessage msg attrs

newtype LuckyDice3Effect = LuckyDice3Effect (EffectAttrs `With` Metadata)
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newtype Metadata = Metadata {alreadyTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

luckyDice3Effect :: EffectArgs -> LuckyDice3Effect
luckyDice3Effect = cardEffect (LuckyDice3Effect . (`with` Metadata False)) Cards.luckyDice3

instance HasModifiersFor LuckyDice3Effect where
  getModifiersFor (LuckyDice3Effect (a `With` _)) =
    modified_ a a.target [IgnoreChaosToken]

instance RunMessage LuckyDice3Effect where
  runMessage msg e@(LuckyDice3Effect (attrs `With` (Metadata hasDrawn))) = runQueueT $ case msg of
    Msg.RevealChaosToken _ _ token -> do
      when (not hasDrawn && token.face `elem` [AutoFail, #curse]) $ do
        for_ attrs.source.asset \aid -> do
          iid <- fieldJust AssetOwner aid
          returnToHand iid aid
      pure $ LuckyDice3Effect $ attrs `with` Metadata True
    SkillTestEnds _ _ _ -> disableReturn e
    _ -> LuckyDice3Effect . (`with` Metadata hasDrawn) <$> liftRunMessage msg attrs
