module Arkham.Asset.Cards.LuckyDice2 (luckyDice2, luckyDice2Effect, LuckyDice2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (IgnoreChaosToken)
import Arkham.ChaosToken
import Arkham.Effect.Runner hiding (RevealChaosToken)
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (IgnoreChaosToken)
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Window (mkAfter)
import Arkham.Window qualified as Window

newtype LuckyDice2 = LuckyDice2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

luckyDice2 :: AssetCard LuckyDice2
luckyDice2 = asset LuckyDice2 Cards.luckyDice2

instance HasAbilities LuckyDice2 where
  getAbilities (LuckyDice2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (RevealChaosToken #after You (ChaosTokenFaceIsNot AutoFail))
          (ResourceCost 2)
    ]

instance RunMessage LuckyDice2 where
  runMessage msg a@(LuckyDice2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      ignoreWindow <-
        checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll
        [ createCardEffect Cards.luckyDice2 Nothing (attrs.ability 1) (ChaosTokenTarget token)
        , DrawAnotherChaosToken iid
        , ignoreWindow
        ]
      pure a
    _ -> LuckyDice2 <$> runMessage msg attrs

newtype LuckyDice2Effect = LuckyDice2Effect (EffectAttrs `With` Metadata)
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newtype Metadata = Metadata {alreadyTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

luckyDice2Effect :: EffectArgs -> LuckyDice2Effect
luckyDice2Effect = cardEffect (LuckyDice2Effect . (`with` Metadata False)) Cards.luckyDice2

instance HasModifiersFor LuckyDice2Effect where
  getModifiersFor target (LuckyDice2Effect (attrs `With` _)) | target == effectTarget attrs = do
    pure $ toModifiers attrs [IgnoreChaosToken]
  getModifiersFor _ _ = pure []

instance RunMessage LuckyDice2Effect where
  runMessage msg e@(LuckyDice2Effect (attrs@EffectAttrs {..} `With` (Metadata hasDrawn))) =
    case msg of
      Msg.RevealChaosToken _ _ token -> do
        when (not hasDrawn && chaosTokenFace token == AutoFail) $ do
          case effectSource of
            AssetSource aid -> push (RemoveFromGame $ AssetTarget aid)
            _ -> error "wrong source"
        pure $ LuckyDice2Effect $ attrs `with` Metadata True
      SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
      _ -> LuckyDice2Effect . (`with` Metadata hasDrawn) <$> runMessage msg attrs
