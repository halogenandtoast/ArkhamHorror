module Arkham.Asset.Cards.LuckyDice3 (luckyDice3, luckyDice3Effect, LuckyDice3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (IgnoreChaosToken)
import Arkham.ChaosToken
import Arkham.Effect.Runner hiding (RevealChaosToken)
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (IgnoreChaosToken)
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (mkAfter)
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
  runMessage msg a@(LuckyDice3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (Window.revealedChaosTokens -> [token]) _ -> do
      ignoreWindow <-
        checkWindows [mkAfter (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
      pushAll
        [ createCardEffect Cards.luckyDice3 Nothing (attrs.ability 1) token
        , DrawAnotherChaosToken iid
        , ignoreWindow
        ]
      pure a
    _ -> LuckyDice3 <$> runMessage msg attrs

newtype LuckyDice3Effect = LuckyDice3Effect (EffectAttrs `With` Metadata)
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newtype Metadata = Metadata {alreadyTriggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

luckyDice3Effect :: EffectArgs -> LuckyDice3Effect
luckyDice3Effect = cardEffect (LuckyDice3Effect . (`with` Metadata False)) Cards.luckyDice3

instance HasModifiersFor LuckyDice3Effect where
  getModifiersFor target (LuckyDice3Effect (attrs `With` _)) | target == effectTarget attrs = do
    pure $ toModifiers attrs [IgnoreChaosToken]
  getModifiersFor _ _ = pure []

instance RunMessage LuckyDice3Effect where
  runMessage msg e@(LuckyDice3Effect (attrs@EffectAttrs {..} `With` (Metadata hasDrawn))) =
    case msg of
      Msg.RevealChaosToken _ _ token -> do
        when (not hasDrawn && token.face `elem` [AutoFail, #curse]) $ do
          case effectSource of
            AssetSource aid -> do
              mOwner <- field AssetOwner aid
              case mOwner of
                Nothing -> error "no owner"
                Just iid -> push (ReturnToHand iid $ AssetTarget aid)
            _ -> error "wrong source"
        pure $ LuckyDice3Effect $ attrs `with` Metadata True
      SkillTestEnds _ _ -> e <$ push (DisableEffect effectId)
      _ -> LuckyDice3Effect . (`with` Metadata hasDrawn) <$> runMessage msg attrs
