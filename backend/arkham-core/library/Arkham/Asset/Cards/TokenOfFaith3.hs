module Arkham.Asset.Cards.TokenOfFaith3 (tokenOfFaith3, TokenOfFaith3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (SkillTestEnded)
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.SkillTest.Base
import Arkham.SkillTestResult
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TokenOfFaith3 = TokenOfFaith3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tokenOfFaith3 :: AssetCard TokenOfFaith3
tokenOfFaith3 = asset TokenOfFaith3 Cards.tokenOfFaith3

instance HasAbilities TokenOfFaith3 where
  getAbilities (TokenOfFaith3 x) =
    [ restrictedAbility x 1 (ControlsThis <> HasRemainingBlessTokens)
        $ ReactionAbility
          ( SkillTestEnded #after Anyone
              $ SkillTestWithRevealedChaosToken
              $ IncludeTokenPool
              $ oneOf [#curse, #autofail]
          )
          (exhaust x)
    ]

getTest :: [Window] -> SkillTest
getTest [] = error "No test found"
getTest ((windowType -> Window.SkillTestEnded st) : _) = st
getTest (_ : rest) = getTest rest

instance RunMessage TokenOfFaith3 where
  runMessage msg a@(TokenOfFaith3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getTest -> st) _ -> do
      let tokens = filter ((`elem` [#autofail, #curse]) . (.face)) $ skillTestRevealedChaosTokens st
      n <- min (length tokens) <$> getRemainingBlessTokens
      pushAll $ replicate n $ AddChaosToken #bless
      case skillTestResult st of
        FailedBy {} -> do
          sid <- getRandom
          chooseOne
            iid
            [ Label "Attempt that skill test again" [RepeatSkillTest sid st]
            , Label "Do not attempt again" []
            ]
        _ -> pure ()

      pure a
    _ -> TokenOfFaith3 <$> liftRunMessage msg attrs
