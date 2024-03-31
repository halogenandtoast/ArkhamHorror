module Arkham.Asset.Cards.TokenOfFaith (tokenOfFaith, TokenOfFaith (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (SkillTestEnded)
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Prelude
import Arkham.SkillTest.Base
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TokenOfFaith = TokenOfFaith AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tokenOfFaith :: AssetCard TokenOfFaith
tokenOfFaith = asset TokenOfFaith Cards.tokenOfFaith

instance HasAbilities TokenOfFaith where
  getAbilities (TokenOfFaith x) =
    [ restrictedAbility x 1 (ControlsThis <> HasRemainingBlessTokens)
        $ ReactionAbility
          (SkillTestEnded #after Anyone $ SkillTestWithRevealedChaosToken $ oneOf [#curse, #autofail])
          (exhaust x)
    ]

getTokens :: [Window] -> [ChaosToken]
getTokens = concatMapMaybe \case
  (windowType -> Window.SkillTestEnded st) -> Just $ skillTestRevealedChaosTokens st
  _ -> Nothing

instance RunMessage TokenOfFaith where
  runMessage msg a@(TokenOfFaith attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getTokens -> tokens) _ -> do
      n <- min (length tokens) <$> getRemainingBlessTokens
      pushAll $ replicate n $ AddChaosToken #bless
      pure a
    _ -> TokenOfFaith <$> runMessage msg attrs
