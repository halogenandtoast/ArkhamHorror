module Arkham.Asset.Cards.TheNecronomicon
  ( TheNecronomicon(..)
  , theNecronomicon
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Source
import Arkham.Token qualified as Token
import Arkham.Window ( defaultWindows )

newtype TheNecronomicon = TheNecronomicon AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomicon :: AssetCard TheNecronomicon
theNecronomicon =
  assetWith TheNecronomicon Cards.theNecronomicon
    $ (horrorL .~ 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomicon where
  getModifiersFor (TokenTarget t) (TheNecronomicon a)
    | Token.tokenFace t == Token.ElderSign = do
      mSkillTestSource <- getSkillTestSource
      case mSkillTestSource of
        Just (SkillTestSource iid _ _ _) -> pure
          [ toModifier a (ForcedTokenChange Token.ElderSign [Token.AutoFail])
          | controlledBy a iid
          ]
        _ -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities TheNecronomicon where
  getAbilities (TheNecronomicon a) =
    [ restrictedAbility a 1 (ControlsThis <> AnyHorrorOnThis)
        $ ActionAbility Nothing
        $ ActionCost 1
    ]

instance RunMessage TheNecronomicon where
  runMessage msg a@(TheNecronomicon attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid))
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorDamage iid source 0 1
      if assetHorror attrs <= 1
        then a <$ push (Discard (toAbilitySource attrs 1) (toTarget attrs))
        else
          pure $ TheNecronomicon
            (attrs { assetHorror = max 0 (assetHorror attrs - 1) })
    _ -> TheNecronomicon <$> runMessage msg attrs
