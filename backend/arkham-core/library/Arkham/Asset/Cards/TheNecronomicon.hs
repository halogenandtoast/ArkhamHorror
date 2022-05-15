module Arkham.Asset.Cards.TheNecronomicon
  ( TheNecronomicon(..)
  , theNecronomicon
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Token qualified as Token

newtype TheNecronomicon = TheNecronomicon AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

theNecronomicon :: AssetCard TheNecronomicon
theNecronomicon =
  assetWith TheNecronomicon Cards.theNecronomicon
    $ (horrorL ?~ 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor env TheNecronomicon where
  getModifiersFor (SkillTestSource iid _ _ _) (TokenTarget t) (TheNecronomicon a)
    | Token.tokenFace t == Token.ElderSign
    = pure
      [ toModifier a (ForcedTokenChange Token.ElderSign [Token.AutoFail])
      | ownedBy a iid
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TheNecronomicon where
  getAbilities (TheNecronomicon a) =
    [ restrictedAbility a 1 (OwnsThis <> AnyHorrorOnThis)
        $ ActionAbility Nothing
        $ ActionCost 1
    ]

instance (AssetRunner env) => RunMessage env TheNecronomicon where
  runMessage msg a@(TheNecronomicon attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (PlayCard iid (toCardId attrs) Nothing False)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ InvestigatorDamage iid source 0 1
      if fromJustNote "Must be set" (assetHorror attrs) == 1
        then a <$ push (Discard (toTarget attrs))
        else pure $ TheNecronomicon
          (attrs { assetHorror = max 0 . subtract 1 <$> assetHorror attrs })
    _ -> TheNecronomicon <$> runMessage msg attrs
