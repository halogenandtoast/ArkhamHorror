module Arkham.Types.Asset.Cards.TheNecronomicon
  ( TheNecronomicon(..)
  , theNecronomicon
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction hiding (PlayCard)
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token

newtype TheNecronomicon = TheNecronomicon AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

theNecronomicon :: AssetCard TheNecronomicon
theNecronomicon =
  handWith TheNecronomicon Cards.theNecronomicon
    $ (horrorL ?~ 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor env TheNecronomicon where
  getModifiersFor (SkillTestSource iid _ _ _ _) (TokenTarget t) (TheNecronomicon a)
    | Token.tokenFace t == Token.ElderSign
    = pure
      [ toModifier a (ForcedTokenChange Token.ElderSign [Token.AutoFail])
      | ownedBy a iid
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions TheNecronomicon where
  getActions (TheNecronomicon a) =
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
