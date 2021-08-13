module Arkham.Types.Asset.Cards.TheNecronomiconAdvanced
  ( TheNecronomiconAdvanced(..)
  , theNecronomiconAdvanced
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
import Arkham.Types.Target
import qualified Arkham.Types.Token as Token

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, Generic, ToJSON, FromJSON, Entity)

theNecronomiconAdvanced :: AssetCard TheNecronomiconAdvanced
theNecronomiconAdvanced =
  handWith TheNecronomiconAdvanced Cards.theNecronomiconAdvanced
    $ (horrorL ?~ 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor env TheNecronomiconAdvanced where
  getModifiersFor _ (InvestigatorTarget iid) (TheNecronomiconAdvanced a) =
    pure $ toModifiers
      a
      [ ForcedTokenChange
          Token.ElderSign
          [Token.Cultist, Token.Tablet, Token.ElderThing]
      | ownedBy a iid
      ]
  getModifiersFor _ _ _ = pure []

instance HasActions TheNecronomiconAdvanced where
  getActions (TheNecronomiconAdvanced a) =
    [ restrictedAbility a 1 (OwnsThis <> AnyHorrorOnThis)
        $ ActionAbility Nothing
        $ ActionCost 1
    ]

instance (AssetRunner env) => RunMessage env TheNecronomiconAdvanced where
  runMessage msg a@(TheNecronomiconAdvanced attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (PlayCard iid (toCardId attrs) Nothing False)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ InvestigatorDamage iid source 0 1
      if fromJustNote "Must be set" (assetHorror attrs) == 1
        then a <$ push (Discard (toTarget attrs))
        else pure $ TheNecronomiconAdvanced
          (attrs { assetHorror = max 0 . subtract 1 <$> assetHorror attrs })
    _ -> TheNecronomiconAdvanced <$> runMessage msg attrs
