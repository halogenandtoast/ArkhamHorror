module Arkham.Asset.Cards.TheNecronomiconAdvanced
  ( TheNecronomiconAdvanced(..)
  , theNecronomiconAdvanced
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Cost
import Arkham.Criteria
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Window (defaultWindows)

newtype TheNecronomiconAdvanced = TheNecronomiconAdvanced AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconAdvanced :: AssetCard TheNecronomiconAdvanced
theNecronomiconAdvanced =
  assetWith TheNecronomiconAdvanced Cards.theNecronomiconAdvanced
    $ (horrorL .~ 3)
    . (canLeavePlayByNormalMeansL .~ False)

instance HasModifiersFor TheNecronomiconAdvanced where
  getModifiersFor (InvestigatorTarget iid) (TheNecronomiconAdvanced a) =
    pure $ toModifiers
      a
      [ ForcedTokenChange
          Token.ElderSign
          [Token.Cultist, Token.Tablet, Token.ElderThing]
      | controlledBy a iid
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities TheNecronomiconAdvanced where
  getAbilities (TheNecronomiconAdvanced a) =
    [ restrictedAbility a 1 (ControlsThis <> AnyHorrorOnThis)
        $ ActionAbility Nothing
        $ ActionCost 1
    ]

instance RunMessage TheNecronomiconAdvanced where
  runMessage msg a@(TheNecronomiconAdvanced attrs) = case msg of
    Revelation iid source | isSource attrs source ->
      a <$ push (PlayCard iid (toCard attrs) Nothing (defaultWindows iid) False)
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorDamage iid source 0 1
      if assetHorror attrs <= 1
        then a <$ push (Discard (toTarget attrs))
        else pure $ TheNecronomiconAdvanced
          (attrs { assetHorror = max 0 (assetHorror attrs -1 )})
    _ -> TheNecronomiconAdvanced <$> runMessage msg attrs
