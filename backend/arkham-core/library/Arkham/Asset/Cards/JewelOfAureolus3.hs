module Arkham.Asset.Cards.JewelOfAureolus3
  ( jewelOfAureolus3
  , JewelOfAureolus3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window qualified as Window

newtype JewelOfAureolus3 = JewelOfAureolus3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jewelOfAureolus3 :: AssetCard JewelOfAureolus3
jewelOfAureolus3 = asset JewelOfAureolus3 Cards.jewelOfAureolus3

instance HasAbilities JewelOfAureolus3 where
  getAbilities (JewelOfAureolus3 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
            (RevealChaosToken Timing.When (InvestigatorAt YourLocation)
            $ TokenMatchesAny
                (map
                  TokenFaceIs
                  [Skull, Cultist, Tablet, ElderThing, AutoFail]
                )
            )
        $ ExhaustCost (toTarget x)
    ]

instance RunMessage JewelOfAureolus3 where
  runMessage msg a@(JewelOfAureolus3 attrs) = case msg of
    UseCardAbility iid source 1 (Window.revealedTokens -> tokens) _
      | isSource attrs source -> do
        drawing <- drawCards iid attrs 1
        push $ If
          (Window.RevealTokenAssetAbilityEffect iid tokens (toId attrs))
          [ chooseOne
              iid
              [ Label "Draw 1 Card" [drawing]
              , Label
                "Take 2 Resources"
                [TakeResources iid 2 (toAbilitySource attrs 1) False]
              ]
          ]
        pure a
    _ -> JewelOfAureolus3 <$> runMessage msg attrs
