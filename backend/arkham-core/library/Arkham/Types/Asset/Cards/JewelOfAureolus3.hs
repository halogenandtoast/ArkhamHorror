module Arkham.Types.Asset.Cards.JewelOfAureolus3
  ( jewelOfAureolus3
  , JewelOfAureolus3(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Token

newtype JewelOfAureolus3 = JewelOfAureolus3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jewelOfAureolus3 :: AssetCard JewelOfAureolus3
jewelOfAureolus3 = asset JewelOfAureolus3 Cards.jewelOfAureolus3

instance HasAbilities JewelOfAureolus3 where
  getAbilities (JewelOfAureolus3 x) =
    [ restrictedAbility
        x
        1
        OwnsThis
        (ReactionAbility
            (RevealChaosToken Timing.When (InvestigatorAt YourLocation)
            $ TokenMatchesAny
                (map
                  TokenFaceIs
                  [Skull, Cultist, Tablet, ElderThing, AutoFail]
                )
            )
        $ ExhaustCost (toTarget x)
        )
    ]

instance AssetRunner env => RunMessage env JewelOfAureolus3 where
  runMessage msg a@(JewelOfAureolus3 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ Label "Draw 1 Card" [DrawCards iid 1 False]
        , Label "Take 2 Resources" [TakeResources iid 2 False]
        ]
      )
    _ -> JewelOfAureolus3 <$> runMessage msg attrs
