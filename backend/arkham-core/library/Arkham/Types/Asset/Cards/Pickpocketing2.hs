module Arkham.Types.Asset.Cards.Pickpocketing2
  ( Pickpocketing2(..)
  , pickpocketing2
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Matcher qualified as Matcher
import Arkham.Types.SkillTest
import Arkham.Types.SkillTestResult
import Arkham.Types.Timing qualified as Timing

newtype Pickpocketing2 = Pickpocketing2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing2 :: AssetCard Pickpocketing2
pickpocketing2 = asset Pickpocketing2 Cards.pickpocketing2

instance HasAbilities Pickpocketing2 where
  getAbilities (Pickpocketing2 a) =
    [ restrictedAbility a 1 OwnsThis $ ReactionAbility
        (Matcher.EnemyEvaded Timing.After You AnyEnemy)
        (ExhaustCost $ toTarget a)
    ]

instance AssetRunner env => RunMessage env Pickpocketing2 where
  runMessage msg a@(Pickpocketing2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      mskillTest <- getSkillTest
      a <$ case skillTestResult <$> mskillTest of
        Just (SucceededBy _ n) | n >= 2 ->
          pushAll [DrawCards iid 1 False, TakeResources iid 1 False]
        _ -> push $ chooseOne
          iid
          [ Label "Draw 1 card" [DrawCards iid 1 False]
          , Label "Gain 1 resource" [TakeResources iid 1 False]
          ]
    _ -> Pickpocketing2 <$> runMessage msg attrs
