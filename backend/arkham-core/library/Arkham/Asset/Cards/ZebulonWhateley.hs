module Arkham.Asset.Cards.ZebulonWhateley
  ( zebulonWhateley
  , ZebulonWhateley(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ZebulonWhateley = ZebulonWhateley AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zebulonWhateley :: AssetCard ZebulonWhateley
zebulonWhateley =
  allyWith ZebulonWhateley Cards.zebulonWhateley (1, 4) (isStoryL .~ True)

instance HasAbilities ZebulonWhateley where
  getAbilities (ZebulonWhateley x) =
    [ restrictedAbility x 1 ControlsThis $ ReactionAbility
        (SkillTestResult
          Timing.After
          You
          (SkillTestOnTreachery AnyTreachery)
          (SuccessResult AnyValue)
        )
        (ExhaustCost $ toTarget x)
    ]

instance HasModifiersFor ZebulonWhateley where
  getModifiersFor (InvestigatorTarget iid) (ZebulonWhateley a) =
    pure [ toModifier a (SkillModifier SkillWillpower 1) | controlledBy a iid ]
  getModifiersFor _ _ = pure []

instance RunMessage ZebulonWhateley where
  runMessage msg a@(ZebulonWhateley attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> ZebulonWhateley <$> runMessage msg attrs
