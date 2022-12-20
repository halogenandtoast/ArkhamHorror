module Arkham.Asset.Cards.StHubertsKey
  ( stHubertsKey
  , StHubertsKey(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( InvestigatorDefeated )
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype StHubertsKey = StHubertsKey AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stHubertsKey :: AssetCard StHubertsKey
stHubertsKey = asset StHubertsKey Cards.stHubertsKey

instance HasModifiersFor StHubertsKey where
  getModifiersFor (InvestigatorTarget iid) (StHubertsKey a)
    | controlledBy a iid = pure $ toModifiers
      a
      [ SkillModifier SkillWillpower 1
      , SkillModifier SkillIntellect 1
      , SanityModifier (-2)
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities StHubertsKey where
  getAbilities (StHubertsKey a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
            (InvestigatorDefeated Timing.When AnySource ByHorror You)
        $ DiscardCost
        $ toTarget a
    ]

instance RunMessage StHubertsKey where
  runMessage msg a@(StHubertsKey attrs) = case msg of
    InDiscard _ (UseCardAbility iid source 1 _ _) | isSource attrs source -> do
      mDefeatedMessage <- findFromQueue \case
        Msg.InvestigatorDefeated{} -> True
        _ -> False
      let
        defeatedSource = case mDefeatedMessage of
          Just (Msg.InvestigatorDefeated x _) -> x
          _ -> error "missing defeated message"
      pushAll
        [ HealHorror (InvestigatorTarget iid) (toSource attrs) 2
        , CancelNext InvestigatorDefeatedMessage
        , CheckDefeated defeatedSource
        ]
      pure a
    _ -> StHubertsKey <$> runMessage msg attrs
