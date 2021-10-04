module Arkham.Types.Asset.Cards.StrangeSolution
  ( strangeSolution
  , StrangeSolution(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.CampaignLogKey
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype StrangeSolution = StrangeSolution AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolution :: AssetCard StrangeSolution
strangeSolution = asset StrangeSolution Cards.strangeSolution

instance HasAbilities StrangeSolution where
  getAbilities (StrangeSolution x) =
    [restrictedAbility x 1 OwnsThis $ ActionAbility Nothing $ ActionCost 1]

instance AssetRunner env => RunMessage env StrangeSolution where
  runMessage msg a@(StrangeSolution attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillIntellect
        4
      )
    PassedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ pushAll
        [ Discard (toTarget attrs)
        , DrawCards iid 2 False
        , Record YouHaveIdentifiedTheSolution
        ]
    _ -> StrangeSolution <$> runMessage msg attrs
