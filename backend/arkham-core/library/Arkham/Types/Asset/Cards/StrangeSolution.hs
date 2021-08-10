module Arkham.Types.Asset.Cards.StrangeSolution
  ( strangeSolution
  , StrangeSolution(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.CampaignLogKey
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype StrangeSolution = StrangeSolution AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolution :: AssetCard StrangeSolution
strangeSolution = asset StrangeSolution Cards.strangeSolution

instance HasActions StrangeSolution where
  getActions (StrangeSolution attrs) =
    [restrictedAbility attrs 1 OwnsThis (ActionAbility Nothing $ ActionCost 1)]

instance HasModifiersFor env StrangeSolution

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env StrangeSolution where
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
