module Arkham.Types.Asset.Cards.RiteOfSeeking4
  ( riteOfSeeking4
  , RiteOfSeeking4(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype RiteOfSeeking4 = RiteOfSeeking4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking4 :: AssetCard RiteOfSeeking4
riteOfSeeking4 = arcane RiteOfSeeking4 Cards.riteOfSeeking4

instance HasActions RiteOfSeeking4 where
  getActions (RiteOfSeeking4 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Investigate)
        (Costs [ActionCost 1, UseCost (toId a) Charge 1])
    ]

instance HasModifiersFor env RiteOfSeeking4

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasId LocationId env InvestigatorId
  )
  => RunMessage env RiteOfSeeking4 where
  runMessage msg a@(RiteOfSeeking4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      a <$ pushAll
        [ CreateEffect "02233" Nothing source (InvestigationTarget iid lid) -- same effect as base
        , skillTestModifier
          source
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 2)
        , Investigate iid lid source SkillWillpower False
        ]
    _ -> RiteOfSeeking4 <$> runMessage msg attrs
