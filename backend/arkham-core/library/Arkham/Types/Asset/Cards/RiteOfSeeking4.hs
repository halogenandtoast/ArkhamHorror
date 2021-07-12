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
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype RiteOfSeeking4 = RiteOfSeeking4 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking4 :: AssetCard RiteOfSeeking4
riteOfSeeking4 = arcaneWith
  RiteOfSeeking4
  Cards.riteOfSeeking4
  (startingUsesL ?~ Uses Charge 3)

instance HasActions env RiteOfSeeking4 where
  getActions iid _ (RiteOfSeeking4 a) | ownedBy a iid = pure
    [ UseAbility
        iid
        (mkAbility
          (toSource a)
          1
          (ActionAbility
            (Just Action.Investigate)
            (Costs [ActionCost 1, UseCost (toId a) Charge 1])
          )
        )
    ]
  getActions _ _ _ = pure []

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
        [ CreateEffect "02028" Nothing source (InvestigationTarget iid lid) -- same effect as base
        , skillTestModifier
          source
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 2)
        , Investigate iid lid source SkillWillpower False
        ]
    _ -> RiteOfSeeking4 <$> runMessage msg attrs
