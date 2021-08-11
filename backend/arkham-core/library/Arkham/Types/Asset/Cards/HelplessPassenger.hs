module Arkham.Types.Asset.Cards.HelplessPassenger
  ( helplessPassenger
  , HelplessPassenger(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Restriction
import Arkham.Types.Target

newtype HelplessPassenger = HelplessPassenger AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helplessPassenger :: AssetCard HelplessPassenger
helplessPassenger =
  allyWith HelplessPassenger Cards.helplessPassenger (1, 1) (isStoryL .~ True)

instance HasActions HelplessPassenger where
  getActions (HelplessPassenger attrs) =
    [ restrictedAbility
        attrs
        1
        (Unowned <> OnSameLocation)
        (ActionAbility (Just Parley) $ ActionCost 1)
    ]

instance HasModifiersFor env HelplessPassenger

instance
  ( HasQueue env
  , HasModifiersFor env ()
  , HasId LocationId env InvestigatorId
  , HasSet InScenarioInvestigatorId env ()
  , HasId (Maybe LocationId) env (Direction, LocationId)
  )
  => RunMessage env HelplessPassenger where
  runMessage msg a@(HelplessPassenger attrs@AssetAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      spawnAt <- fromMaybe lid <$> getId (LeftOf, lid)
      a <$ push (AttachAsset assetId (LocationTarget spawnAt))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeControlOfAsset iid assetId)
    When (Discard target) | isTarget attrs target -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ pushAll
        [ InvestigatorAssignDamage iid' (toSource attrs) DamageAny 0 1
        | iid' <- investigatorIds
        ]
    _ -> HelplessPassenger <$> runMessage msg attrs
