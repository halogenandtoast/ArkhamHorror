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
import Arkham.Types.Target
import Arkham.Types.Window

newtype HelplessPassenger = HelplessPassenger AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helplessPassenger :: AssetCard HelplessPassenger
helplessPassenger = allyWith HelplessPassenger Cards.helplessPassenger (1, 1) (isStoryL .~ True)

ability :: AssetAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility (Just Parley) $ ActionCost 1)

instance HasId LocationId env InvestigatorId => HasActions env HelplessPassenger where
  getActions iid NonFast (HelplessPassenger attrs) = do
    lid <- getId iid
    case assetLocation attrs of
      Just location -> pure
        [ ActivateCardAbilityAction iid (ability attrs)
        | lid == location && isNothing (assetInvestigator attrs)
        ]
      _ -> pure mempty
  getActions iid window (HelplessPassenger attrs) = getActions iid window attrs

instance HasModifiersFor env HelplessPassenger where
  getModifiersFor = noModifiersFor

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
      a <$ unshiftMessage (AttachAsset assetId (LocationTarget spawnAt))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ unshiftMessage (TakeControlOfAsset iid assetId)
    When (Discard target) | isTarget attrs target -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ unshiftMessages
        [ InvestigatorAssignDamage iid' (toSource attrs) DamageAny 0 1
        | iid' <- investigatorIds
        ]
    _ -> HelplessPassenger <$> runMessage msg attrs
