module Arkham.Types.Asset.Cards.RiteOfSeeking
  ( riteOfSeeking
  , RiteOfSeeking(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Target
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Uses

newtype RiteOfSeeking = RiteOfSeeking AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: AssetId -> RiteOfSeeking
riteOfSeeking uuid =
  RiteOfSeeking $ (baseAttrs uuid "02028") { assetSlots = [ArcaneSlot] }

instance ActionRunner env => HasActions env RiteOfSeeking where
  getActions iid window (RiteOfSeeking a) | ownedBy a iid = do
    investigateAvailable <- hasInvestigateActions iid window
    pure
      [ ActivateCardAbilityAction
          iid
          (mkAbility
            (toSource a)
            1
            (ActionAbility
              (Just Action.Investigate)
              (Costs [ActionCost 1, UseCost (toId a) Charge 1])
            )
          )
      | useCount (assetUses a) > 0 && investigateAvailable
      ]
  getActions _ _ _ = pure []

instance HasModifiersFor env RiteOfSeeking where
  getModifiersFor = noModifiersFor

instance (HasQueue env, HasModifiersFor env (), HasId LocationId env InvestigatorId) => RunMessage env RiteOfSeeking where
  runMessage msg a@(RiteOfSeeking attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      RiteOfSeeking <$> runMessage msg (attrs & usesL .~ Uses Charge 3)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      a <$ unshiftMessages
        [ CreateEffect "02028" Nothing source (InvestigationTarget iid lid)
        , Investigate iid lid source SkillWillpower False
        ]
    _ -> RiteOfSeeking <$> runMessage msg attrs
