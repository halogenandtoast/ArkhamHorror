module Arkham.Asset.Cards.HelplessPassenger
  ( helplessPassenger
  , HelplessPassenger(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action
import Arkham.Asset.Attrs
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.Id
import Arkham.Matcher
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype HelplessPassenger = HelplessPassenger AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helplessPassenger :: AssetCard HelplessPassenger
helplessPassenger = allyWith
  HelplessPassenger
  Cards.helplessPassenger
  (1, 1)
  ((isStoryL .~ True) . (slotsL .~ mempty))

instance HasAbilities HelplessPassenger where
  getAbilities (HelplessPassenger x) =
    [ restrictedAbility
      x
      1
      (Unowned <> OnSameLocation)
      (ActionAbility (Just Parley) $ ActionCost 1)
    , mkAbility x 2
      $ ForcedAbility
      $ AssetLeavesPlay Timing.When
      $ AssetWithId
      $ toId x
    ]

instance AssetRunner env => RunMessage env HelplessPassenger where
  runMessage msg a@(HelplessPassenger attrs@AssetAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <- getId @LocationId iid
      spawnAt <- fromMaybe lid <$> getId (LeftOf, lid)
      a <$ push (AttachAsset assetId (LocationTarget spawnAt))
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeControlOfAsset iid assetId)
    UseCardAbility _ source _ 2 _ | isSource attrs source -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ pushAll
        [ InvestigatorAssignDamage iid' source DamageAny 0 1
        | iid' <- investigatorIds
        ]
    _ -> HelplessPassenger <$> runMessage msg attrs
