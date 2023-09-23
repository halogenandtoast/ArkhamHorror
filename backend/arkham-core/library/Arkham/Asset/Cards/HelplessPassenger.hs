module Arkham.Asset.Cards.HelplessPassenger (
  helplessPassenger,
  HelplessPassenger (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Direction
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype HelplessPassenger = HelplessPassenger AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helplessPassenger :: AssetCard HelplessPassenger
helplessPassenger =
  allyWith
    HelplessPassenger
    Cards.helplessPassenger
    (1, 1)
    ((isStoryL .~ True) . (slotsL .~ mempty))

instance HasAbilities HelplessPassenger where
  getAbilities (HelplessPassenger x) =
    [ restrictedAbility
        x
        1
        (Uncontrolled <> OnSameLocation)
        (ActionAbility (Just Parley) $ ActionCost 1)
    , mkAbility x 2
        $ ForcedAbility
        $ AssetLeavesPlay Timing.When
        $ AssetWithId
        $ toId x
    ]

instance RunMessage HelplessPassenger where
  runMessage msg a@(HelplessPassenger attrs@AssetAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source -> do
      lid <-
        fieldMap
          InvestigatorLocation
          (fromJustNote "must be at a location")
          iid
      spawnAt <-
        fromMaybe lid
          <$> selectOne (LocationInDirection LeftOf (LocationWithId lid))
      a <$ push (AttachAsset assetId (LocationTarget spawnAt))
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a <$ push (TakeControlOfAsset iid assetId)
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      investigatorIds <- selectList UneliminatedInvestigator
      a
        <$ pushAll
          [ InvestigatorAssignDamage iid' source DamageAny 0 1
          | iid' <- investigatorIds
          ]
    _ -> HelplessPassenger <$> runMessage msg attrs
