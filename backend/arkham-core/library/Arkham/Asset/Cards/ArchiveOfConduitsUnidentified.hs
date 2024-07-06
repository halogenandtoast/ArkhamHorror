module Arkham.Asset.Cards.ArchiveOfConduitsUnidentified (
  archiveOfConduitsUnidentified,
  ArchiveOfConduitsUnidentified (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.Window (getInvestigatedLocation)
import Arkham.Matcher
import Arkham.Token

newtype ArchiveOfConduitsUnidentified = ArchiveOfConduitsUnidentified AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archiveOfConduitsUnidentified :: AssetCard ArchiveOfConduitsUnidentified
archiveOfConduitsUnidentified = asset ArchiveOfConduitsUnidentified Cards.archiveOfConduitsUnidentified

instance HasAbilities ArchiveOfConduitsUnidentified where
  getAbilities (ArchiveOfConduitsUnidentified attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility (SuccessfulInvestigation #after You $ LocationWithToken Leyline) (exhaust attrs)
    ]

instance RunMessage ArchiveOfConduitsUnidentified where
  runMessage msg a@(ArchiveOfConduitsUnidentified attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getInvestigatedLocation -> location) _ -> do
      pushAll [MoveTokens (attrs.ability 1) (toSource location) (toTarget attrs) Leyline 1, DoStep 1 msg]
      pure a
    DoStep 1 (UseCardAbility iid (isSource attrs -> True) 1 _ _) -> do
      let leylines = attrs.token Leyline
      when (leylines >= 4) do
        toDiscardBy iid (attrs.ability 1) attrs
        pushAll
          [ MoveTokens (attrs.ability 1) (toSource attrs) (ResourceTarget iid) Leyline leylines
          , Record YouHaveIdentifiedTheGateway
          ]
      pure a
    _ -> ArchiveOfConduitsUnidentified <$> liftRunMessage msg attrs
