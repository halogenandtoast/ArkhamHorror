module Arkham.Asset.Cards.ArchiveOfConduitsGatewayToAcheron4 (
  archiveOfConduitsGatewayToAcheron4,
  ArchiveOfConduitsGatewayToAcheron4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Query (getPlayer)
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Token qualified as Token

newtype ArchiveOfConduitsGatewayToAcheron4 = ArchiveOfConduitsGatewayToAcheron4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archiveOfConduitsGatewayToAcheron4 :: AssetCard ArchiveOfConduitsGatewayToAcheron4
archiveOfConduitsGatewayToAcheron4 = asset ArchiveOfConduitsGatewayToAcheron4 Cards.archiveOfConduitsGatewayToAcheron4

instance HasAbilities ArchiveOfConduitsGatewayToAcheron4 where
  getAbilities (ArchiveOfConduitsGatewayToAcheron4 attrs) =
    [ controlledAbility attrs 1 (exists RevealedLocation <> exists (be attrs <> AssetWithUses Leyline))
        $ FastAbility Free
    , controlledAbility
        attrs
        2
        ( exists $ LocationWithToken Token.Leyline <> CanEnterLocation (affectsOthers Anyone)
        )
        actionAbility
    ]

instance RunMessage ArchiveOfConduitsGatewayToAcheron4 where
  runMessage msg a@(ArchiveOfConduitsGatewayToAcheron4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select RevealedLocation
      chooseOne
        iid
        [ targetLabel location [MoveUses (toSource attrs) (toTarget location) Leyline 1]
        | location <- locations
        ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <-
        select
          $ affectsOthers
          $ InvestigatorCanMoveTo (attrs.ability 2) (LocationWithToken Token.Leyline)
      chooseOrRunOne
        iid
        [targetLabel iid' [HandleTargetChoice iid (attrs.ability 2) (toTarget iid')] | iid' <- iids]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid') -> do
      locations <-
        select
          $ LocationWithToken Token.Leyline
          <> CanEnterLocation (InvestigatorWithId iid')
      player <- getPlayer iid
      choices <- for locations \location -> do
        investigate <- mkInvestigateLocation iid' (attrs.ability 2) location
        pure
          $ targetLabel
            location
            [ toMessage $ move (attrs.ability 2) iid' location
            , Msg.chooseOne
                player
                [ Label "Do not remove Leyline" []
                , Label
                    "Remove Leyline"
                    [ RemoveTokens (attrs.ability 2) (toTarget location) Token.Leyline 1
                    , toMessage investigate
                    ]
                ]
            ]

      chooseOrRunOne iid choices
      pure a
    _ -> ArchiveOfConduitsGatewayToAcheron4 <$> lift (runMessage msg attrs)
