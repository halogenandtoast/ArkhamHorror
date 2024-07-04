module Arkham.Asset.Cards.ArchiveOfConduitsGatewayToAldebaran4 (
  archiveOfConduitsGatewayToAldebaran4,
  ArchiveOfConduitsGatewayToAldebaran4 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Query (getPlayer)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message qualified as Msg
import Arkham.Movement
import Arkham.Token qualified as Token

newtype ArchiveOfConduitsGatewayToAldebaran4 = ArchiveOfConduitsGatewayToAldebaran4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

archiveOfConduitsGatewayToAldebaran4 :: AssetCard ArchiveOfConduitsGatewayToAldebaran4
archiveOfConduitsGatewayToAldebaran4 = asset ArchiveOfConduitsGatewayToAldebaran4 Cards.archiveOfConduitsGatewayToAldebaran4

instance HasAbilities ArchiveOfConduitsGatewayToAldebaran4 where
  getAbilities (ArchiveOfConduitsGatewayToAldebaran4 attrs) =
    [ controlledAbility attrs 1 (exists NonEliteEnemy <> exists (be attrs <> AssetWithUses Leyline))
        $ FastAbility Free
    , doesNotProvokeAttacksOfOpportunity
        $ controlledAbility
          attrs
          2
          (exists $ EnemyIsEngagedWith (affectsOthers Anyone) <> EnemyWithToken Token.Leyline)
          actionAbility
    ]

instance RunMessage ArchiveOfConduitsGatewayToAldebaran4 where
  runMessage msg a@(ArchiveOfConduitsGatewayToAldebaran4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select NonEliteEnemy
      chooseOne
        iid
        [targetLabel enemy [MoveUses (toSource attrs) (toTarget enemy) Leyline 1] | enemy <- enemies]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      iids <-
        select
          $ affectsOthers
          $ InvestigatorEngagedWith (EnemyWithToken Token.Leyline)
      chooseOrRunOne
        iid
        [targetLabel iid' [HandleTargetChoice iid (attrs.ability 2) (toTarget iid')] | iid' <- iids]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (InvestigatorTarget iid') -> do
      enemies <- select $ EnemyWithToken Token.Leyline
      connectedLocations <-
        select $ ConnectedFrom (locationWithInvestigator iid') <> CanEnterLocation (InvestigatorWithId iid')
      player <- getPlayer iid
      choices <- for enemies \enemy -> do
        pure
          $ targetLabel enemy
          $ DisengageEnemy iid' enemy
          : [ Msg.chooseOrRunOne
              player
              [ targetLabel location [Move $ move (attrs.ability 2) iid' location] | location <- connectedLocations
              ]
            | notNull connectedLocations
            ]
            <> [ Msg.chooseOne
                  player
                  [ Label "Do not remove Leyline" []
                  , Label "Remove Leyline" [EnemyEvaded iid enemy]
                  ]
               ]

      chooseOrRunOne iid choices
      pure a
    _ -> ArchiveOfConduitsGatewayToAldebaran4 <$> liftRunMessage msg attrs
