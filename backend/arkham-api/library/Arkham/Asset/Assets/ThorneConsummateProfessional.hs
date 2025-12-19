module Arkham.Asset.Assets.ThorneConsummateProfessional (thorneConsummateProfessional) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Matcher
import Arkham.Placement
import Arkham.Window qualified as Window

newtype ThorneConsummateProfessional = ThorneConsummateProfessional AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thorneConsummateProfessional :: AssetCard ThorneConsummateProfessional
thorneConsummateProfessional = allyWith ThorneConsummateProfessional Cards.thorneConsummateProfessional (2, 2) noSlots

instance HasAbilities ThorneConsummateProfessional where
  getAbilities (ThorneConsummateProfessional a) =
    [ controlled_ a 1
        $ triggered (CampaignEvent #after (Just $ affectsColocatedMatch You) "hollowed") (exhaust a)
    ]

instance RunMessage ThorneConsummateProfessional where
  runMessage msg a@(ThorneConsummateProfessional attrs) = runQueueT $ case msg of
    CardEnteredPlay _ card | card.id == attrs.cardId -> do
      keysFor attrs >>= traverse_ (`createScarletKeyAt_` AttachedToAsset attrs.id Nothing)
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      let
        go = \case
          [] -> pure ()
          ((Window.windowType -> Window.CampaignEvent "hollowed" (Just iid) _) : _) -> do
            takeActionAsIfTurn iid (attrs.ability 1)
          (_ : xs) -> go xs
      go ws
      pure a
    _ -> ThorneConsummateProfessional <$> liftRunMessage msg attrs
