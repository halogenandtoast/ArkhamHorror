module Arkham.Asset.Assets.PrismaticShardAlienMeteorite (prismaticShardAlienMeteorite) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EncounterCardSource)
import Arkham.Classes.HasQueue
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Matcher
import Arkham.Token
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype PrismaticShardAlienMeteorite = PrismaticShardAlienMeteorite AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

prismaticShardAlienMeteorite :: AssetCard PrismaticShardAlienMeteorite
prismaticShardAlienMeteorite = asset PrismaticShardAlienMeteorite Cards.prismaticShardAlienMeteorite

instance HasAbilities PrismaticShardAlienMeteorite where
  getAbilities (PrismaticShardAlienMeteorite a) =
    [ controlled_ a 1
        $ triggered
          (WouldDiscardFromHand #when You EncounterCardSource)
          (exhaust a <> assetUseCost a Brilliance 1)
    ]

instance RunMessage PrismaticShardAlienMeteorite where
  runMessage msg a@(PrismaticShardAlienMeteorite attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> do
        lift $ case windowType w of
          Window.WouldDiscardFromHand iid' source | iid == iid' -> do
            let drawing = Helpers.drawCards iid source
            replaceMessageMatching
              \case
                Do (DiscardFromHand handDiscard) -> handDiscard.investigator == iid && handDiscard.source == toSource source
                _ -> False
              \case
                Do (DiscardFromHand handDiscard) -> [drawing handDiscard.amount]
                _ -> []
          _ -> pure ()
      pure a
    _ -> PrismaticShardAlienMeteorite <$> liftRunMessage msg attrs
