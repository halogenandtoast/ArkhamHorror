module Arkham.Homebrew.AgainstTheWendigo.Locations.NorthHanninah1 (northHanninah1) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Card (flipCard, toCard)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Placement

newtype NorthHanninah1 = NorthHanninah1 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- "This location is connected to the River locations directly to the North
or South and connected to the Wild locations directly to the East or West."
-}
northHanninah1 :: LocationCard NorthHanninah1
northHanninah1 =
  locationWith NorthHanninah1 Cards.northHanninah1 4 (Static 0) connectsToAdjacent

instance HasModifiersFor NorthHanninah1 where
  getModifiersFor (NorthHanninah1 a) = riverMovementBan a

instance HasAbilities NorthHanninah1 where
  getAbilities (NorthHanninah1 a) =
    extendRevealed a
      $ riverActions a
      <> [ campaignI18n
             $ withI18nTooltip "northHanninah.investigate"
             $ withI18nResultLabel "northHanninah.investigate"
             $ skillTestAbility
             $ restricted a 3 Here investigateAction_
         ]

instance RunMessage NorthHanninah1 where
  runMessage msg l@(NorthHanninah1 attrs) = runQueueT $ case msg of
    {- "Revelation - Attach the Expedition Notebook to this location."

    Revealing a location doesn't trigger a revelation since it is not drawn.
    However the original designer wrote it as a revelation effect, so we simulate it here.
    -}
    Msg.RevealLocation miid lid | lid == toId attrs -> do
      pid <- maybe getLeadPlayer getPlayer miid
      sendRevelation pid (toJSON $ flipCard $ toCard attrs)
      createAssetAt_ Assets.expeditionNotebook (AttachedToLocation attrs.id)
      NorthHanninah1 <$> liftRunMessage msg attrs
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      resolveWalkAlongTheRiver (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      resolveNavigate (attrs.ability 2) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      sid <- getRandom
      investigate sid iid (attrs.ability 3)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 3 -> True) _ _ -> do
      selectForMaybeM (assetIs Assets.expeditionNotebook) (takeControlOfAsset iid)
      pure l
    _ -> NorthHanninah1 <$> liftRunMessage msg attrs
