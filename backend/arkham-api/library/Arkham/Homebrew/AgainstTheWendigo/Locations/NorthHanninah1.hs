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

northHanninah1 :: LocationCard NorthHanninah1
northHanninah1 = location NorthHanninah1 Cards.northHanninah1 4 (Static 0)

instance HasModifiersFor NorthHanninah1 where
  getModifiersFor (NorthHanninah1 a) = riverMovementBan a

instance HasAbilities NorthHanninah1 where
  getAbilities (NorthHanninah1 a) =
    extendRevealed a
      $ riverActions a
      <> [ campaignI18n
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
      -- "Forced - When you take control of Expedition Notebook: Test [willpower] (3).
      -- If you fail, take 1 direct horror."
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 3) iid #willpower (Fixed 3)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 3 -> True) -> do
      directHorror iid (attrs.ability 3) 1
      pure l
    _ -> NorthHanninah1 <$> liftRunMessage msg attrs
