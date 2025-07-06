module Arkham.Location.Cards.HotelRoof (hotelRoof) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Name
import Arkham.Projection

newtype HotelRoof = HotelRoof LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hotelRoof :: LocationCard HotelRoof
hotelRoof = location HotelRoof Cards.hotelRoof 3 (PerPlayer 1)

instance HasAbilities HotelRoof where
  getAbilities (HotelRoof a) =
    extendRevealed
      a
      [ skillTestAbility
          $ withTooltip
            "{action}: Test {agility} (4) or {combat} (4). If you succeed, move to either Room 212, Room 225, or Room 245."
          $ restricted a 1 Here actionAbility
      , skillTestAbility
          $ withTooltip
            "{action}: Test {willpower} (3). If you succeed, move any number of clues controlled by investigators at this location to Alien Device (if it is in play)."
          $ restricted a 2 Here actionAbility
      ]

instance RunMessage HotelRoof where
  runMessage msg l@(HotelRoof attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#agility, #combat] (Fixed 4)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      rooms <- select $ mapOneOf locationIs [Locations.room212, Locations.room225, Locations.room245]
      chooseTargetM iid rooms $ moveTo (attrs.ability 1) iid
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      alienDevice <- selectOne $ assetIs Assets.alienDevice

      unless (null iids || isNothing alienDevice) $ do
        named <- traverse (\(iid', n) -> (,n) <$> field InvestigatorName iid') iids
        chooseAmounts
          iid
          "number of clues to move to Alien Device"
          (MinAmountTarget 0)
          (map (\(name, n) -> (toTitle name, (0, n))) named)
          attrs
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      let
        iidsWithAmounts =
          flip mapMaybe named $ \(iid', name) ->
            let amount = getChoiceAmount (toTitle name) choices
             in guard (amount > 0) $> (iid', amount)
      alienDevice <- selectJust $ assetIs Assets.alienDevice
      for_ iidsWithAmounts \(iid, n) -> do
        moveTokens (attrs.ability 2) iid alienDevice #clue n
      pure l
    _ -> HotelRoof <$> liftRunMessage msg attrs
