module Arkham.Location.Cards.HotelRoof (
  hotelRoof,
  HotelRoof (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Name
import Arkham.Projection

newtype HotelRoof = HotelRoof LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

hotelRoof :: LocationCard HotelRoof
hotelRoof = location HotelRoof Cards.hotelRoof 3 (PerPlayer 1)

instance HasAbilities HotelRoof where
  getAbilities (HotelRoof attrs) =
    withRevealedAbilities
      attrs
      [ withTooltip
          "{action}: Test {agility} (4) or {combat} (4). If you succeed, move to either Room 212, Room 225, or Room 245."
          $ restrictedAbility attrs 1 Here actionAbility
      , withTooltip
          "{action}: Test {willpower} (3). If you succeed, move any number of clues controlled by investigators at this location to Alien Device (if it is in play)."
          $ restrictedAbility attrs 2 Here actionAbility
      ]

instance RunMessage HotelRoof where
  runMessage msg l@(HotelRoof attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ SkillLabel skill [beginSkillTest iid (toAbilitySource attrs 1) iid skill 4]
          | skill <- [#agility, #combat]
          ]
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ beginSkillTest iid (toAbilitySource attrs 2) iid #willpower 3
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      rooms <-
        selectList
          $ oneOf [locationIs Locations.room212, locationIs Locations.room225, locationIs Locations.room245]
      player <- getPlayer iid
      push $ chooseOne player $ targetLabels rooms (only . MoveTo . move (toAbilitySource attrs 1) iid)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      player <- getPlayer iid
      iids <- selectWithField InvestigatorClues $ investigatorAt (toId attrs) <> InvestigatorWithAnyClues
      alienDevice <- selectOne $ assetIs Assets.alienDevice

      unless (null iids || isNothing alienDevice) $ do
        named <- traverse (\(iid', n) -> (,n) <$> field InvestigatorName iid') iids
        push
          $ chooseAmounts
            player
            "number of clues to move to Alien Device"
            (MinAmountTarget 0)
            (map (\(name, n) -> (toTitle name, (0, n))) named)
            (toTarget attrs)
      pure l
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      named <- selectWithField InvestigatorName UneliminatedInvestigator
      let
        iidsWithAmounts =
          flip mapMaybe named $ \(iid', name) ->
            let amount = getChoiceAmount (toTitle name) choices
             in guard (amount > 0) $> (iid', amount)
      alienDevice <- selectJust $ assetIs Assets.alienDevice
      pushAll
        $ [ MovedClues (toSource iid) (toTarget alienDevice) n
          | (iid, n) <- iidsWithAmounts
          ]
      pure l
    _ -> HotelRoof <$> runMessage msg attrs
