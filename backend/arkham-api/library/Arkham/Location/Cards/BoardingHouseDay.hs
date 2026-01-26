module Arkham.Location.Cards.BoardingHouseDay (boardingHouseDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (campaignI18n, codex)
import Arkham.Helpers.Healing
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype BoardingHouseDay = BoardingHouseDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

boardingHouseDay :: LocationCard BoardingHouseDay
boardingHouseDay = symbolLabel $ location BoardingHouseDay Cards.boardingHouseDay 0 (Static 0)

instance HasAbilities BoardingHouseDay where
  getAbilities (BoardingHouseDay a) =
    campaignI18n
      $ extendRevealed
        a
        [ groupLimit PerGame $ withI18nTooltip "boardingHouse.day.codex" $ restricted a 1 Here actionAbility
        , playerLimit PerGame
            $ withI18nTooltip "boardingHouse.day.heal"
            $ withCriteria
              (mkAbility a 2 actionAbility)
              (Here <> any_ [HealableInvestigator (toSource a) kind You | kind <- [#horror, #damage]])
        ]

instance RunMessage BoardingHouseDay where
  runMessage msg l@(BoardingHouseDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 9
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      chooseHealDamageOrHorror (attrs.ability 1) iid
      pure l
    _ -> BoardingHouseDay <$> liftRunMessage msg attrs
