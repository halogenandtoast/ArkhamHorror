module Arkham.Location.Cards.HemlockChapelDay (hemlockChapelDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (campaignI18n, codex)
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype HemlockChapelDay = HemlockChapelDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hemlockChapelDay :: LocationCard HemlockChapelDay
hemlockChapelDay = symbolLabel $ location HemlockChapelDay Cards.hemlockChapelDay 3 (Static 0)

instance HasAbilities HemlockChapelDay where
  getAbilities (HemlockChapelDay a) =
    campaignI18n
      $ extendRevealed
        a
        [ groupLimit PerGame $ withI18nTooltip "hemlockChapel.day.codex" $ restricted a 1 Here actionAbility
        , playerLimit PerGame
            $ withI18nTooltip "hemlockChapel.day.search"
            $ restricted a 2 (Here <> youExist can.search.deck) actionAbility
        ]

instance RunMessage HemlockChapelDay where
  runMessage msg l@(HemlockChapelDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 11
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      search iid (attrs.ability 2) iid [fromTopOfDeck 9] (basic #spell) (DrawFound iid 1)
      pure l
    _ -> HemlockChapelDay <$> liftRunMessage msg attrs
