module Arkham.Location.Cards.ValeSchoolhouseDay (valeSchoolhouseDay) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers (campaignI18n, codex)
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype ValeSchoolhouseDay = ValeSchoolhouseDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

valeSchoolhouseDay :: LocationCard ValeSchoolhouseDay
valeSchoolhouseDay = symbolLabel $ location ValeSchoolhouseDay Cards.valeSchoolhouseDay 3 (Static 0)

instance HasAbilities ValeSchoolhouseDay where
  getAbilities (ValeSchoolhouseDay a) =
    campaignI18n
      $ extendRevealed
        a
        [ groupLimit PerGame $ withI18nTooltip "valeSchoolhouse.day.codex" $ restricted a 1 Here actionAbility
        , playerLimit PerGame
            $ withI18nTooltip "valeSchoolhouse.day.search"
            $ restricted a 2 (Here <> youExist can.search.deck) actionAbility
        ]

instance RunMessage ValeSchoolhouseDay where
  runMessage msg l@(ValeSchoolhouseDay attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid 15
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      search iid (attrs.ability 2) iid [fromDeck] (basic #tome) (DrawFound iid 1)
      pure l
    _ -> ValeSchoolhouseDay <$> liftRunMessage msg attrs
