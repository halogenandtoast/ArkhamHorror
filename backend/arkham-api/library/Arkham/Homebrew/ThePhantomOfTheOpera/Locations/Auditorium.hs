module Arkham.Homebrew.ThePhantomOfTheOpera.Locations.Auditorium (auditorium) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Matcher
import Arkham.Campaigns.TheScarletKeys.Helpers (placeConcealedCard)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Import
import Arkham.I18n
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype Auditorium = Auditorium LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

auditorium :: LocationCard Auditorium
auditorium = symbolLabel $ location Auditorium Cards.auditorium 2 (PerPlayer 2)

instance HasAbilities Auditorium where
  getAbilities (Auditorium a) =
    extendRevealed
      a
      [ scenarioI18n
          $ withI18nTooltip "auditorium.drawIn"
          $ groupLimit PerRound
          $ restricted a 1 (Here <> exists (ConcealedCardAt (RevealedLocation <> not_ (be a))))
          $ FastAbility Free
      , scenarioI18n
          $ withI18nTooltip "auditorium.panic"
          $ limitedAbility (MaxPer Cards.auditorium PerGame 1)
          $ restricted a 2 (exists (investigatorAt a.id))
          $ forced
          $ AgendaAdvances #when AnyAgenda
      ]

instance RunMessage Auditorium where
  runMessage msg l@(Auditorium attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealed <- getConcealedChoicesAt NotForExpose (RevealedLocation <> not_ (be attrs))
      chooseTargetM iid concealed \c -> placeConcealedCard iid c.id (AtLocation attrs.id)
      pure l
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      selectEach (investigatorAt attrs.id) (`forInvestigator` msg)
      pure l
    ForInvestigator iid (UseThisAbility _ (isSource attrs -> True) 2) -> do
      chooseOneM iid $ withI18n do
        countVar 3 $ labeled "takeDamage" $ assignDamage iid (attrs.ability 2) 3
        countVar 2 $ labeled "loseActions" $ loseActions iid (attrs.ability 2) 2
      pure l
    _ -> Auditorium <$> liftRunMessage msg attrs
